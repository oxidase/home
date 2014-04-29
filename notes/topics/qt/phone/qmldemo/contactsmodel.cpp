// -*- compile-command: "make" -*-
#include <QDebug>
#include <QImage>

#include <ofonovoicecall.h>

#include <QtVersit/qversitreader.h>
#include <QtVersit/qversitwriter.h>

#include "client_interface.h"
#include "phonebookaccess_interface.h"

#include "contactsmodel.h"

QTVERSIT_USE_NAMESPACE

AvatarProvider::AvatarProvider() :
    QQuickImageProvider(Image)
{
    m_avatars.insert(QStringLiteral("undefined"), QImage(QStringLiteral("./assets/icon-user.png")));
    m_avatars.insert(QStringLiteral("user"), QImage(QStringLiteral("./assets/user.png")));
    m_avatars.insert(QStringLiteral("email"), QImage(QStringLiteral("./assets/email.png")));
}

void AvatarProvider::insertImage(const QByteArray& data)
{
    QImage avatar;
    avatar.loadFromData(data);
    m_avatars.insert(QString::number(qHash(data)), avatar);
}

QImage AvatarProvider::requestImage(const QString &id, QSize *size, const QSize& requestedSize)
{
    QImage img;

    if (m_avatars.contains(id))
        img = m_avatars[id];

    if (requestedSize.isValid())
        img = img.scaled(requestedSize, Qt::IgnoreAspectRatio);
    
    if (size)
        *size = img.size();

    return img;
}

ContactsModelFetcher::ContactsModelFetcher()
{
}

void ContactsModelFetcher::requestAllVCards(const QString& address, const QString& location, const QString& phonebook)
{
    if (isRunning())
        terminate();

    m_address = address;
    m_location = location;
    m_phonebook = phonebook;

    start();
}

void ContactsModelFetcher::run()
{

    qDebug() << __PRETTY_FUNCTION__ << QThread::currentThreadId();

    const QString ooService = QStringLiteral("org.openobex.client");

    org::openobex::Client ooc(ooService, QStringLiteral("/"), QDBusConnection::sessionBus());
    QVariantMap target;
    target["Destination"] = m_address;
    target["Target"] = QStringLiteral("PBAP");
    QString sessionPath = ooc.CreateSession(target).value().path();
    qDebug() << "OpenOBEX session is" <<sessionPath;

    org::openobex::PhonebookAccess oopa(ooService, sessionPath, QDBusConnection::sessionBus());
    oopa.Select(m_location, m_phonebook).waitForFinished();
    oopa.SetFormat(QStringLiteral("vcard21"));
    int nVCards = oopa.GetSize().value();
    qDebug() << "Phonebook for the saved contacts has" << nVCards << "entries";

    QString contactsFilename = QStringLiteral("contacts.vcf");
    QFile infile(contactsFilename);
    if (infile.open(QIODevice::ReadOnly)) {
        QVersitReader reader(&infile);
        reader.startReading();
        reader.waitForFinished();
        if (reader.results().length() == nVCards) {
            emit fetchedAllVCards(reader.results());
            return;
        }
    }
    
    QStringList filter;
    filter << QStringLiteral("N") << QStringLiteral("FN") << QStringLiteral("LABEL")
           << QStringLiteral("TEL") << QStringLiteral("EMAIL") << QStringLiteral("PHOTO")
           << QStringLiteral("BDAY") << QStringLiteral("ADR") << QStringLiteral("NOTE")
           << QStringLiteral("UID");
    oopa.SetFilter(filter);

    QString vcards = oopa.PullAll();

    QVersitReader reader(vcards.toUtf8());
    reader.startReading();
    reader.waitForFinished();

    QFile outfile(contactsFilename);
    if (outfile.open(QIODevice::WriteOnly)) {
        QVersitWriter writer(&outfile);
        writer.startWriting(reader.results());
        writer.waitForFinished();
    }
        
    emit fetchedAllVCards(reader.results());

    qDebug() << __PRETTY_FUNCTION__ << QThread::currentThreadId() << "exit";
}

ContactsModel::ContactsModel(const QString& address, const QString& location, const QString& phonebook, AvatarProvider *avatarProvider, QObject* parent) :
    QAbstractListModel(parent),
    m_address(address),
    m_location(location),
    m_phonebook(phonebook),
    m_avatarProvider(avatarProvider),
    m_loading(false)
{
    qDebug() << __PRETTY_FUNCTION__ << QThread::currentThreadId();

    m_roleNames = QAbstractListModel::roleNames();
    m_roleNames.insert(VcfNameRole, QByteArray("vcfName"));
    m_roleNames.insert(NRole, QByteArray("n"));
    m_roleNames.insert(FNRole, QByteArray("fn"));
    m_roleNames.insert(LabelRole, QByteArray("label"));
    m_roleNames.insert(TelRole, QByteArray("tel"));
    m_roleNames.insert(EmailRole, QByteArray("email"));
    m_roleNames.insert(PhotoRole, QByteArray("photo"));
    m_roleNames.insert(BdayRole, QByteArray("bday"));
    m_roleNames.insert(AddressRole, QByteArray("address"));
    m_roleNames.insert(NoteRole, QByteArray("note"));
    m_roleNames.insert(UIDRole, QByteArray("uid"));

    m_fetcher = new ContactsModelFetcher();

    connect(m_fetcher, SIGNAL(fetchedAllVCards(QVersitDocumentList)), this, SLOT(processAllVCards(QVersitDocumentList)), Qt::QueuedConnection);
    connect(this, SIGNAL(requestAllVCards(QString,QString,QString)), m_fetcher, SLOT(requestAllVCards(QString,QString,QString)), Qt::QueuedConnection);

    emit requestAllVCards(m_address, m_location, m_phonebook);

    m_loading = true;
    emit loadingChanged();
}

ContactsModel::~ContactsModel() {
    m_fetcher->quit();
    m_fetcher->wait();
}

QHash<int, QByteArray> ContactsModel::roleNames() const
{
    return m_roleNames;
}

int ContactsModel::rowCount(const QModelIndex &parent) const
{
    if (parent.isValid())
        return 0;
    return m_data.count();
}

static inline QString getPhotoSource(const QByteArray& data)
{
    return QStringLiteral("image://avatar/%1").arg(qHash(data));
}
    

QVariant ContactsModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid() || index.row() < 0 || index.row() >= m_data.size())
        return QVariant();

    if (role == VcfNameRole)
        return m_data.at(index.row()).vcfName;

    if (!m_roleNames.contains(role))
        return QVariant();

    QString property = m_roleNames.value(role).toUpper();
    QVersitDocument document = m_data.at(index.row()).document;

    QVariantList data;
    foreach (const QVersitProperty& p, document.properties()) {
        if (property == p.name()) {
            QVariant value = p.variantValue();
            if (!value.isNull() && !(value.type() == QVariant::String && value.toString().isEmpty())) 
                data << p.variantValue();
        }
    }

    if (data.length() == 0)
        return QVariant();
    else if (property == QStringLiteral("N") || property == QStringLiteral("FN"))
        return data.front();
    else if (property == QStringLiteral("PHOTO"))
        return getPhotoSource(data.front().toByteArray());
    return data;
}

void ContactsModel::processAllVCards(const QVersitDocumentList& vcards)
{
    qDebug() << __PRETTY_FUNCTION__ << QThread::currentThreadId();

    if (vcards.length() == 0) {
        m_loading = false;
        emit loadingChanged();
        qDebug() << __PRETTY_FUNCTION__ << QThread::currentThreadId() << "exit at 0";
        return;
    }

    beginResetModel();
    m_data.clear();
    for (int i = 0; i < vcards.length(); ++i) {
        QVersitDocument document = vcards.at(i);
        m_data << ContactsItem(QStringLiteral("%1.vcf").arg(i));
        m_data.back().document = document;
        foreach (const QVersitProperty& p, document.properties()) {
            if (p.name() == QStringLiteral("PHOTO") && m_avatarProvider) {
                m_avatarProvider->insertImage(p.variantValue().toByteArray());
            }
        }
    }
    endResetModel();

    m_loading = false;
    emit loadingChanged();
    qDebug() << __PRETTY_FUNCTION__ << QThread::currentThreadId() << "exit";
}

QVariantMap ContactsModel::getCallInfo(const QString& call)
{
    OfonoVoiceCall ovc(call);
    QString phoneNumber = ovc.lineIdentification();
    foreach (const ContactsItem& item, m_data) {
        foreach (const QVersitProperty& p, item.document.properties()) {
            if (p.name() == QStringLiteral("TEL") && p.value() == phoneNumber) {
                QVariantMap info;
                foreach (const QVersitProperty& pp, item.document.properties())
                    if (pp.name() == QStringLiteral("PHOTO"))
                        info[pp.name()] = getPhotoSource(pp.variantValue().toByteArray());
                    else
                        info[pp.name()] = pp.value();
                info[QStringLiteral("TEL")] = phoneNumber;
                qDebug() << info;
                return info;
            }
        }
    }
    return QVariantMap();
}
