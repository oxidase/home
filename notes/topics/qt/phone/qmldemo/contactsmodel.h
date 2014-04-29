// -*- compile-command: "make" -*-
#ifndef CONTACTS_MODEL_H
#define CONTACTS_MODEL_H

#include <QAbstractItemModel>
#include <QList>
#include <QThread>
#include <QQuickImageProvider>

#include <QtVersit/qversitdocument.h>

typedef QList<QtVersit::QVersitDocument> QVersitDocumentList;

class AvatarProvider : public QQuickImageProvider
{
public:
    explicit AvatarProvider();
    virtual QImage requestImage(const QString &id, QSize *size, const QSize& requestedSize);
    void insertImage(const QByteArray& data);
private:
    QHash<QString, QImage> m_avatars;
};

// QTVERSITORGANIZER_USE_NAMESPACE
class ContactsModelFetcher : public QThread
{
    Q_OBJECT
public:
    ContactsModelFetcher();

signals:
    void fetchedAllVCards(const QVersitDocumentList& vcards);

public slots:
    void requestAllVCards(const QString& address, const QString& location, const QString& phonebook);

protected:
    void run();

    QString m_address;
    QString m_location;
    QString m_phonebook;
};

struct ContactsItem {
    QString vcfName;
    QtVersit::QVersitDocument document;

    ContactsItem(const QString& vcfName) : vcfName(vcfName) {}
};

class ContactsModel : public QAbstractListModel {
    Q_OBJECT
    Q_PROPERTY(bool loading READ getLoading NOTIFY loadingChanged)
public:
    enum {
        VcfNameRole = Qt::UserRole + 1,
        NRole,
        FNRole,
        TelRole,
        EmailRole,
        LabelRole,
        PhotoRole,
        BdayRole,
        AddressRole,
        NoteRole,
        UIDRole
    };

    ContactsModel(const QString& address, const QString& location, const QString& phonebook, AvatarProvider *avatarProvider = 0, QObject* parent = 0);
    ~ContactsModel();
    QHash<int,QByteArray> roleNames() const;
    int rowCount(const QModelIndex &parent = QModelIndex()) const;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;

signals:
    void requestAllVCards(const QString& address, const QString& location, const QString& phonebook);
    void loadingChanged();

public:
    bool getLoading() const { return m_loading; }

    Q_INVOKABLE QVariantMap getCallInfo(const QString& call);

public slots:
    void processAllVCards(const QVersitDocumentList& vcards);
    
private:
    QList<ContactsItem> m_data;
    QMap<QString, int> m_vcard2index;
    QString m_address;
    QString m_location;
    QString m_phonebook;
    QThread *m_fetcherThread;
    ContactsModelFetcher *m_fetcher;
    QHash<int, QByteArray> m_roleNames;
    AvatarProvider *m_avatarProvider;
    bool m_loading;
};

#endif
