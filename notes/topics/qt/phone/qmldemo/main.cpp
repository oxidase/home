// -*- compile-command: "make" -*-
#include <QGuiApplication>
#include <QtQuick/QQuickView>
#include <QQmlContext>
#include <QDebug>

#include <ofonomodemmanager.h>
#include <ofonoconnman.h>
#include <ofonovoicecallmanager.h>
#include <ofonovoicecall.h>
#include <ofonocallvolume.h>
#include <ofononetworkregistration.h>
#include <ofononetworkoperator.h>
#include <ofonovoicecall.h>
#include <ofonosimmanager.h>

#include <QtBluetooth/QBluetoothLocalDevice>
#include <QtBluetooth/QBluetoothAddress>
#include <QtBluetooth/QBluetoothSocket>

#include <QtVersitOrganizer/qversitorganizerimporter.h>
#include <QtVersit/qversitreader.h>
#include <QtVersit/qversitproperty.h>

#include "manager_interface.h"
#include "adapter_interface.h"
#include "device_interface.h"
#include "agent_adaptor.h"

#include "client_interface.h"
#include "phonebookaccess_interface.h"

#include "bluezagent.h"
#include "contactsmodel.h"
#include "callmanager.h"
#include "callsmodel.h"

QTVERSITORGANIZER_USE_NAMESPACE

int main(int argc, char *argv[])
{
    qRegisterMetaType< QVersitDocumentList >("QVersitDocumentList");
    qDBusRegisterMetaType< QPair<QString, QString> >();
    qDBusRegisterMetaType< QList<QPair<QString, QString> > >();

    qmlRegisterType<CallsModel>("",1,0,"CallsModel");

    QGuiApplication app(argc, argv);

    QQuickView *view = new QQuickView;

    QObject::connect((QObject*)view->engine(), SIGNAL(quit()), qApp, SLOT(quit()));

#if 0
    org::bluez::Manager obm(QStringLiteral("org.bluez"), QStringLiteral("/"), QDBusConnection::systemBus());
    QString defaultAdapter = obm.DefaultAdapter().value().path();
    org::bluez::Adapter oba(QStringLiteral("org.bluez"), defaultAdapter, QDBusConnection::systemBus());
    qDebug() << oba.GetProperties().value();
    QString defaultDevice = oba.ListDevices().value().at(0).path();
    org::bluez::Device obd(QStringLiteral("org.bluez"), defaultDevice, QDBusConnection::systemBus());
    QVariantMap odbProps;
    odbProps = obd.GetProperties().value();
    qDebug() << odbProps;

    org::openobex::Client ooc(QStringLiteral("org.openobex.client"), QStringLiteral("/"), QDBusConnection::sessionBus());
    QVariantMap argsTarget;
    argsTarget["Destination"] = odbProps[QStringLiteral("Address")];
    argsTarget["Target"] = QStringLiteral("PBAP");
    QString sessionPath = ooc.CreateSession(argsTarget).value().path();
    qDebug() << argsTarget << sessionPath;

    org::openobex::PhonebookAccess oopa(QStringLiteral("org.openobex.client"), sessionPath, QDBusConnection::sessionBus());
    oopa.Select(QStringLiteral("INT"), QStringLiteral("PB")).waitForFinished();
    oopa.SetFormat(QStringLiteral("vcard40"));
    qDebug() << "\nPhonebook for the saved contacts has" << oopa.GetSize().value() << "values";

    QStringList filter;
    filter << "N" << "FN" << "LABEL" << "TEL" << "EMAIL" <<"PHOTO" << "BDAY" << "ADR" << "NOTE" << "UID";
    oopa.SetFilter(filter);
    qDebug() << oopa.PullAll();

    QList<QPair<QString, QString> > list = oopa.List();
    qDebug() << list ;

    qDebug() << "\nPhonebook has possible " << oopa.ListFilterFields().value() << "filter entries";
    qDebug() << "\nPhonebook has current " << oopa.GetFilter().value() << "filter entries";

    QByteArray vCard = oopa.Pull("0.vcf").value().toUtf8();
    qDebug() << "\n239.vcf : " << vCard;

    QVersitReader* mReader = new QVersitReader(vCard);
    mReader->startReading();
    mReader->waitForFinished();
    qDebug() << mReader->results();

    // org::openobex::Synchronization oos(QStringLiteral("org.openobex.client"), sessionPath, QDBusConnection::sessionBus());
    // oos.SetLocation("INT");
    // qDebug() << oos.GetPhonebook();

#else

    QString bluezService = QStringLiteral("org.bluez");
    org::bluez::Manager obm(bluezService, QStringLiteral("/"), QDBusConnection::systemBus());
    QString defaultAdapter = obm.DefaultAdapter().value().path();
    org::bluez::Adapter oba(bluezService, defaultAdapter, QDBusConnection::systemBus());
    qDebug() << oba.GetProperties().value();
    QString defaultDevice = oba.ListDevices().value().at(0).path();
    org::bluez::Device obd(bluezService, defaultDevice, QDBusConnection::systemBus());
    QVariantMap odbProps;
    odbProps = obd.GetProperties().value();
    qDebug() << odbProps;

    QString agentPath = QStringLiteral("/agent");
    QString agentCapability = QStringLiteral("KeyboardDisplay");
    BluezAgent bluezAgent;
    AgentAdaptor bluezAgentAdaptor(&bluezAgent);
    QDBusConnection::systemBus().registerObject(agentPath, &bluezAgentAdaptor);
    // oba.RegisterAgent(QDBusObjectPath(agentPath), agentCapability);

    OfonoModemManager omm(0);
    QString modemPath = omm.modems().length() > 0 ? omm.modems().at(0) : "";
    CallManager ovcm(modemPath);

	AvatarProvider *avatarProvider = new AvatarProvider;

    ContactsModel contactsModel(odbProps[QStringLiteral("Address")].toString(), QStringLiteral("INT"), QStringLiteral("PB"),
                                avatarProvider);

    view->engine()->addImageProvider("avatar", avatarProvider);
    view->rootContext()->setContextProperty(QStringLiteral("callManager"), &ovcm);
    view->rootContext()->setContextProperty(QStringLiteral("contactsModel"), &contactsModel);
    view->setSource(QUrl("main.qml"));
    view->show();
#endif

    return app.exec();
}
