#include <QDebug>
#include "bluezagent.h"

BluezAgent::BluezAgent(QObject *parent) :
    QObject(parent)
{
    qDebug() << __PRETTY_FUNCTION__;
}

BluezAgent::~BluezAgent()
{
    qDebug() << __PRETTY_FUNCTION__;
}

void BluezAgent::Authorize(const QDBusObjectPath &device, const QString &uuid)
{
    qDebug() << __PRETTY_FUNCTION__ << device.path() << uuid;
}

void BluezAgent::Cancel()
{
    qDebug() << __PRETTY_FUNCTION__;
}

void BluezAgent::ConfirmModeChange(const QString &mode)
{
    qDebug() << __PRETTY_FUNCTION__ << mode;
}

void BluezAgent::DisplayPasskey(const QDBusObjectPath &device, uint passkey)
{
    qDebug() << __PRETTY_FUNCTION__ << device.path() << passkey;
}

void BluezAgent::DisplayPinCode(const QDBusObjectPath &device, const QString &pincode)
{
    qDebug() << __PRETTY_FUNCTION__ << device.path() << pincode;
}

void BluezAgent::Release()
{
    qDebug() << __PRETTY_FUNCTION__;
}

void BluezAgent::RequestConfirmation(const QDBusObjectPath &device, uint passkey)
{
    qDebug() << __PRETTY_FUNCTION__ << device.path() << passkey;
}

uint BluezAgent::RequestPasskey(const QDBusObjectPath &device)
{
    qDebug() << __PRETTY_FUNCTION__ << device.path();
    return 1234;
}

QString BluezAgent::RequestPinCode(const QDBusObjectPath &device)
{
    qDebug() << __PRETTY_FUNCTION__ << device.path();
    return QStringLiteral("1234");
}

