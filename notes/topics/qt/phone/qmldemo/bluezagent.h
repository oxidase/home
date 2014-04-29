#ifndef BLUEZ_AGENT_H
#define BLUEZ_AGENT_H

#include <QtCore/QObject>
#include <QtDBus/QtDBus>

class BluezAgent: public QObject
{
    Q_OBJECT
public:
    BluezAgent(QObject *parent = 0);
    virtual ~BluezAgent();

public Q_SLOTS: // METHODS
    void Authorize(const QDBusObjectPath &device, const QString &uuid);
    void Cancel();
    void ConfirmModeChange(const QString &mode);
    void DisplayPasskey(const QDBusObjectPath &device, uint passkey);
    void DisplayPinCode(const QDBusObjectPath &device, const QString &pincode);
    void Release();
    void RequestConfirmation(const QDBusObjectPath &device, uint passkey);
    uint RequestPasskey(const QDBusObjectPath &device);
    QString RequestPinCode(const QDBusObjectPath &device);
};


#endif
