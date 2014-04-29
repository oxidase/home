// -*- compile-command: "make" -*-
#include <QDebug>
#include "callmanager.h"

CallManager::CallManager(const QString& path, QObject* parent) :
    OfonoVoiceCallManager(OfonoModem::AutomaticSelect, path, parent)
{
}

void CallManager::dial(const QString& number)
{
    qDebug() << "dial" << number;
    bool success = false;
    OfonoVoiceCallManager::dial(number, QString(), success);
}
