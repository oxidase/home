// -*- compile-command: "make" -*-
#include <QDebug>
#include "callmanager.h"
#include "contactsmodel.h"

CallManager::CallManager(const QString& path, QObject* parent) :
    OfonoVoiceCallManager(OfonoModem::AutomaticSelect, path, parent)
{
    m_callsModel = new CallsModel(this);
}

void CallManager::dial(const QString& number)
{
    qDebug() << "dial" << number;
    bool success = false;
    OfonoVoiceCallManager::dial(normalizePhoneNumber(number), QString(), success);
}
