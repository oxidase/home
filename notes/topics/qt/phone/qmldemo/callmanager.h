// -*- compile-command: "make" -*-
#ifndef CALL_MANAGER_H
#define CALL_MANAGER_H

#include <ofonovoicecallmanager.h>

class CallManager : public OfonoVoiceCallManager {
    Q_OBJECT
public:
    CallManager(const QString& path, QObject* parent = 0);

public slots:
    void dial(const QString& number);
};

#endif
