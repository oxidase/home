// -*- compile-command: "make" -*-
#ifndef CALL_MANAGER_H
#define CALL_MANAGER_H

#include <ofonovoicecallmanager.h>
#include "callsmodel.h"

class CallManager : public OfonoVoiceCallManager {
    Q_OBJECT
    Q_PROPERTY(CallsModel* calls READ getCallsModel CONSTANT)
public:
    CallManager(const QString& path, QObject* parent = 0);

public slots:
    void dial(const QString& number);

    CallsModel* getCallsModel() const { return m_callsModel; }

private:
    CallsModel* m_callsModel;
};

#endif
