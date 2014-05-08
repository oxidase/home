// -*- compile-command: "make" -*-
#ifndef CALLS_MODEL_H
#define CALLS_MODEL_H

#include <QtQml>
#include <QAbstractItemModel>
#include <QList>
#include <QDateTime>
#include <ofonovoicecall.h>
#include <ofonovoicecallmanager.h>

class VoiceCall : public OfonoVoiceCall {
    Q_OBJECT
public:
    VoiceCall(const QString& call, QObject* parent = 0);

    QString path() const { return m_call->path(); }
    QString lineIdentification() const { return m_call->lineIdentification(); }
    QString incomingLine() const { return m_call->incomingLine(); }
    QString name() const { return m_call->name(); }
    QString state() const { return m_call->state(); }
    QDateTime startTime() const { return m_startTime; }
    QString information() const { return m_call->information(); }
    bool multiparty() const { return m_call->multiparty(); }
    bool emergency() const { return m_call->emergency(); }
    quint8 icon() const { return m_call->icon(); }
    bool remoteHeld() const { return m_call->remoteHeld(); }
    bool remoteMultiparty() const { return m_call->remoteMultiparty(); }

signals:
    void callDataChanged(const QString& path);

public Q_SLOTS:
    void onDataChanged();

private:
    OfonoVoiceCall* m_call;
    QDateTime m_startTime;
};

class CallsModel : public QAbstractListModel {
    Q_OBJECT
public:
    enum {
        PathRole = Qt::UserRole + 1,
        LineIdentificationRole,
        IncomingLineRole,
        NameRole,
        StateRole,
        StartTimeRole,
        InformationRole,
        MultipartyRole,
        EmergencyRole,
        IconRole,
        RemoteHeldRole,
        RemoteMultipartyRole
    };

    CallsModel(OfonoVoiceCallManager* parent = 0);
    ~CallsModel();
    QHash<int,QByteArray> roleNames() const;
    int rowCount(const QModelIndex &parent = QModelIndex()) const;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;

    Q_INVOKABLE void answer(const QString &call);
    Q_INVOKABLE void hangup(const QString &call);

protected Q_SLOTS:
    void onCallAdded(const QString &call, const QVariantMap &values);
    void onCallRemoved(const QString &call);
    void onCallDataChanged(const QString &call);

private:
    QList<VoiceCall*> m_data;
    QHash<int,QByteArray> m_roleNames;

    int callIndex(const QString &call);
};

QML_DECLARE_TYPE(VoiceCall)
QML_DECLARE_TYPE(CallsModel)

#endif
