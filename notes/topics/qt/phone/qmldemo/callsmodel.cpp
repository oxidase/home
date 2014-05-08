// -*- compile-command: "make" -*-
#include <QDebug>
#include "callsmodel.h"

VoiceCall::VoiceCall(const QString& call, QObject* parent) :
    OfonoVoiceCall(call, parent)
{
    m_call = new OfonoVoiceCall(call, this);
    m_startTime = QDateTime::currentDateTime();

    connect(m_call, SIGNAL(lineIdentificationChanged(QString)), this, SLOT(onDataChanged()));
    connect(m_call, SIGNAL(nameChanged(QString)), this, SLOT(onDataChanged()));
    connect(m_call, SIGNAL(stateChanged(QString)), this, SLOT(onDataChanged()));
    connect(m_call, SIGNAL(informationChanged(QString)), this, SLOT(onDataChanged()));
    connect(m_call, SIGNAL(incomingLineChanged(QString)), this, SLOT(onDataChanged()));
    connect(m_call, SIGNAL(disconnectReason(QString)), this, SLOT(onDataChanged()));
    connect(m_call, SIGNAL(multipartyChanged(bool)), this, SLOT(onDataChanged()));
    connect(m_call, SIGNAL(iconChanged(quint8)), this, SLOT(onDataChanged()));
    connect(m_call, SIGNAL(emergencyChanged(bool)), this, SLOT(onDataChanged()));
    connect(m_call, SIGNAL(remoteHeldChanged(bool)), this, SLOT(onDataChanged()));
    connect(m_call, SIGNAL(remoteMultipartyChanged(bool)), this, SLOT(onDataChanged()));
}

void VoiceCall::onDataChanged() {
    emit callDataChanged(path());
}

CallsModel::CallsModel(OfonoVoiceCallManager* parent)
    : QAbstractListModel(parent)
{
    m_roleNames = QAbstractListModel::roleNames();
    m_roleNames.insert(PathRole, QByteArray("path"));
    m_roleNames.insert(LineIdentificationRole, QByteArray("lineIdentification"));
    m_roleNames.insert(IncomingLineRole, QByteArray("incomingLine"));
    m_roleNames.insert(NameRole, QByteArray("name"));
    m_roleNames.insert(StateRole, QByteArray("state"));
    m_roleNames.insert(StartTimeRole, QByteArray("startTime"));
    m_roleNames.insert(InformationRole, QByteArray("information"));
    m_roleNames.insert(MultipartyRole, QByteArray("multiparty"));
    m_roleNames.insert(EmergencyRole, QByteArray("emergency"));
    m_roleNames.insert(IconRole, QByteArray("icon"));
    m_roleNames.insert(RemoteHeldRole, QByteArray("remoteHeld"));
    m_roleNames.insert(RemoteMultipartyRole, QByteArray("remoteMultiparty"));

    connect(parent, SIGNAL(callAdded(QString,QVariantMap)), this, SLOT(onCallAdded(QString,QVariantMap)));
    connect(parent, SIGNAL(callRemoved(QString)), this, SLOT(onCallRemoved(QString)));

    foreach (const QString& call, parent->getCalls()) {
        m_data.push_back(new VoiceCall(call, this));
        connect(m_data.back(), SIGNAL(callDataChanged(QString)), this, SLOT(onCallDataChanged(QString)));
    }
}

CallsModel::~CallsModel()
{
}

QHash<int,QByteArray> CallsModel::roleNames() const
{
    return m_roleNames;
}

int CallsModel::rowCount(const QModelIndex &parent) const
{
    if (parent.isValid())
        return 0;
    return m_data.count();
}

QVariant CallsModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid() || index.row() < 0 || index.row() >= m_data.size())
        return QVariant();

    int idx = index.row();
    switch (role) {
    case PathRole:
        return m_data[idx]->path();
    case LineIdentificationRole:
        return m_data[idx]->lineIdentification();
    case IncomingLineRole:
        return m_data[idx]->incomingLine();
    case NameRole:
        return m_data[idx]->name();
    case StateRole:
        return m_data[idx]->state();
    case StartTimeRole:
        return m_data[idx]->startTime();
    case InformationRole:
        return m_data[idx]->information();
    case MultipartyRole:
        return m_data[idx]->multiparty();
    case EmergencyRole:
        return m_data[idx]->emergency();
    case IconRole:
        return m_data[idx]->icon();
    case RemoteHeldRole:
        return m_data[idx]->remoteHeld();
    case RemoteMultipartyRole:
        return m_data[idx]->remoteMultiparty();
    }
    return QVariant();
}

int CallsModel::callIndex(const QString &call)
{
    for (int i = 0; i < m_data.size(); ++i) {
        OfonoVoiceCall* c = m_data[i];
        if (c->path() == call)
            return i;
    }
    return -1;
}

void CallsModel::onCallAdded(const QString &call, const QVariantMap &)
{
    beginInsertRows(QModelIndex(), m_data.size(), m_data.size());
    m_data.push_back(new VoiceCall(call, this));
    connect(m_data.back(), SIGNAL(callDataChanged(QString)), this, SLOT(onCallDataChanged(QString)));
    endInsertRows();
}

void CallsModel::onCallRemoved(const QString &call)
{
    int idx = callIndex(call);
    if (idx == -1)
        return;
    beginRemoveRows(QModelIndex(), idx, idx);
    OfonoVoiceCall* c = m_data[idx];
    m_data.removeAt(idx);
    c->deleteLater();
    endRemoveRows();
}

void CallsModel::onCallDataChanged(const QString &call)
{
    int idx = callIndex(call);
    if (idx == -1)
        return;
    emit dataChanged(createIndex(idx, 0), createIndex(idx, 0));
}

void CallsModel::answer(const QString &call)
{
    int idx = callIndex(call);
    if (idx == -1)
        return;
    m_data[idx]->answer();
}

void CallsModel::hangup(const QString &call)
{
    int idx = callIndex(call);
    if (idx == -1)
        return;
    m_data[idx]->hangup();
}
