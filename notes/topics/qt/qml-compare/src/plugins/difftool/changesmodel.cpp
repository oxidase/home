// -*- compile-command: "make" -*-
#include "changesmodel.h"

QHash<int, QByteArray> ChangesModel::roleNames() const
{
    QHash<int, QByteArray> roleNames = QAbstractListModel::roleNames();
    roleNames.insert(PathRole, QByteArray("path"));
    roleNames.insert(WhatRole, QByteArray("what"));
    roleNames.insert(OldValueRole, QByteArray("oldValue"));
    roleNames.insert(NewValueRole, QByteArray("newValue"));
    roleNames.insert(OldItemRole, QByteArray("oldItem"));
    roleNames.insert(NewItemRole, QByteArray("newItem"));
    return roleNames;
}

int ChangesModel::rowCount(const QModelIndex &parent) const
{
    if (parent.isValid())
        return 0;

    return m_changes.count();
}

QVariant ChangesModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid() || index.row() < 0 || index.row() >= m_changes.size())
        return QVariant();

    QVariant value;
    switch (role) {
    case PathRole:
        value = m_changes.at(index.row()).path;
        break;
    case WhatRole:
        value = m_changes.at(index.row()).what;
        break;
    case OldValueRole:
        value = m_changes.at(index.row()).oldValue;
        break;
    case NewValueRole:
        value = m_changes.at(index.row()).newValue;
        break;
    case OldItemRole:
        value.setValue(m_changes.at(index.row()).oldItem);
        break;
    case NewItemRole:
        value.setValue(m_changes.at(index.row()).newItem);
        break;
    }
    return value;
}

void ChangesModel::append(const ChangeItem &item)
{
    qDebug() << "append" << item.path << item.what << item.oldValue;
    beginInsertRows(QModelIndex(), m_changes.size(), m_changes.size());
    m_changes.append(item);
    endInsertRows();
    emit countChanged();
}

void ChangesModel::clear()
{
    if (m_changes.isEmpty())
        return;

    beginResetModel();
    m_changes.clear();
    endResetModel();
    emit countChanged();
}
