#include "model.h"

Model::Model(const QStringList& names, const QString& color, QObject* parent) :
    QAbstractListModel(parent),
    m_names(names), m_color(color)
{
}

QHash<int, QByteArray> Model::roleNames() const
{
    QHash<int, QByteArray> roleNames = QAbstractListModel::roleNames();
    roleNames.insert(NameRole, QByteArray("name"));
    roleNames.insert(ColorRole, QByteArray("colorRole"));
    return roleNames;
}

int Model::rowCount(const QModelIndex &parent) const
{
    if (parent.isValid())
        return 0;
    return m_names.count();
}

QVariant Model::data(const QModelIndex &index, int role) const
{
    if (!index.isValid() || index.row() < 0 || index.row() >= m_names.size())
        return QVariant();

    switch (role) {
    case NameRole:
        return m_names.at(index.row());
    case ColorRole:
        return m_color;
    }
    return QVariant();
}
