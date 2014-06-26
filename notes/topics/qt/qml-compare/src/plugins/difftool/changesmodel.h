// -*- compile-command: "make" -*-
#ifndef CHANGESMODEL_H
#define CHANGESMODEL_H

#include <QAbstractItemModel>
#include <QStringList>
#include <QtQml>

struct ChangeItem {
    QString path;
    QString what;
    QVariant oldValue;
    QVariant newValue;
    QObject* oldItem;
    QObject* newItem;

    ChangeItem(const QString& path, const QString& what, const QVariant& oldValue, const QVariant& newValue, QObject* oldItem,  QObject* newItem) :
        path(path), what(what), oldValue(oldValue), newValue(newValue), oldItem(oldItem),  newItem(newItem) {}
    ChangeItem(bool insertion, const QString& path, const QString& what, const QVariant& value, QObject* item) :
        path(path), what(what), oldItem(0),  newItem(0)
    {
        if (insertion) {
            newValue = value;
            newItem = item;
        } else {
            oldValue = value;
            oldItem = item;
        }
    }
};

class ChangesModel : public QAbstractListModel {
    Q_OBJECT
    Q_PROPERTY(int count READ rowCount NOTIFY countChanged)
public:
    enum {
        PathRole = Qt::UserRole + 1,
        WhatRole,
        OldValueRole,
        NewValueRole,
        OldItemRole,
        NewItemRole,
    };

    ChangesModel(QObject* parent = 0) : QAbstractListModel(parent) {};
    QHash<int,QByteArray> roleNames() const;
    int rowCount(const QModelIndex &parent = QModelIndex()) const;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;
    void append(const ChangeItem &item);
    void clear();

signals:
    void countChanged();

private:
    QList<ChangeItem> m_changes;
};

// QML_DECLARE_TYPE(ChangesModel)

#endif
