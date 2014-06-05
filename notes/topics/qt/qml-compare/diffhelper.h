// -*- compile-command: "make" -*-
#ifndef DIFFHELPER_H
#define DIFFHELPER_H

#include <QObject>
#include <QtQml>

#include <private/qquickitem_p.h>
#include <private/qquickstate_p.h>
#include <private/qquickstate_p_p.h>
#include <private/qquicktransition_p.h>

#include "changesmodel.h"

class DiffHelper : public QObject {
    Q_OBJECT

    Q_PROPERTY(ChangesModel* insertions READ getInsertionsModel CONSTANT)
    Q_PROPERTY(ChangesModel* deletions READ getDeletionsModel CONSTANT)
    Q_PROPERTY(ChangesModel* modifications READ getModificationsModel CONSTANT)

public:
    DiffHelper(QObject* parent = 0);
    virtual ~DiffHelper();

    Q_INVOKABLE QString md5(const QString& fileName) const;

    Q_INVOKABLE QString absolutePath(const QString& path) const;

    Q_INVOKABLE void getChanges(QObject* oldItem, QObject* newItem);

protected:
    ChangesModel* getInsertionsModel() const { return const_cast<ChangesModel*>(&insertionsModel); }
    ChangesModel* getDeletionsModel() const { return const_cast<ChangesModel*>(&deletionsModel); }
    ChangesModel* getModificationsModel() const { qDebug() << "getModificationsModel" ; return const_cast<ChangesModel*>(&modificationsModel); }

signals:
    void changesModelUpdated();

private:
    QString getObjectName(QObject* o) const;

    void getChanges(QObject* oldItem, QObject* newItem, QQuickItem* oldParentItem, QQuickItem* newParentItem, const QString& parentPath);

    void getChanges(QQuickStateAction oldItem, QQuickStateAction newItem, QQuickItem* oldParentItem, QQuickItem* newParentItem, const QString& parentPath);

    template <typename T>
    void getChanges(QList<T> oldItem, QList<T> newItem, QQuickItem* oldParentItem, QQuickItem* newParentItem, const QString& parentPath);

    template <typename T>
    void getChanges(QQmlListProperty<T> oldItem, QQmlListProperty<T> newItem, QQuickItem* oldParentItem, QQuickItem* newParentItem, const QString& parentPath);

    QSet<QObject*> visitedItems;
    ChangesModel insertionsModel;
    ChangesModel deletionsModel;
    ChangesModel modificationsModel;
};

QML_DECLARE_TYPE(DiffHelper)

#endif
