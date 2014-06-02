// -*- compile-command: "make" -*-
#ifndef DIFFHELPER_H
#define DIFFHELPER_H

#include <QObject>
#include <QtQml>

#include <private/qquickstate_p.h>
#include <private/qquickstate_p_p.h>
#include <private/qquicktransition_p.h>

class DiffHelper : public QObject {
    Q_OBJECT

    // Q_PROPERTY(QQmlListProperty<Model> models READ models CONSTANT)

public:
    DiffHelper(QObject* parent = 0);
    virtual ~DiffHelper();

    Q_INVOKABLE QString md5(const QString& fileName) const;

    Q_INVOKABLE QStringList getChanges(QObject* oldItem, QObject* newItem, const QString& indent = QString()) const;

private:
    void getChanges(QQuickStateAction oldItem, QQuickStateAction newItem, const QString& indent) const;

    template <typename T>
    void getChanges(QList<T> oldItem, QList<T> newItem, const QString& indent) const;

    template <typename T>
    void getChanges(QQmlListProperty<T> oldItem, QQmlListProperty<T> newItem, const QString& indent) const;
};

QML_DECLARE_TYPE(DiffHelper)

#endif
