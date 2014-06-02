// -*- compile-command: "make" -*-
#ifndef DIFFHELPER_H
#define DIFFHELPER_H

#include <QObject>
#include <QtQml>

class DiffHelper : public QObject {
    Q_OBJECT

    // Q_PROPERTY(QQmlListProperty<Model> models READ models CONSTANT)

public:
    DiffHelper(QObject* parent = 0);
    virtual ~DiffHelper();

    Q_INVOKABLE QString md5(const QString& fileName) const;
};

QML_DECLARE_TYPE(DiffHelper)

#endif
