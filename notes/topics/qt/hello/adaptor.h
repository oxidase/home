#ifndef ADAPTOR_H
#define ADAPTOR_H

#include <QObject>
#include <QQmlListProperty>
#include "model.h"

class Adaptor : public QObject {
    Q_OBJECT

    Q_PROPERTY(QQmlListProperty<Model> models READ models CONSTANT)

public:
    Adaptor(QObject* parent = 0);
    virtual ~Adaptor();

    Q_INVOKABLE QObject* getModel();

    QQmlListProperty<Model> models();
    void deleteModels();

private:
    Model model1, model2;
    QList<Model *> m_modelList;
};

#endif
