#include <QDebug>
#include "adaptor.h"

Adaptor::Adaptor(QObject* parent) :
    QObject(parent),
    model1(QStringList() << "A" << "B" << "C" << "D", "green"),
    model2(QStringList() << "X" << "Y" << "Z", "red")
{
    for (int i=0; i<10; ++i)
        m_modelList.append(0);
}

Adaptor::~Adaptor()
{
    deleteModels();
}

QObject* Adaptor::getModel()
{
    static int state = 0;
    if (++state % 2 == 0)
        return &model1;
    if (state % 2 == 1)
        return &model2;
    return &model1;
}

int modelsSize(QQmlListProperty<Model> *property)
{
    QList<Model*>* modelList = static_cast< QList<Model *> *>(property->data);
    if (!modelList)
        return 0;

    return modelList->size();
}

Model *modelAt(QQmlListProperty<Model> *property, int index)
{
    QList<Model*>* modelList = static_cast< QList<Model *> *>(property->data);
    if (!modelList)
        return 0;

    if (index < 0 || index >= modelList->size())
        return 0;

    if (!modelList->at(index)) {
        qDebug() << "Create a model for index" << index;

        QStringList data;
        data << "K from model index " + QString::number(index) << "M from model index " + QString::number(index);
        modelList->operator[](index) = new Model(data, "blue");
    }

    return modelList->at(index);
}

QQmlListProperty<Model> Adaptor::models()
{
    return QQmlListProperty<Model>(this, &m_modelList, &modelsSize, &modelAt);
}

void Adaptor::deleteModels()
{
    for (int i=0; i<m_modelList.size(); ++i) {
        Model* model = m_modelList.at(i);
        if (!model)
            continue;
        m_modelList[i] = 0;
        model->deleteLater();
    }
}
