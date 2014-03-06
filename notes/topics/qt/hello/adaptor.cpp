#include "adaptor.h"

Adaptor::Adaptor(QObject* parent) :
    QObject(parent),
    model1(QStringList() << "A" << "B" << "C" << "D", "green"),
    model2(QStringList() << "X" << "Y" << "Z", "red")
{
    
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


