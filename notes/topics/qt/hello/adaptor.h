#ifndef ADAPTOR_H
#define ADAPTOR_H

#include <QObject>
#include "model.h"

class Adaptor : public QObject {
    Q_OBJECT
    
public:
    Adaptor(QObject* parent = 0);

    Q_INVOKABLE QObject* getModel();

private:
    Model model1, model2;
};

#endif
