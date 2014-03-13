#include <QGuiApplication>
#include <QtQuick/QQuickView>
#include <QQmlContext>
#include "model.h"
#include "adaptor.h"

int main(int argc, char *argv[])
{
    qmlRegisterType<Model>("",1,0,"Model");

    QGuiApplication app(argc, argv);
    
    QQuickView *view = new QQuickView;

    QObject::connect((QObject*)view->engine(), SIGNAL(quit()), qApp, SLOT(quit()));

    Model rootModel(QStringList() << "Alex" << "Ford");
    view->rootContext()->setContextProperty(QStringLiteral("rootModel"), &rootModel);
    Adaptor adaptor;
    view->rootContext()->setContextProperty(QStringLiteral("adaptor"), &adaptor);
    view->setSource(QUrl("main.qml"));
    view->show();
    
    return app.exec();
}

