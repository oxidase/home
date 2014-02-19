#include <QGuiApplication>
#include <QtQuick/QQuickView>

int main(int argc, char *argv[])
{
    QGuiApplication app(argc, argv);
    
    QQuickView *view = new QQuickView;

    QObject::connect((QObject*)view->engine(), SIGNAL(quit()), qApp, SLOT(quit()));
    
    view->setSource(QUrl("main.qml"));
    view->show();
    
    return app.exec();
}

