#include <QApplication>
#include <QLabel>

#ifndef QT_NO_OPENGL
#include "mainwidget.h"
#endif

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    app.setApplicationName("objects");
    app.setApplicationVersion("0.1");

#ifndef QT_NO_OPENGL
    MainWidget widget;
    widget.show();
#else
    QLabel note("OpenGL Support required");
    note.show();
#endif
    return app.exec();
}
