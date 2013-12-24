QT       += core gui widgets 3d
CONFIG   += c++11

TARGET = glyphs
TEMPLATE = app

SOURCES += main.cpp

qtHaveModule(opengl) {
    QT += opengl

    SOURCES += mainwidget.cpp

    HEADERS += \
        mainwidget.h
}
