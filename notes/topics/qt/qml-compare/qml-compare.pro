# -*- compile-command: "/usr/bin/qmake && make" -*-
QT += qml quick core-private gui-private quick-private qml-private
qtHaveModule(widgets): QT += widgets

SOURCES = main.cpp diffhelper.cpp changesmodel.cpp

HEADERS = diffhelper.h changesmodel.h
