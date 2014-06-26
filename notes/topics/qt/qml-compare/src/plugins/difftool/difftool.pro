# -*- compile-command: "/usr/bin/qmake && make" -*-
TEMPLATE = lib
CONFIG += plugin debug
QT *= qml quick core-private gui-private quick-private qml-private

TARGET = qmldifftoolplugin
URI = QmlDiffTool

SOURCES += diffhelper.cpp \
        changesmodel.cpp
HEADERS += plugin.h \
        diffhelper.h \
        changesmodel.h
DEFINES += "URI='\"$$URI\"'"

IMPORTS = $$top_builddir/imports
DESTDIR = $$IMPORTS/$$URI
OBJECTS_DIR = .obj
MOC_DIR = .moc

OTHER_FILES += \
     qmldir

# Copy the qmldir file to the same folder as the plugin binary
QMAKE_POST_LINK += $$QMAKE_COPY $$quote($$PWD/qmldir) $$quote($$DESTDIR) $$escape_expand(\\n\\t)

# Generate plugins.qmltypes file
load(resolve_target)
qmltypes.target = qmltypes
qmltypes.commands = $$[QT_INSTALL_BINS]/qmlplugindump $$URI 1.0 $$IMPORTS > $$PWD/plugins.qmltypes && $$QMAKE_COPY $$quote($$PWD/plugins.qmltypes) $$quote($$DESTDIR)
qmltypes.depends = $$QMAKE_RESOLVED_TARGET
QMAKE_EXTRA_TARGETS += qmltypes

# Install target and qmldir
installPath = $$[QT_INSTALL_QML]/$$replace(URI, \\., /)
qmldir.files = qmldir
qmldir.path = $$installPath
target.path = $$installPath
INSTALLS += target qmldir
