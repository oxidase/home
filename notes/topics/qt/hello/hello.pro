QT += quick qml
SOURCES = main.cpp model.cpp adaptor.cpp
HEADERS = model.h adaptor.h


arch = $$(TARGET_ARCH)
contains(arch, 'arm') {
    BUILD_DIR = build/arm
    INSTALL_PATH = /mnt/target/tmp/hello
} else {
    BUILD_DIR = build/default
    INSTALL_PATH = /tmp/hello
}
release: DESTDIR = $$BUILD_DIR/release
debug:   DESTDIR = $$BUILD_DIR/debug

OBJECTS_DIR = $$DESTDIR/.obj
MOC_DIR = $$DESTDIR/.moc
RCC_DIR = $$DESTDIR/.qrc
UI_DIR = $$DESTDIR/.ui

target.files = main.qml $$DESTDIR/hello
target.path = $$INSTALL_PATH
INSTALLS += target
