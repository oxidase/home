QT += quick 
SOURCES = main.cpp


eval($$(TARGET_ARCH)=='arm') {
    BUILD_DIR = build/arm
    INSTALL_PATH = /mnt/target/tmp/hello
} else {
    BUILD_DIR = build/default
    INSTALL_PATH = /tmp/hello
}
release: DESTDIR = $$BUILD_DIR/releaes
debug:   DESTDIR = $$BUILD_DIR/debug

OBJECTS_DIR = $$DESTDIR/.obj
MOC_DIR = $$DESTDIR/.moc
RCC_DIR = $$DESTDIR/.qrc
UI_DIR = $$DESTDIR/.ui

target.files = main.qml $$DESTDIR/hello
target.path = $$INSTALL_PATH
INSTALLS += target
