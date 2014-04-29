QT += quick dbus bluetooth organizer versitorganizer
CONFIG *= link_pkgconfig
QMAKE_CXXFLAGS += -std=c++11
PKGCONFIG += ofono-qt

SUBDIRS += imports

SOURCES = main.cpp contactsmodel.cpp bluezagent.cpp callmanager.cpp
HEADERS = contactsmodel.h bluezagent.h callmanager.h

DBUS_INTERFACES = interfaces/org.bluez.Manager.xml \
                  interfaces/org.bluez.Adapter.xml \
                  interfaces/org.bluez.Device.xml \
                  interfaces/org.openobex.Client.xml \
                  interfaces/org.openobex.PhonebookAccess.xml
DBUS_ADAPTORS = interfaces/org.bluez.Agent.xml