TARGET = qtgeoservices_google
QT += location-private positioning-private network

PLUGIN_TYPE = geoservices
load(qt_plugin)

HEADERS += \
    qgeoserviceproviderplugingoogle.h \
    qgeotiledmappingmanagerenginegoogle.h \
    qgeotilefetchergoogle.h \
    qgeomapreplygoogle.h \
    qgeocodingmanagerenginegoogle.h \
    qgeocodereplygoogle.h \
    qgeoroutingmanagerengineosm.h \
    qgeoroutereplyosm.h


SOURCES += \
    qgeoserviceproviderplugingoogle.cpp \
    qgeotiledmappingmanagerenginegoogle.cpp \
    qgeotilefetchergoogle.cpp \
    qgeomapreplygoogle.cpp \
    qgeocodingmanagerenginegoogle.cpp \
    qgeocodereplygoogle.cpp \
    qgeoroutingmanagerengineosm.cpp \
    qgeoroutereplyosm.cpp

OTHER_FILES += \
    google_maps_plugin.json

