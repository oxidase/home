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
    qgeoroutingmanagerenginegoogle.h \
    qgeoroutereplygoogle.h \
    qplacemanagerenginegoogle.h \
    qplacesearchreplygoogle.h \
    googlehelpers.h


SOURCES += \
    qgeoserviceproviderplugingoogle.cpp \
    qgeotiledmappingmanagerenginegoogle.cpp \
    qgeotilefetchergoogle.cpp \
    qgeomapreplygoogle.cpp \
    qgeocodingmanagerenginegoogle.cpp \
    qgeocodereplygoogle.cpp \
    qgeoroutingmanagerenginegoogle.cpp \
    qgeoroutereplygoogle.cpp \
    qplacemanagerenginegoogle.cpp \
    qplacesearchreplygoogle.cpp \
    googlehelpers.cpp

OTHER_FILES += \
    google_maps_plugin.json

