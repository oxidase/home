// -*- compile-command: "make" -*-
#include "googlehelpers.h"

#include <limits>
#include <QtCore/QList>
#include <QtCore/QDebug>

QList<QGeoCoordinate> parsePolyline(const QByteArray &data)
{
    QList<QGeoCoordinate> path;

    bool parsingLatitude = true;

    int shift = 0;
    int value = 0;

    QGeoCoordinate coord(0, 0);

    for (int i = 0; i < data.length(); ++i) {
        unsigned char c = data.at(i) - 63;

        value |= (c & 0x1f) << shift;
        shift += 5;

        // another chunk
        if (c & 0x20)
            continue;

        int diff = (value & 1) ? ~(value >> 1) : (value >> 1);

        if (parsingLatitude) {
            coord.setLatitude(coord.latitude() + (double)diff/1e5);
        } else {
            coord.setLongitude(coord.longitude() + (double)diff/1e5);
            path.append(coord);
        }

        parsingLatitude = !parsingLatitude;

        value = 0;
        shift = 0;
    }

    return path;
}

QGeoCoordinate jsonCoordinate(const QJsonObject &obj)
{
    QGeoCoordinate coord;
    coord.setLatitude(obj.value(QStringLiteral("lat")).toDouble());
    coord.setLongitude(obj.value(QStringLiteral("lng")).toDouble());
    return coord;
}

QGeoRectangle jsonBoundingBox(const QJsonObject &obj)
{
    QGeoRectangle boundingBox;
    QJsonObject northeast = obj.value(QStringLiteral("northeast")).toObject();
    QJsonObject southwest = obj.value(QStringLiteral("southwest")).toObject();
    boundingBox.setTopRight(jsonCoordinate(northeast));
    boundingBox.setBottomLeft(jsonCoordinate(southwest));
    return boundingBox;
}

double jsonValue(const QJsonObject &obj)
{
    if (obj.contains(QStringLiteral("value")))
        return obj.value(QStringLiteral("value")).toDouble();
    return std::numeric_limits<double>::quiet_NaN();
}


