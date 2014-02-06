// -*- compile-command: "make" -*-
#ifndef GOOGLEHELPERS_H
#define GOOGLEHELPERS_H

#include <QtCore/QJsonObject>
#include <QtPositioning/QGeoCoordinate>
#include <QtPositioning/QGeoRectangle>

QList<QGeoCoordinate> parsePolyline(const QByteArray &data);
QGeoCoordinate jsonCoordinate(const QJsonObject &obj);
QGeoRectangle jsonBoundingBox(const QJsonObject &obj);
double jsonValue(const QJsonObject &obj);

#endif // GOOGLEHELPERS_H
