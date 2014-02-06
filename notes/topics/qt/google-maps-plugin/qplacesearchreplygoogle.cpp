// -*- compile-command: "make" -*-
/****************************************************************************
**
** Copyright (C) 2013 Aaron McCarthy <mccarthy.aaron@gmail.com>
** Contact: http://www.qt-project.org/legal
**
** This file is part of the QtLocation module of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and Digia.  For licensing terms and
** conditions see http://qt.digia.com/licensing.  For further information
** use the contact form at http://qt.digia.com/contact-us.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU Lesser General Public License version 2.1 requirements
** will be met: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** In addition, as a special exception, Digia gives you certain additional
** rights.  These rights are described in the Digia Qt LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 3.0 as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU General Public License version 3.0 requirements will be
** met: http://www.gnu.org/copyleft/gpl.html.
**
**
** $QT_END_LICENSE$
**
****************************************************************************/

#include "qplacesearchreplygoogle.h"

#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>
#include <QtCore/QJsonArray>
#include <QtPositioning/QGeoCoordinate>
#include <QtPositioning/QGeoAddress>
#include <QtPositioning/QGeoLocation>
#include <QtPositioning/QGeoRectangle>
#include <unordered_map>

// // https://developers.google.com/maps/documentation/geocoding/

// QT_BEGIN_NAMESPACE

// enum QGeoCodeTypeGoogle {
//     GeoCodeTypeUnknown,
//     StreetAddress, // indicates a precise street address.
//     Route, // indicates a named route (such as "US 101").
//     Intersection, // indicates a major intersection, usually of two major roads.
//     Political, // indicates a political entity. Usually, this type indicates a polygon of some civil administration.
//     Country, // indicates the national political entity, and is typically the highest order type returned by the Geocoder.
//     AdministrativeAreaLevel1, // indicates a first-order civil entity below the country level. Within the United States, these administrative levels are states.
//     AdministrativeAreaLevel2, // indicates a second-order civil entity below the country level. Within the United States, these administrative levels are counties.
//     AdministrativeAreaLevel3, // indicates a third-order civil entity below the country level. This type indicates a minor civil division.
//     ColloquialArea, // indicates a commonly-used alternative name for the entity.
//     Locality, // indicates an incorporated city or town political entity.
//     Sublocality, // indicates a first-order civil entity below a locality. For some locations may receive one of the additional types: sublocality_level_1 through to sublocality_level_5. Each sublocality level is a civil entity. Larger numbers indicate a smaller geographic area.
//     SublocalityLevel1,
//     SublocalityLevel2,
//     SublocalityLevel3,
//     SublocalityLevel4,
//     SublocalityLevel5,
//     Neighborhood, // indicates a named neighborhood
//     Premise, // indicates a named location, usually a building or collection of buildings with a common name
//     Subpremise, // indicates a first-order entity below a named location, usually a singular building within a collection of buildings with a common name
//     PostalCode, // indicates a postal code as used to address postal mail within the country.
//     NaturalFeature, // indicates a prominent natural feature.
//     Airport, // indicates an airport.
//     Park, // indicates a named park.
//     PointOfInterest, // indicates a named point of interest. Typically, these "POI"s are prominent local entities that don't easily fit in another category such as "Empire State Building" or "Statue of Liberty."
//     Floor, // indicates the floor of a building address.
//     Establishment, // typically indicates a place that has not yet been categorized.
//     Parking, // indicates a parking lot or parking structure.
//     PostBox, // indicates a specific postal box.
//     PostalTown, // indicates a grouping of geographic areas, such as locality and sublocality, used for mailing addresses in some countries.
//     RoomIndicates, // the room of a building address.
//     StreetNumber, // indicates the precise street number.
//     BusStation, //  indicate the location of a bus stop.
//     TrainStation, //  indicate the location of a train stop.
//     TransitStation, // indicate the location of a public transit stop.
// };

// static QGeoCodeTypeGoogle QString2QGeoCodeTypeGoogle(const QString &key) {
//     static QMap<QString, QGeoCodeTypeGoogle> m = {
//         {QStringLiteral("street_address"), StreetAddress},
//         {QStringLiteral("route"), Route},
//         {QStringLiteral("intersection"), Intersection},
//         {QStringLiteral("political"), Political},
//         {QStringLiteral("country"), Country},
//         {QStringLiteral("administrative_area_level_1"), AdministrativeAreaLevel1},
//         {QStringLiteral("administrative_area_level_2"), AdministrativeAreaLevel2},
//         {QStringLiteral("administrative_area_level_3"), AdministrativeAreaLevel3},
//         {QStringLiteral("colloquial_area"), ColloquialArea},
//         {QStringLiteral("locality"), Locality},
//         {QStringLiteral("sublocality"), Sublocality},
//         {QStringLiteral("sublocality_level_1"), SublocalityLevel1},
//         {QStringLiteral("sublocality_level_2"), SublocalityLevel2},
//         {QStringLiteral("sublocality_level_3"), SublocalityLevel3},
//         {QStringLiteral("sublocality_level_4"), SublocalityLevel4},
//         {QStringLiteral("sublocality_level_5"), SublocalityLevel5},
//         {QStringLiteral("neighborhood"), Neighborhood},
//         {QStringLiteral("premise"), Premise},
//         {QStringLiteral("subpremise"), Subpremise},
//         {QStringLiteral("postal_code"), PostalCode},
//         {QStringLiteral("natural_feature"), NaturalFeature},
//         {QStringLiteral("airport"), Airport},
//         {QStringLiteral("park"), Park},
//         {QStringLiteral("point_of_interest"), PointOfInterest},
//         {QStringLiteral("floor"), Floor},
//         {QStringLiteral("establishment"), Establishment},
//         {QStringLiteral("parking"), Parking},
//         {QStringLiteral("post_box"), PostBox},
//         {QStringLiteral("postal_town"), PostalTown},
//         {QStringLiteral("room indicates"), RoomIndicates},
//         {QStringLiteral("street_number"), StreetNumber},
//         {QStringLiteral("bus_station"), BusStation},
//         {QStringLiteral("train_station"), TrainStation},
//         {QStringLiteral("transit_station"), TransitStation},
//     };
//     return m.value(key, GeoCodeTypeUnknown);
// }

// static QSet<QGeoCodeTypeGoogle> json2QGeoCodeTypeGoogle(const QJsonArray &types) {
//     QSet<QGeoCodeTypeGoogle> result;
//     for (int i=0; i<types.size(); ++i) {
//         result |= QString2QGeoCodeTypeGoogle(types[i].toString());
//     }
//     return result;
// }





QPlaceSearchReplyGoogle::QPlaceSearchReplyGoogle(QNetworkReply *reply, QObject *parent)
:   QPlaceSearchReply(parent), m_reply(reply)
{
    connect(m_reply, SIGNAL(finished()), this, SLOT(networkReplyFinished()));
    connect(m_reply, SIGNAL(error(QNetworkReply::NetworkError)),
            this, SLOT(networkReplyError(QNetworkReply::NetworkError)));

}

QPlaceSearchReplyGoogle::~QPlaceSearchReplyGoogle()
{
    if (m_reply)
        m_reply->deleteLater();
}

void QPlaceSearchReplyGoogle::abort()
{
    if (!m_reply)
        return;

    m_reply->abort();

    m_reply->deleteLater();
    m_reply = 0;
}

void QPlaceSearchReplyGoogle::networkReplyFinished()
{
    if (!m_reply)
        return;

    if (m_reply->error() != QNetworkReply::NoError)
        return;

    QJsonDocument document = QJsonDocument::fromJson(m_reply->readAll());
    QJsonObject object = document.object();

    qDebug() << document.toJson();

    // if (object.value(QStringLiteral("status")) != QStringLiteral("OK")) {
    //     QString error = object.value(QStringLiteral("status")).toString();
    //     qWarning() << m_reply->url() << "returned" << error;
    //     setError(QPlaceSearchReply::CommunicationError, error);
    //     m_reply->deleteLater();
    //     m_reply = 0;
    //     return;
    // }

    // QList<QGeoLocation> locations;
    // QJsonArray results = object.value(QStringLiteral("results")).toArray();
    // for (int i=0; i<results.size(); ++i) {
    //     if (!results[i].isObject())
    //         continue;

    //     QJsonObject geocode = results[i].toObject();

    //     QGeoAddress address;
    //     if (geocode.contains(QStringLiteral("formatted_address"))) {
    //         address.setText(geocode.value(QStringLiteral("formatted_address")).toString());
    //     }

    //     if (geocode.contains(QStringLiteral("address_components"))) {
    //         QJsonArray ac = geocode.value(QStringLiteral("address_components")).toArray();

    //         for (int j=0; j<ac.size(); ++j) {
    //             if (!ac[j].isObject())
    //                 continue;

    //             QJsonObject c = ac[j].toObject();
    //             if (!c.contains(QStringLiteral("types")))
    //                 continue;

    //             QSet<QGeoCodeTypeGoogle> types = json2QGeoCodeTypeGoogle(c[QStringLiteral("types")].toArray());
    //             QString long_name = c[QStringLiteral("long_name")].toString();
    //             QString short_name = c[QStringLiteral("short_name")].toString();
    //             if (types.contains(Country)) {
    //                 address.setCountry(long_name);
    //                 address.setCountryCode(short_name);
    //             } else if (types.contains(AdministrativeAreaLevel1)) {
    //                 address.setState(long_name);
    //             } else if (types.contains(AdministrativeAreaLevel2)) {
    //                 address.setCounty(long_name);
    //             } else if (types.contains(Locality)) {
    //                 address.setCity(long_name);
    //             } else if (types.contains(Sublocality)) {
    //                 address.setDistrict(long_name);
    //             } else if (types.contains(PostalCode)) {
    //                 address.setPostalCode(long_name);
    //             } else if (types.contains(StreetAddress) || types.contains(Route) || types.contains(Intersection)) {
    //                 address.setStreet(long_name);
    //             }
    //         }
    //     }

    //     QGeoCoordinate coordinate;
    //     QGeoRectangle boundingBox;
    //     if (geocode.contains(QStringLiteral("geometry"))) {
    //         QJsonObject geom = geocode.value(QStringLiteral("geometry")).toObject();
    //         if (geom.contains(QStringLiteral("location"))) {
    //             QJsonObject location = geom.value(QStringLiteral("location")).toObject();
    //             coordinate.setLatitude(location.value(QStringLiteral("lat")).toDouble());
    //             coordinate.setLongitude(location.value(QStringLiteral("lng")).toDouble());
    //         }
    //         if (geom.contains(QStringLiteral("bounds"))) {
    //             QJsonObject bounds = geom.value(QStringLiteral("bounds")).toObject();
    //             QJsonObject northeast = bounds.value(QStringLiteral("northeast")).toObject();
    //             QJsonObject southwest = bounds.value(QStringLiteral("southwest")).toObject();
    //             QGeoCoordinate topRight(northeast.value(QStringLiteral("lat")).toDouble(),
    //                                     northeast.value(QStringLiteral("lng")).toDouble());
    //             QGeoCoordinate bottomLeft(southwest.value(QStringLiteral("lat")).toDouble(),
    //                                       southwest.value(QStringLiteral("lng")).toDouble());
    //             boundingBox.setTopRight(topRight);
    //             boundingBox.setBottomLeft(bottomLeft);
    //         }
    //     }

    //     QGeoLocation location;
    //     location.setAddress(address);
    //     location.setCoordinate(coordinate);
    //     location.setBoundingBox(boundingBox);

    //     locations << location;
    // }

    // setLocations(locations);
    setFinished(true);

    m_reply->deleteLater();
    m_reply = 0;
}

void QPlaceSearchReplyGoogle::networkReplyError(QNetworkReply::NetworkError error)
{
    Q_UNUSED(error)

    if (!m_reply)
        return;

    setError(QPlaceSearchReply::CommunicationError, m_reply->errorString());

    m_reply->deleteLater();
    m_reply = 0;
}

QT_END_NAMESPACE
