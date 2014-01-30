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

#include "qgeoroutingmanagerengineosm.h"
#include "qgeoroutereplyosm.h"

#include <QtCore/QUrlQuery>

#include <QtCore/QDebug>

// https://developers.google.com/maps/documentation/directions/

// import json
// from urllib.parse import urlencode
// from urllib.request import urlopen

// payload = {'origin': 'Guildford, Surrey',
//            'destination': 'Embankment, London',
//            'sensor': 'false',
//            'units': 'metric' }
// url = 'http://maps.googleapis.com/maps/api/directions/json?' + urlencode(payload)
// content = urlopen(url).read()
// data = json.loads(content.decode('utf-8'))
// print(data)

static QString geoCoordinate2Query(const QGeoCoordinate &c) {
    return QStringLiteral("%1,%2").arg(c.latitude()).arg(c.longitude());
}

QGeoRoutingManagerEngineOsm::QGeoRoutingManagerEngineOsm(const QVariantMap &parameters,
                                                         QGeoServiceProvider::Error *error,
                                                         QString *errorString)
:   QGeoRoutingManagerEngine(parameters), m_networkManager(new QNetworkAccessManager(this))
{
    if (parameters.contains(QStringLiteral("useragent")))
        m_userAgent = parameters.value(QStringLiteral("useragent")).toString().toLatin1();
    else
        m_userAgent = "Qt Location based application";

    *error = QGeoServiceProvider::NoError;
    errorString->clear();
}

QGeoRoutingManagerEngineOsm::~QGeoRoutingManagerEngineOsm()
{
}

QGeoRouteReply* QGeoRoutingManagerEngineOsm::calculateRoute(const QGeoRouteRequest &request)
{
    QNetworkRequest networkRequest;
    networkRequest.setRawHeader("User-Agent", m_userAgent);

    QUrl url(QLatin1String("http://maps.googleapis.com/maps/api/directions/json"));
    QUrlQuery query;

    query.addQueryItem(QStringLiteral("sensor"), QStringLiteral("false"));
    query.addQueryItem(QStringLiteral("language"), locale().name().left(2));

    switch(request.travelModes()) {
    case QGeoRouteRequest::CarTravel:
    case QGeoRouteRequest::TruckTravel:
        query.addQueryItem(QStringLiteral("mode"), QStringLiteral("driving"));
        break;
    case QGeoRouteRequest::PedestrianTravel:
        query.addQueryItem(QStringLiteral("mode"), QStringLiteral("walking"));
        break;
    case QGeoRouteRequest::BicycleTravel:
        query.addQueryItem(QStringLiteral("mode"), QStringLiteral("bicycling"));
        break;
    case QGeoRouteRequest::PublicTransitTravel:
        query.addQueryItem(QStringLiteral("mode"), QStringLiteral("transit"));
        // TODO arrival_time, departure_time
        break;
    }
    
    if (request.numberAlternativeRoutes() > 0) {
        query.addQueryItem(QStringLiteral("alternatives"), request.numberAlternativeRoutes()==1 ?
                           QStringLiteral("false") :
                           QStringLiteral("true"));
    }
    query.addQueryItem(QStringLiteral("units"), QStringLiteral("metric")); // TODO from locale
    // TODO: RouteOptimization

    const QList<QGeoCoordinate> &waypoints = request.waypoints();
    if (waypoints.size() > 0) {
        query.addQueryItem(QStringLiteral("origin"), geoCoordinate2Query(waypoints[0]));
        query.addQueryItem(QStringLiteral("destination"), geoCoordinate2Query(waypoints[waypoints.size()-1]));
    }

    if (waypoints.size() > 2) {
        QStringList points;
        for (int i=1; i<waypoints.size()-1; ++i) {
            points << geoCoordinate2Query(waypoints[i]);
        }
        query.addQueryItem(QStringLiteral("waypoints"), points.join('|'));
    }
    

    url.setQuery(query);
    networkRequest.setUrl(url);
    qDebug() << url;

    QNetworkReply *reply = m_networkManager->get(networkRequest);

    QGeoRouteReplyOsm *routeReply = new QGeoRouteReplyOsm(reply, request, this);

    connect(routeReply, SIGNAL(finished()), this, SLOT(replyFinished()));
    connect(routeReply, SIGNAL(error(QGeoRouteReply::Error, QString)),
            this, SLOT(replyError(QGeoRouteReply::Error,QString)));

    return routeReply;
}

void QGeoRoutingManagerEngineOsm::replyFinished()
{
    QGeoRouteReply *reply = qobject_cast<QGeoRouteReply *>(sender());
    if (reply)
        emit finished(reply);
}

void QGeoRoutingManagerEngineOsm::replyError(QGeoRouteReply::Error errorCode,
                                             const QString &errorString)
{
    QGeoRouteReply *reply = qobject_cast<QGeoRouteReply *>(sender());
    if (reply)
        emit error(reply, errorCode, errorString);
}
