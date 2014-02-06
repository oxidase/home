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

#include "qplacemanagerenginegoogle.h"
#include "qplacesearchreplygoogle.h"

#include <QtCore/QVariantMap>
#include <QtCore/QUrl>
#include <QtCore/QUrlQuery>
#include <QtCore/QLocale>
#include <QtNetwork/QNetworkAccessManager>
#include <QtNetwork/QNetworkRequest>
#include <QtPositioning/QGeoCoordinate>
#include <QtPositioning/QGeoAddress>
#include <QtPositioning/QGeoShape>
#include <QtPositioning/QGeoRectangle>

// https://developers.google.com/places/documentation/search

QT_BEGIN_NAMESPACE

QPlaceManagerEngineGoogle::QPlaceManagerEngineGoogle(const QVariantMap &parameters,
                                                       QGeoServiceProvider::Error *error,
                                                       QString *errorString)
:   QPlaceManagerEngine(parameters), m_networkManager(new QNetworkAccessManager(this))
{
    if (parameters.contains(QStringLiteral("useragent")))
        m_userAgent = parameters.value(QStringLiteral("useragent")).toString().toLatin1();
    else
        m_userAgent = "Qt Location based application";

    *error = QGeoServiceProvider::NoError;
    errorString->clear();
}

QPlaceManagerEngineGoogle::~QPlaceManagerEngineGoogle()
{
}

QPlaceSearchReply *QPlaceManagerEngineGoogle::search(const QPlaceSearchRequest &query)
{
    QNetworkRequest request;
    request.setRawHeader("User-Agent", m_userAgent);

    QUrl url(QStringLiteral("https://maps.googleapis.com/maps/api/place/nearbysearch/json"));
    QUrlQuery urlQuery;
    // query.addQueryItem(QStringLiteral("sensor"), QStringLiteral("false"));
    // query.addQueryItem(QStringLiteral("language"), locale().name().left(2));
    // query.addQueryItem(QStringLiteral("address"), address);
    // if (bounds.type() == QGeoShape::RectangleType) {
    //     query.addQueryItem(QStringLiteral("bounds"), boundingBoxToLtrb(bounds));
    // }

    url.setQuery(urlQuery);
    request.setUrl(url);
    qDebug() << url;

    QNetworkReply *reply = m_networkManager->get(request);
    reply->setParent(0);

    QPlaceSearchReply *placeSearchReply = new QPlaceSearchReplyGoogle(reply);

    connect(placeSearchReply, SIGNAL(finished()), this, SLOT(replyFinished()));
    connect(placeSearchReply, SIGNAL(error(QGeoCodeReply::Error,QString)),
            this, SLOT(replyError(QGeoCodeReply::Error,QString)));

    return placeSearchReply;
}


void QPlaceManagerEngineGoogle::replyFinished()
{
    QPlaceSearchReply *reply = qobject_cast<QPlaceSearchReply *>(sender());
    if (reply)
        emit finished(reply);
}

void QPlaceManagerEngineGoogle::replyError(QPlaceSearchReply::Error errorCode, const QString &errorString)
{
    QPlaceSearchReply *reply = qobject_cast<QPlaceSearchReply *>(sender());
    if (reply)
        emit error(reply, errorCode, errorString);
}

QT_END_NAMESPACE
