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

#include "qgeotiledmappingmanagerenginegoogle.h"
#include "qgeotilefetchergoogle.h"

#include <QtLocation/private/qgeocameracapabilities_p.h>
#include <QtLocation/private/qgeomaptype_p.h>
#include <QtLocation/private/qgeotiledmapdata_p.h>

QT_BEGIN_NAMESPACE

QGeoTiledMappingManagerEngineGoogle::QGeoTiledMappingManagerEngineGoogle(const QVariantMap &parameters, QGeoServiceProvider::Error *error, QString *errorString)
:   QGeoTiledMappingManagerEngine()
{
    QGeoCameraCapabilities cameraCaps;
    cameraCaps.setMinimumZoomLevel(0.0);
    cameraCaps.setMaximumZoomLevel(22.0);
    setCameraCapabilities(cameraCaps);

    setTileSize(QSize(256, 256));

    QList<QGeoMapType> mapTypes;
    mapTypes << QGeoMapType(QGeoMapType::StreetMap, tr("Street Map"), tr("Google street map"), false, 1);
    mapTypes << QGeoMapType(QGeoMapType::SatelliteMapDay, tr("Satellite Map"), tr("Google satellite map"), false, 2);
    mapTypes << QGeoMapType(QGeoMapType::TerrainMap, tr("Terrain Map"), tr("Google terrain map"), false, 3);
    mapTypes << QGeoMapType(QGeoMapType::HybridMap, tr("Hybrid Map"), tr("Google hybrid map"), false, 4);
    setSupportedMapTypes(mapTypes);

    QGeoTileFetcherGoogle *tileFetcher = new QGeoTileFetcherGoogle(this);
    if (parameters.contains(QStringLiteral("useragent"))) {
        const QByteArray ua = parameters.value(QStringLiteral("useragent")).toString().toLatin1();
        tileFetcher->setUserAgent(ua);
    }

    setTileFetcher(tileFetcher);

    *error = QGeoServiceProvider::NoError;
    errorString->clear();
}

QGeoTiledMappingManagerEngineGoogle::~QGeoTiledMappingManagerEngineGoogle()
{
}

QGeoMapData *QGeoTiledMappingManagerEngineGoogle::createMapData()
{
    return new QGeoTiledMapData(this, 0);
}

QT_END_NAMESPACE