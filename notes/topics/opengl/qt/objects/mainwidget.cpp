/****************************************************************************
**
** Copyright (C) 2013 Digia Plc and/or its subsidiary(-ies).
** Contact: http://www.qt-project.org/legal
**
** This file is part of the QtCore module of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:BSD$
** You may use this file under the terms of the BSD license as follows:
**
** "Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are
** met:
**   * Redistributions of source code must retain the above copyright
**     notice, this list of conditions and the following disclaimer.
**   * Redistributions in binary form must reproduce the above copyright
**     notice, this list of conditions and the following disclaimer in
**     the documentation and/or other materials provided with the
**     distribution.
**   * Neither the name of Digia Plc and its Subsidiary(-ies) nor the names
**     of its contributors may be used to endorse or promote products derived
**     from this software without specific prior written permission.
**
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
** "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
** LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
** A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
** OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
** SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
** LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
** DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
** THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
** OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
**
** $QT_END_LICENSE$
**
****************************************************************************/

#include "mainwidget.h"

#include <QMouseEvent>
#include <QBox3D>

#include <math.h>
#include <locale.h>

MainWidget::MainWidget(QWidget *parent) :
    QGLWidget(parent),
    angularSpeed(0),
    viewer{QVector3D(0, 0, 10), QVector3D(0, 0, 0), QVector3D(0, 1, 0)}
{
    setMouseTracking(true);
}

MainWidget::~MainWidget()
{
    deleteTexture(texture_day);
    deleteTexture(texture_night);
    glDeleteBuffers(3, vboIds);
}

//! [0]
void MainWidget::keyPressEvent(QKeyEvent *e)
{
    const qreal acc = 10.;
    const qreal step = 0.2;
    
    if (e->key() == Qt::Key_Left) {
        rotationAxis = (rotationAxis * angularSpeed + QVector3D(0.0, -1.0, 0.0) * acc).normalized();
        angularSpeed += acc;
    } else if (e->key() == Qt::Key_Right) {
        rotationAxis = (rotationAxis * angularSpeed + QVector3D(0.0, +1.0, 0.0) * acc).normalized();
        angularSpeed += acc;
    } else if (e->key() == Qt::Key_Up) {
        rotationAxis = (rotationAxis * angularSpeed + QVector3D(-1.0, 0.0, 0.0) * acc).normalized();
        angularSpeed += acc;
    } else if (e->key() == Qt::Key_Down) {
        rotationAxis = (rotationAxis * angularSpeed + QVector3D(+1.0, 0.0, 0.0) * acc).normalized();
        angularSpeed += acc;
    } else if (e->key() == Qt::Key_W) {
        viewer[0] += step*viewer[1];
        updateGL();
    } else if (e->key() == Qt::Key_S) {
        viewer[0] -= step*viewer[1];
        updateGL();
    } else if (e->key() == Qt::Key_A) {
        QVector3D dir = QVector3D::crossProduct(viewer[1], viewer[2]).normalized();
        viewer[0] -= step*dir;
        updateGL();
    } else if (e->key() == Qt::Key_D) {
        QVector3D dir = QVector3D::crossProduct(viewer[1], viewer[2]).normalized();
        viewer[0] += step*dir;
        updateGL();
    }
}

void MainWidget::mouseMoveEvent(QMouseEvent *e) {
    qDebug() << e->localPos().x() <<  e->localPos().y() << rect().width() << rect().height();
}

void MainWidget::mousePressEvent(QMouseEvent *e)
{
    // Save mouse press position
    mousePressPosition = QVector2D(e->localPos());
}

void MainWidget::mouseReleaseEvent(QMouseEvent *e)
{
    // Mouse release position - mouse press position
    QVector2D diff = QVector2D(e->localPos()) - mousePressPosition;

    // Rotation axis is perpendicular to the mouse position difference
    // vector
    QVector3D n = QVector3D(diff.y(), diff.x(), 0.0).normalized();

    // Accelerate angular speed relative to the length of the mouse sweep
    qreal acc = diff.length() / 100.0;

    // Calculate new rotation axis as weighted sum
    rotationAxis = (rotationAxis * angularSpeed + n * acc).normalized();

    // Increase angular speed
    angularSpeed += acc;
}
//! [0]

//! [1]
void MainWidget::timerEvent(QTimerEvent *)
{
    // Decrease angular speed (friction)
    angularSpeed *= 0.99;

    // Stop rotation when speed goes below threshold
    if (angularSpeed < 0.01) {
        angularSpeed = 0.0;
    } else {
        // Update rotation
        rotation = QQuaternion::fromAxisAndAngle(rotationAxis, angularSpeed) * rotation;

        // Update scene
        updateGL();
    }
}
//! [1]

void MainWidget::initializeGL()
{
    initializeGLFunctions();
    qglClearColor(Qt::black);
    initShaders();
    initTextures();
    initScene();

//! [2]
    // Enable depth buffer
    glEnable(GL_DEPTH_TEST);

    // Enable back face culling
    glEnable(GL_CULL_FACE);
//! [2]

    // Use QBasicTimer because its faster than QTimer
    timer.start(12, this);
}

//! [3]
void MainWidget::initShaders()
{
    // Override system locale until shaders are compiled
    setlocale(LC_NUMERIC, "C");

    // Compile vertex shader
    if (!program.addShaderFromSourceFile(QGLShader::Vertex, QString::fromLatin1(":/%1.v.glsl").arg(shader_)))
        close();

    // Compile fragment shader
    if (!program.addShaderFromSourceFile(QGLShader::Fragment, QString::fromLatin1(":/%1.f.glsl").arg(shader_)))
        qDebug() << "Shader compilation failed " << program.log();

    // Link shader pipeline
    if (!program.link())
        close();

    // Bind shader pipeline for use
    if (!program.bind())
        close();

    // Restore system locale
    setlocale(LC_ALL, "");
}
//! [3]

//! [4]
void MainWidget::initTextures()
{
    // Load textures
    glEnable(GL_TEXTURE_2D);
    
    texture_day = bindTexture(QImage(":/earth_day.jpg"));

    // Set nearest filtering mode for texture minification
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

    // Set bilinear filtering mode for texture magnification
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    // Wrap texture coordinates by repeating
    // f.ex. texture coordinate (1.1, 1.2) is same as (0.1, 0.2)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

    // 
    texture_night = bindTexture(QImage(":/earth_night.jpg"));

    // Set nearest filtering mode for texture minification
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

    // Set bilinear filtering mode for texture magnification
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    // Wrap texture coordinates by repeating
    // f.ex. texture coordinate (1.1, 1.2) is same as (0.1, 0.2)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
}
//! [4]

void MainWidget::initScene()
{
    QFile file(QString::fromLatin1(":/") + object_);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text) | file.atEnd()) {
        qWarning() << "Object" << (QString::fromLatin1(":/") + object_) << "is not found";
        return;
    }

    QBox3D objectExtent;
    QVector<GLushort> elements;
    QVector<QVector3D> vertices;
    while (!file.atEnd()) {
        QByteArray line = file.readLine();
        if (line[0] == 'v') {
            QTextStream stream(&line);
            char dummy;
            qreal x, y, z;
            stream >> dummy >> x >> y >> z;
            QVector3D vertex(x, y, z);
            vertices.append(vertex);
            objectExtent.unite(vertex);
        } else if (line[0] == 'f') {
            QTextStream stream(&line);
            char dummy;
            GLushort a,b,c;
            stream >> dummy >> a >> b >> c;
            a--; b--; c--;
            elements << a << b << c;
        }
    }

    // set viewer position and orientation
    QVector3D objectSize = objectExtent.size();
    qDebug() << objectExtent << objectSize << objectExtent.center();
    viewer[0] = objectExtent.center() + QVector3D(0, 0, objectSize.x() + objectSize.y() + objectSize.z());
    viewer[1] = (objectExtent.center() - viewer[0]).normalized();
    viewer[2] = QVector3D(0, 1, 0);
    qDebug() << "Viewer eye =" << viewer[0] << "center =" << viewer[1] << "up =" << viewer[2];

    QVector<QVector3D> normals(vertices.size());
    for (int i = 0; i < elements.size(); i+=3) {
        GLushort ia = elements[i];
        GLushort ib = elements[i+1];
        GLushort ic = elements[i+2];
        QVector3D normal = QVector3D::normal(vertices[ia], vertices[ib], vertices[ic]);

        normals[ia] += normal;
        normals[ib] += normal;
        normals[ic] += normal;
    }
    for (int i = 0; i < normals.size(); ++i)
        normals[i].normalize();

    glGenBuffers(3, vboIds);

    glBindBuffer(GL_ARRAY_BUFFER, vboIds[0]);
    glBufferData(GL_ARRAY_BUFFER,  vertices.size() * sizeof(QVector3D), vertices.constData(), GL_STATIC_DRAW);

    glBindBuffer(GL_ARRAY_BUFFER, vboIds[1]);
    glBufferData(GL_ARRAY_BUFFER,  normals.size() * sizeof(QVector3D), normals.constData(), GL_STATIC_DRAW);

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboIds[2]);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER,  elements.size() * sizeof(GLushort), elements.constData(), GL_STATIC_DRAW);
}


//! [5]
void MainWidget::resizeGL(int w, int h)
{
    // Set OpenGL viewport to cover whole widget
    glViewport(0, 0, w, h);

    // Calculate aspect ratio
    qreal aspect = qreal(w) / qreal(h ? h : 1);

    // Set near plane to 3.0, far plane to 7.0, field of view 45 degrees
    const qreal zNear = 0.1, zFar = 1000.0, fov = 45.0;

    // Reset projection
    projection.setToIdentity();

    // Set perspective projection
    projection.perspective(fov, aspect, zNear, zFar);
}
//! [5]

void MainWidget::paintGL()
{
    // Clear color and depth buffer
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // Set model view projection transformations
    QMatrix4x4 model;
    model.translate(0.0, 0.0, 0.0);
    model.rotate(rotation);
    program.setUniformValue("m", model);

    QMatrix4x4 view;
    view.lookAt(viewer[0], viewer[0] + viewer[1], viewer[2]);
    program.setUniformValue("v", view);

    program.setUniformValue("p", projection);

    program.setUniformValue("m_3x3_inv_transp", model.normalMatrix());
    program.setUniformValue("v_inv", view.inverted());

    // Use textures
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, texture_day);
    program.setUniformValue("texture_day", 0);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, texture_night);
    program.setUniformValue("texture_night", 1);

    int attribute_v_coord = program.attributeLocation("v_coord");
    glBindBuffer(GL_ARRAY_BUFFER, vboIds[0]);
    glEnableVertexAttribArray(attribute_v_coord);
    glVertexAttribPointer(attribute_v_coord,
                          3,                  // number of elements per vertex, here (x,y,z)
                          GL_FLOAT,           // the type of each element
                          GL_FALSE,           // take our values as-is
                          0,                  // no extra data between each position
                          0                   // offset of first element
                          );

    int attribute_v_normal = program.attributeLocation("v_normal");
    glBindBuffer(GL_ARRAY_BUFFER, vboIds[1]);
    glEnableVertexAttribArray(attribute_v_normal);
    glVertexAttribPointer(attribute_v_normal,
                          3,                  // number of elements per vertex, here (x,y,z)
                          GL_FLOAT,           // the type of each element
                          GL_FALSE,           // take our values as-is
                          0,                  // no extra data between each position
                          0                   // offset of first element
                          );

    // Draw geometry
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboIds[2]);
    int size;  glGetBufferParameteriv(GL_ELEMENT_ARRAY_BUFFER, GL_BUFFER_SIZE, &size);
    glDrawElements(GL_TRIANGLES, size/sizeof(GLushort), GL_UNSIGNED_SHORT, 0);

    glDisableVertexAttribArray(attribute_v_coord);
    glDisableVertexAttribArray(attribute_v_normal);
}
