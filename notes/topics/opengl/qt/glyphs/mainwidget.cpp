#include "mainwidget.h"

#include <QMouseEvent>
#include <QBox3D>

#include <math.h>
#include <locale.h>

MainWidget::MainWidget(QWidget *parent) :
    QGLWidget(parent),
    angularSpeed(0),
    viewer{QVector3D(0, 0, 10), QVector3D(0, 0, 0), QVector3D(0, 1, 0)},
    mouseLastX(rect().width()/2), mouseLastY(rect().height()/2)
{
    setMouseTracking(true);
}

MainWidget::~MainWidget()
{
    deleteTexture(texture);
    glDeleteBuffers(3, vboIds);
}

//! [0]
void MainWidget::keyPressEvent(QKeyEvent *e)
{
    const qreal acc = 10.;
    const qreal step = 0.2;
    
    if (e->key() == Qt::Key_Escape) {
        close();
    } else if (e->key() == Qt::Key_Left) {
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
    float sens = 1e-3;
    if (mouseLastX != -1 || mouseLastY != -1) {
        // float x = 2 * e->localPos().x() / rect().width() - 1.f;
        // float y = 1.f - 2 * e->localPos().y() / rect().height();
        QVector3D dirx = QVector3D::crossProduct(viewer[1], viewer[2]).normalized();

        viewer[1] = viewer[1] + sens*(e->localPos().x()-mouseLastX)*dirx - sens*(e->localPos().y()-mouseLastY)*viewer[2];
        viewer[1] = viewer[1].normalized();
        updateGL();
    }
    mouseLastX = e->localPos().x();
    mouseLastY = e->localPos().y();
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
    if (!program.addShaderFromSourceFile(QGLShader::Vertex, QString::fromLatin1("glyphs.vsl")))
        close();

    // Compile fragment shader
    if (!program.addShaderFromSourceFile(QGLShader::Fragment, QString::fromLatin1("glyphs.fsl")))
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
    
    texture = bindTexture(QImage("10fig03.jpg"));

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
    QFile file(QString::fromLatin1("cube.obj"));
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text) | file.atEnd()) {
        qWarning() << "Object" << (QString::fromLatin1("cube.obj")) << "is not found";
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
    viewer[0] = objectExtent.center() + QVector3D(0, 0, objectSize.x() + objectSize.y() + objectSize.z());
    viewer[1] = (objectExtent.center() - viewer[0]).normalized();
    viewer[2] = QVector3D(0, 1, 0);
    qDebug() << "Viewer eye =" << viewer[0] << "center =" << viewer[1] << "up =" << viewer[2];

    bool average_normals = false;
    QVector<QVector3D> normals(vertices.size());
    for (int i = 0; i < elements.size(); i+=3) {
        GLushort ia = elements[i];
        GLushort ib = elements[i+1];
        GLushort ic = elements[i+2];
        QVector3D normal = QVector3D::normal(vertices[ia], vertices[ib], vertices[ic]);

        if (average_normals) {
            normals[ia] += normal;
            normals[ib] += normal;
            normals[ic] += normal;
        } else {
            normals[ia] = normal;
            normals[ib] = normal;
            normals[ic] = normal;
        }
    }
    for (int i = 0; i < normals.size(); ++i)
        normals[i] = normals[i].normalized();

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

    program.setUniformValue("SpecularContribution", (GLfloat)0.2);
    program.setUniformValue("LightPosition", QVector3D(4, 14, 4));
    program.setUniformValue("ScaleFactor", (GLfloat)10.);
    program.setUniformValue("ModelColor", QVector4D(1, 1, 1, 1));
    program.setUniformValue("GlyphTex", 0);
    program.setUniformValue("RandomTex", 1);
    program.setUniformValue("ColAdjust", (GLfloat)0.75);
    program.setUniformValue("Percentage", (GLfloat)1.);
    program.setUniformValue("SamplesPerCell", (GLfloat)1.);
    program.setUniformValue("R01", (GLfloat)0.29);
    program.setUniformValue("RandomScale", false);
    program.setUniformValue("RandomRotate", false);

    
    // Set model view projection transformations
    QMatrix4x4 model;
    model.translate(0.0, 0.0, 0.0);
    model.rotate(rotation);
    program.setUniformValue("MVMatrix", model);

    QMatrix4x4 view;
    view.lookAt(viewer[0], viewer[0] + viewer[1], viewer[2]);

    program.setUniformValue("MVPMatrix", projection*view*model);
    program.setUniformValue("v_inv", view.inverted());
    program.setUniformValue("NormalMatrix", model.normalMatrix());

    // Use textures
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, texture);
    program.setUniformValue("GlyphTex", 0);

    int attribute_v_coord = program.attributeLocation("MCVertex");
    glBindBuffer(GL_ARRAY_BUFFER, vboIds[0]);
    glEnableVertexAttribArray(attribute_v_coord);
    glVertexAttribPointer(attribute_v_coord,
                          3,                  // number of elements per vertex, here (x,y,z)
                          GL_FLOAT,           // the type of each element
                          GL_FALSE,           // take our values as-is
                          0,                  // no extra data between each position
                          0                   // offset of first element
                          );

    int attribute_v_normal = program.attributeLocation("MCNormal");
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
