#include "mainwidget.h"

#include <QMouseEvent>
#include <QBox3D>
#include <QGLAbstractScene>

#include <math.h>
#include <locale.h>
#include <stdlib.h>

MainWidget::MainWidget(QWidget *parent) :
    QGLWidget(parent),
    angularSpeed(0),
    viewer{QVector3D(0, 2, 6), QVector3D(0, -0.3, -1), QVector3D(0, 1, 0)},
    mouseLastX(rect().width()/2), mouseLastY(rect().height()/2)
{
    setMouseTracking(true);
}

MainWidget::~MainWidget()
{
    deleteTexture(glyph_texture);
    deleteTexture(random_texture);
    glDeleteBuffers(4, vboIds);
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
    return;
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
    if (!program.addShaderFromSourceFile(QGLShader::Vertex, QString::fromLatin1("glyphs.v.glsl")))
        exit(1);

    // Compile fragment shader
    if (!program.addShaderFromSourceFile(QGLShader::Fragment, QString::fromLatin1("glyphs.f.glsl")))
        exit(1);

    // Link shader pipeline
    if (!program.link())
        exit(1);

    // Bind shader pipeline for use
    if (!program.bind())
        exit(1);

    // Restore system locale
    setlocale(LC_ALL, "");
}
//! [3]

//! [4]
void MainWidget::initTextures()
{
    // Load textures
    glEnable(GL_TEXTURE_2D);
    
    glyph_texture = bindTexture(QImage("10fig03.jpg"));

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

    // Set nearest filtering mode for texture minification
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

    // Set bilinear filtering mode for texture magnification
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    // Random texture
    QByteArray random_data(100 * 100 * 4, 0);
    srand(time(0));
    srand(42);
    for (int i=0; i < random_data.size(); ++i)
        random_data[i] = rand() % (1<<sizeof(random_data[0]));
    QImage random_image((unsigned char*)random_data.constData(), 100, 100, QImage::Format_ARGB32);
    random_texture = bindTexture(random_image);
}
//! [4]

void MainWidget::initScene()
{
    // Load the 3d model from the file
    QGLAbstractScene *scene = QGLAbstractScene::loadScene("cube.obj");
    QGLSceneNode *cube = qobject_cast<QGLSceneNode*>(scene->object("Cube"));
    if (!cube)
        return;
    
    if (cube->geometry().isEmpty()) {
        foreach(QGLSceneNode* o, cube->children()) {
            if (!o->geometry().isEmpty()) {
                cube = o;
                break;
            }
        }
    }
    if (cube->geometry().isEmpty())
        return;

    // qDebug() << cube->geometry().vertices() << cube->geometry().normals() << cube->geometry().texCoords() << cube->geometry().indices();

    glGenBuffers(4, vboIds);

    glBindBuffer(GL_ARRAY_BUFFER, vboIds[0]);
    glBufferData(GL_ARRAY_BUFFER, cube->geometry().vertices().size() * sizeof(QVector3D), cube->geometry().vertices().constData(), GL_STATIC_DRAW);

    glBindBuffer(GL_ARRAY_BUFFER, vboIds[1]);
    glBufferData(GL_ARRAY_BUFFER, cube->geometry().normals().size() * sizeof(QVector3D), cube->geometry().normals().constData(), GL_STATIC_DRAW);

    glBindBuffer(GL_ARRAY_BUFFER, vboIds[2]);
    glBufferData(GL_ARRAY_BUFFER, cube->geometry().texCoords().size() * sizeof(QVector2D), cube->geometry().texCoords().constData(), GL_STATIC_DRAW);
    
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboIds[3]);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, cube->geometry().indices().size() * sizeof(GLuint), cube->geometry().indices().constData(), GL_STATIC_DRAW);
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
    program.setUniformValue("LightPosition", QVector3D(0, 2, 4));
    program.setUniformValue("ScaleFactor", (GLfloat)10.);
    program.setUniformValue("ModelColor", QVector4D(1, 1, 1, 1));
    program.setUniformValue("ColAdjust", (GLfloat)0.42);
    program.setUniformValue("Percentage", (GLfloat).75);
    program.setUniformValue("SamplesPerCell", (GLfloat)1.);
    program.setUniformValue("RO1", (GLfloat)0.29);
    program.setUniformValue("RandomScale", true);
    program.setUniformValue("RandomRotate", true);

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
    glBindTexture(GL_TEXTURE_2D, glyph_texture);
    program.setUniformValue("GlyphTex", 0);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, random_texture);
    program.setUniformValue("RandomTex", 1);

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

    int attribute_v_texCoord = program.attributeLocation("MCTexCoord");
    glBindBuffer(GL_ARRAY_BUFFER, vboIds[2]);
    glEnableVertexAttribArray(attribute_v_texCoord);
    glVertexAttribPointer(attribute_v_texCoord,
                          2,                  // number of elements per vertex, here (x,y,z)
                          GL_FLOAT,           // the type of each element
                          GL_FALSE,           // take our values as-is
                          0,                  // no extra data between each position
                          0                   // offset of first element
                          );

    // Draw geometry
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboIds[3]);
    int size;  glGetBufferParameteriv(GL_ELEMENT_ARRAY_BUFFER, GL_BUFFER_SIZE, &size);
    glDrawElements(GL_TRIANGLES, size/sizeof(GLuint), GL_UNSIGNED_INT, 0);

    glDisableVertexAttribArray(attribute_v_coord);
    glDisableVertexAttribArray(attribute_v_normal);
    glDisableVertexAttribArray(attribute_v_texCoord);
}
