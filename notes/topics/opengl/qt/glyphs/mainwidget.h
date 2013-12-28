#ifndef MAINWIDGET_H
#define MAINWIDGET_H

#include <QGLWidget>
#include <QGLFunctions>
#include <QMatrix4x4>
#include <QQuaternion>
#include <QVector2D>
#include <QBasicTimer>
#include <QGLShaderProgram>

struct viewer_t {
    QVector3D pos, dir, up;
    QVector3D lookAt() const { return pos + dir; }
    QVector3D side() const { return QVector3D::crossProduct(dir, up).normalized(); }
};

class GeometryEngine;

class MainWidget : public QGLWidget, protected QGLFunctions
{
    Q_OBJECT

public:
    explicit MainWidget(QWidget *parent = 0);
    ~MainWidget();

protected:
    void keyPressEvent(QKeyEvent *e);
    void mouseMoveEvent(QMouseEvent *e);
    void mousePressEvent(QMouseEvent *e);
    void mouseReleaseEvent(QMouseEvent *e);
    void timerEvent(QTimerEvent *e);

    void initializeGL();
    void resizeGL(int w, int h);
    void paintGL();

    void initShaders();
    void initTextures();
    void initScene();

private:
    bool repaintGL;
    QBasicTimer timer;
    QGLShaderProgram program;

    GLuint glyph_texture, random_texture;

    QMatrix4x4 projection;

    QVector2D mousePressPosition;
    QVector3D rotationAxis;
    qreal angularSpeed;
    QQuaternion rotation;

    viewer_t viewer;

    GLuint vboIds[4];
    QPoint mouseLastPosition;
};

#endif // MAINWIDGET_H
