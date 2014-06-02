// -*- compile-command: "make" -*-
#include <QGuiApplication>
#include <QtQuick/QQuickView>
#include <QtQuick/QQuickItem>
#include <private/qquickloader_p.h>
#include <private/qquickanchors_p.h>
#include <private/qquickanchors_p_p.h>
#include <QQmlContext>
#include <QtQml>

#include "diffhelper.h"

QString properties(QQuickItem *item, bool linebreak)
{
    const QMetaObject *meta = item->metaObject();

    QHash<QString, QVariant> list;
    for (int i = 0; i < meta->propertyCount(); i++)
    {
        QMetaProperty property = meta->property(i);
        const char* name = property.name();
        QVariant value = item->property(name);
        list[name] = value;
    }

    QString out;
    QHashIterator<QString, QVariant> i(list);
    while (i.hasNext()) {
        i.next();
        if (!out.isEmpty())
        {
            out += ", ";
            if (linebreak) out += "\n";
        }
        out.append(i.key());
        out.append(" (");
        out.append(i.value().typeName());
        out.append(") : ");
        if (qvariant_cast<QQuickAnchors*>(i.value())) qDebug() << "BINGO!!!!";
        qDebug() << "BINGO!!!! QQuickAnchorLine" <<qvariant_cast<QQuickAnchorLine>(i.value()).anchorLine;

        out.append(i.value().toString());
    }
    return out;
}

int main(int argc, char *argv[])
{
    qmlRegisterType<DiffHelper>("Helpers", 1, 0, "DiffHelper");

    QGuiApplication app(argc, argv);

    QQuickView *view = new QQuickView;

    QObject::connect((QObject*)view->engine(), SIGNAL(quit()), qApp, SLOT(quit()));

    // Model rootModel(QStringList() << "Alex" << "Ford");
    // view->rootContext()->setContextProperty(QStringLiteral("rootModel"), &rootModel);
    // Adaptor adaptor;
    // view->rootContext()->setContextProperty(QStringLiteral("adaptor"), &adaptor);
    view->setSource(QUrl("main.qml"));
    view->show();

    // qDebug() << view->rootContext() << view->rootContext()->findChild<QObject*>("a");
    // QQuickItem *root = view->rootObject();
    // qDebug() << root << root->findChild<QObject*>("skinA");
    // QQuickLoader* loaderA = root->findChild<QQuickLoader*>("skinA");
    // QQuickItem* skinA = qobject_cast<QQuickItem*>(loaderA->item());
    // qDebug() << loaderA << loaderA->item();

    // foreach (const QObject* c, skinA->children()) {
    //     qDebug() << c << c->metaObject();
    // }

    // qDebug() << skinA->childItems();
    // qDebug() << properties(skinA, true);

    // QFile file("main.qml");
    // if (file.open(QIODevice::ReadOnly | QIODevice::Text)) {
    //     qDebug() << QString(QCryptographicHash::hash(file.readAll(), QCryptographicHash::Md5).toHex());
    // }

    return app.exec();
}
