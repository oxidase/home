// -*- compile-command: "make" -*-
#include <QDebug>
#include <QFile>
#include <QCryptographicHash>

#include <private/qquickanchors_p.h>
#include <private/qquickanchors_p_p.h>
#include <private/qquickpropertychanges_p.h>

#include "diffhelper.h"

DiffHelper::DiffHelper(QObject* parent) :
    QObject(parent)
{
}

DiffHelper::~DiffHelper()
{
}

QString DiffHelper::md5(const QString& fileName) const
{
    QFile file(fileName);
    if (file.open(QIODevice::ReadOnly | QIODevice::Text)) {
        return QCryptographicHash::hash(file.readAll(), QCryptographicHash::Md5).toHex();
    }
    return QString("invalid");
}

class ExcludedProperties : public QSet<QString>
{
public:
    ExcludedProperties()
    {
        insert(QStringLiteral("parent"));
        insert(QStringLiteral("data"));
        insert(QStringLiteral("resources"));
        insert(QStringLiteral("children"));
        insert(QStringLiteral("target"));
        insert(QStringLiteral("visibleChildren"));
    }
};
Q_GLOBAL_STATIC(ExcludedProperties, excludedProperties);

template <typename T>
void DiffHelper::getChanges(QList<T> oldItem, QList<T> newItem, const QString& indent) const
{
    int oldCount = oldItem.count(), newCount = newItem.count();
    qDebug() << qPrintable(indent) << oldCount << newCount;
    for (int i = 0; i < oldCount; ++i) {
        getChanges(oldItem.at(i), newItem.at(i), indent + "  ");
    }
}

template <typename T>
void DiffHelper::getChanges(QQmlListProperty<T> oldItem, QQmlListProperty<T> newItem, const QString& indent) const
{
    int oldCount = oldItem.count(&oldItem), newCount = newItem.count(&newItem);
    qDebug() << qPrintable(indent) << oldCount << newCount;
    for (int i = 0; i < oldCount; ++i) {
        getChanges(oldItem.at(&oldItem, i), newItem.at(&newItem, i), indent + "  ");
    }
}

void DiffHelper::getChanges(QQuickStateAction oldItem, QQuickStateAction newItem, const QString& indent) const
{
    if (oldItem.property.name() != newItem.property.name())
        qDebug() << qPrintable(indent) << "property: " << oldItem.property.name() << newItem.property.name();
    if (oldItem.fromValue != newItem.fromValue)
        qDebug() << qPrintable(indent) << "fromValue: " << oldItem.fromValue << newItem.fromValue;
    if (oldItem.toValue != newItem.toValue)
        qDebug() << qPrintable(indent) << "toValue: " << oldItem.toValue << newItem.toValue;
}

QStringList DiffHelper::getChanges(QObject* oldItem, QObject* newItem, const QString& indent) const
{
    if (!oldItem || !newItem)
        return QStringList();

    QString newIndent = indent + "  ";

    QList<QByteArray> properties = oldItem->dynamicPropertyNames();
    const QMetaObject *meta = oldItem->metaObject();
    for (int i = 0; i < meta->propertyCount(); i++) {
        QMetaProperty property = meta->property(i);
        properties << property.name();
    }

    qDebug() << qPrintable(indent) << oldItem << newItem << "properties" << properties.size() << properties;

    foreach (const QByteArray& name, properties) {
        // const char* name
        if (excludedProperties->contains(name))
            continue;

        QVariant oldValue = oldItem->property(name);
        QVariant newValue = newItem->property(name);

        if (oldValue.canConvert<QQuickAnchorLine>() && newValue.canConvert<QQuickAnchorLine>()) {
            // item?
            QQuickAnchorLine::AnchorLine oldAnchorLine = qvariant_cast<QQuickAnchorLine>(oldValue).anchorLine;
            QQuickAnchorLine::AnchorLine newAnchorLine = qvariant_cast<QQuickAnchorLine>(newValue).anchorLine;
            if (oldAnchorLine != newAnchorLine)
                qDebug() << qPrintable(indent) << name << oldAnchorLine << newAnchorLine;
        } else if (oldValue.canConvert<QQmlListProperty<QQuickState> >() && newValue.canConvert<QQmlListProperty<QQuickState> >()) {
            qDebug() << qPrintable(indent) << name;
            getChanges(qvariant_cast<QQmlListProperty<QQuickState> >(oldValue), qvariant_cast<QQmlListProperty<QQuickState> >(newValue), newIndent);
        } else if (oldValue.canConvert<QQmlListProperty<QQuickStateOperation> >() && newValue.canConvert<QQmlListProperty<QQuickStateOperation> >()) {
            qDebug() << qPrintable(indent) << name;
            getChanges(qvariant_cast<QQmlListProperty<QQuickStateOperation> >(oldValue), qvariant_cast<QQmlListProperty<QQuickStateOperation> >(newValue), newIndent);
        } else if (oldValue.canConvert<QQmlListProperty<QQuickTransition> >() && newValue.canConvert<QQmlListProperty<QQuickTransition> >()) {
            qDebug() << qPrintable(indent) << name;
            getChanges(qvariant_cast<QQmlListProperty<QQuickTransition> >(oldValue), qvariant_cast<QQmlListProperty<QQuickTransition> >(newValue), newIndent);
        } else if (oldValue.canConvert<QObject*>() && newValue.canConvert<QObject*>()) {
            qDebug() << qPrintable(indent) << name;
            getChanges(qvariant_cast<QObject*>(oldValue), qvariant_cast<QObject*>(newValue), newIndent);
        } else if (oldValue != newValue) {
            qDebug() << qPrintable(indent) << name << oldValue << newValue;
        }
    }

    if (qobject_cast<QQuickPropertyChanges*>(oldItem) && qobject_cast<QQuickPropertyChanges*>(newItem)) {
        getChanges(qobject_cast<QQuickPropertyChanges*>(oldItem)->actions(), qobject_cast<QQuickPropertyChanges*>(newItem)->actions(), newIndent);
    }

    for (int i = 0; i < oldItem->children().size(); ++i) {
        getChanges(oldItem->children().at(i), newItem->children().at(i), indent + "  ");
    }


    QStringList r;
    r << "A" << "B" << "C";
    return r;
}
