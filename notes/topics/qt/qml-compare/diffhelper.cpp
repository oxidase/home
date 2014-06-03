// -*- compile-command: "make" -*-
#include <QDebug>
#include <QFile>
#include <QCryptographicHash>

#include <private/qquickitem_p.h>
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
        insert(QStringLiteral("baseUrl"));
    }
};
Q_GLOBAL_STATIC(ExcludedProperties, excludedProperties);

template <typename T>
void DiffHelper::getChanges(QList<T> oldItem, QList<T> newItem, const QString& indent) const
{
    int oldCount = oldItem.count(), newCount = newItem.count();
    qDebug() << qPrintable(indent) << oldCount << newCount << "===========================================";
    for (int i = 0; i < oldCount; ++i) {
        getChanges(oldItem.at(i), newItem.at(i), indent + "  ");
    }
}

template <typename T>
void DiffHelper::getChanges(QQmlListProperty<T> oldItem, QQmlListProperty<T> newItem, const QString& indent) const
{
    int oldCount = oldItem.count(&oldItem), newCount = newItem.count(&newItem);
    // qDebug() << qPrintable(indent) << oldCount << newCount;
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
    if (visitedItems.contains(oldItem) || visitedItems.contains(newItem))
        return QStringList();

    if (!oldItem && !newItem)
        return QStringList();

    visitedItems.insert(oldItem);
    visitedItems.insert(newItem);

    QString newIndent = indent + "  ";

    QSet<QByteArray> oldProperties = QSet<QByteArray>::fromList(oldItem->dynamicPropertyNames());
    for (int i = 0; i < newItem->metaObject()->propertyCount(); i++)
        oldProperties << oldItem->metaObject()->property(i).name();

    QSet<QByteArray> newProperties = QSet<QByteArray>::fromList(newItem->dynamicPropertyNames());
    for (int i = 0; i < newItem->metaObject()->propertyCount(); i++)
        newProperties << newItem->metaObject()->property(i).name();

    QSet<QByteArray> insertedProperties = newProperties - oldProperties;
    QSet<QByteArray> removedProperties = oldProperties - newProperties;
    QSet<QByteArray> commonProperties = oldProperties - removedProperties;

    foreach (const QByteArray& name, removedProperties) {
        if (excludedProperties->contains(name))
            continue;
        qDebug() << "-" << name << oldItem->property(name).toString();
    }

    foreach (const QByteArray& name, insertedProperties) {
        if (excludedProperties->contains(name))
            continue;
        qDebug() << "+" << name << newItem->property(name).toString();
    }

    foreach (const QByteArray& name, commonProperties) {
        if (excludedProperties->contains(name))
            continue;

        QVariant oldValue = oldItem->property(name);
        QVariant newValue = newItem->property(name);

        if (oldValue.canConvert<QQuickAnchorLine>() && newValue.canConvert<QQuickAnchorLine>()) {
            QQuickAnchorLine::AnchorLine oldAnchorLine = qvariant_cast<QQuickAnchorLine>(oldValue).anchorLine;
            QQuickAnchorLine::AnchorLine newAnchorLine = qvariant_cast<QQuickAnchorLine>(newValue).anchorLine;
            if (oldAnchorLine != newAnchorLine)
                qDebug() << "~" << name << oldAnchorLine << newAnchorLine;
        } else if (oldValue.canConvert<QQmlBinding*>() && newValue.canConvert<QQmlBinding*>()) {
            QQmlBinding* oldBinding = qvariant_cast<QQmlBinding*>(oldValue);
            QQmlBinding* newBinding = qvariant_cast<QQmlBinding*>(newValue);
            qDebug() << oldBinding->property();
        } else if (oldValue.canConvert<QQmlListProperty<QQuickState> >() && newValue.canConvert<QQmlListProperty<QQuickState> >()) {
            getChanges(qvariant_cast<QQmlListProperty<QQuickState> >(oldValue), qvariant_cast<QQmlListProperty<QQuickState> >(newValue), newIndent);
        } else if (oldValue.canConvert<QQmlListProperty<QQuickStateOperation> >() && newValue.canConvert<QQmlListProperty<QQuickStateOperation> >()) {
            getChanges(qvariant_cast<QQmlListProperty<QQuickStateOperation> >(oldValue), qvariant_cast<QQmlListProperty<QQuickStateOperation> >(newValue), newIndent);
        } else if (oldValue.canConvert<QQmlListProperty<QQuickTransition> >() && newValue.canConvert<QQmlListProperty<QQuickTransition> >()) {
            getChanges(qvariant_cast<QQmlListProperty<QQuickTransition> >(oldValue), qvariant_cast<QQmlListProperty<QQuickTransition> >(newValue), newIndent);
        } else if (oldValue.canConvert<QQmlListProperty<QQuickAbstractAnimation> >() && newValue.canConvert<QQmlListProperty<QQuickAbstractAnimation> >()) {
            getChanges(qvariant_cast<QQmlListProperty<QQuickAbstractAnimation> >(oldValue), qvariant_cast<QQmlListProperty<QQuickAbstractAnimation> >(newValue), newIndent);
        } else if (oldValue.canConvert<QQmlListProperty<QQuickTransform> >() && newValue.canConvert<QQmlListProperty<QQuickTransform> >()) {
            getChanges(qvariant_cast<QQmlListProperty<QQuickTransform> >(oldValue), qvariant_cast<QQmlListProperty<QQuickTransform> >(newValue), newIndent);
        } else if (oldValue.canConvert<QObject*>() && newValue.canConvert<QObject*>()) {
            qDebug() << name;
            getChanges(qvariant_cast<QObject*>(oldValue), qvariant_cast<QObject*>(newValue), newIndent);
        } else if (oldValue != newValue) {
            qDebug() << "~" << name << oldValue << newValue;
        }
    }

    if (qobject_cast<QQuickPropertyChanges*>(oldItem) && qobject_cast<QQuickPropertyChanges*>(newItem)) {
        getChanges(qobject_cast<QQuickPropertyChanges*>(oldItem)->actions(), qobject_cast<QQuickPropertyChanges*>(newItem)->actions(), newIndent);
    }

    for (int i = 0; i < oldItem->children().size(); ++i) {
        getChanges(oldItem->children().at(i), newItem->children().at(i), indent + "  ");
    }


    visitedItems.remove(oldItem);
    visitedItems.remove(newItem);

    QStringList r;
    r << "A" << "B" << "C";
    return r;
}
