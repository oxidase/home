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
        insert(QStringLiteral("baseUrl"));
    }
};
Q_GLOBAL_STATIC(ExcludedProperties, excludedProperties);

QString DiffHelper::getObjectName(QObject* o) const
{
    if (!o)
        return QString();

    if (o->objectName().isEmpty())
        return o->metaObject()->className();
    return o->objectName();
}

template <typename T>
void DiffHelper::getChanges(QList<T> oldItem, QList<T> newItem, QQuickItem* oldParentItem, QQuickItem* newParentItem, const QString& parentPath)
{
    int oldCount = oldItem.count(), newCount = newItem.count();
    for (int i = 0; i < oldCount; ++i) {
        getChanges(oldItem.at(i), newItem.at(i), oldParentItem, newParentItem, parentPath + QStringLiteral("[") + i + QStringLiteral("]"));
    }
}

template <typename T>
void DiffHelper::getChanges(QQmlListProperty<T> oldItem, QQmlListProperty<T> newItem, QQuickItem* oldParentItem, QQuickItem* newParentItem, const QString& parentPath)
{
    int oldCount = oldItem.count(&oldItem), newCount = newItem.count(&newItem);
    for (int i = 0; i < oldCount; ++i) {
        getChanges(oldItem.at(&oldItem, i), newItem.at(&newItem, i), oldParentItem, newParentItem, parentPath + QStringLiteral("[") + i + QStringLiteral("]"));
    }
}

void DiffHelper::getChanges(QQuickStateAction oldItem, QQuickStateAction newItem, QQuickItem* oldParentItem, QQuickItem* newParentItem, const QString& parentPath)
{
    if (oldItem.property.name() != newItem.property.name())
        qDebug() << parentPath << "." << "name" << oldItem.property.name() << newItem.property.name();
    if (oldItem.fromValue != newItem.fromValue)
        qDebug() << parentPath << "." << "fromValue: " << oldItem.fromValue << newItem.fromValue;
    if (oldItem.toValue != newItem.toValue)
        qDebug() << parentPath << "." <<  "toValue: " << oldItem.toValue << newItem.toValue;
}

void DiffHelper::getChanges(QObject* oldItem, QObject* newItem, QQuickItem* oldParentItem, QQuickItem* newParentItem, const QString& parentPath)
{
    if (visitedItems.contains(oldItem) || visitedItems.contains(newItem))
        return;

    // qDebug()  << getObjectName(oldItem) << getObjectName(newItem);
    if (!oldItem && !newItem) {
        return;
    } else if (oldItem && !newItem) {
        // remove old item
        return;
    } else if (!oldItem && newItem) {
        // added new item
        return;
    }

    // qDebug() << oldItem->metaObject()->className() << newItem->metaObject()->className();
    visitedItems.insert(oldItem);
    visitedItems.insert(newItem);
    QQuickItem* oldVisibleItem = qobject_cast<QQuickItem*>(oldItem); if (!oldVisibleItem) oldVisibleItem = oldParentItem;
    QQuickItem* newVisibleItem = qobject_cast<QQuickItem*>(newItem); if (!newVisibleItem) newVisibleItem = newParentItem;
    QString path = parentPath.isEmpty() ? getObjectName(oldItem) : parentPath + QStringLiteral(".") + getObjectName(oldItem);

    // get new, removed and common properties
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
        qDebug() << "-" << path << "." << name << oldItem->property(name).toString();
    }

    foreach (const QByteArray& name, insertedProperties) {
        if (excludedProperties->contains(name))
            continue;
        qDebug() << "+" << path << "." << name << newItem->property(name).toString();
    }

    foreach (const QByteArray& name, commonProperties) {
        if (excludedProperties->contains(name))
            continue;

        QString propertyPath = path + QStringLiteral(".") + name;
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
            // qDebug() << oldBinding->property();
        } else if (oldValue.canConvert<QQmlListProperty<QQuickState> >() && newValue.canConvert<QQmlListProperty<QQuickState> >()) {
            getChanges(qvariant_cast<QQmlListProperty<QQuickState> >(oldValue), qvariant_cast<QQmlListProperty<QQuickState> >(newValue), oldVisibleItem, newVisibleItem, path);
        } else if (oldValue.canConvert<QQmlListProperty<QQuickStateOperation> >() && newValue.canConvert<QQmlListProperty<QQuickStateOperation> >()) {
            getChanges(qvariant_cast<QQmlListProperty<QQuickStateOperation> >(oldValue), qvariant_cast<QQmlListProperty<QQuickStateOperation> >(newValue), oldVisibleItem, newVisibleItem, path);
        } else if (oldValue.canConvert<QQmlListProperty<QQuickTransition> >() && newValue.canConvert<QQmlListProperty<QQuickTransition> >()) {
            getChanges(qvariant_cast<QQmlListProperty<QQuickTransition> >(oldValue), qvariant_cast<QQmlListProperty<QQuickTransition> >(newValue), oldVisibleItem, newVisibleItem, path);
        } else if (oldValue.canConvert<QQmlListProperty<QQuickAbstractAnimation> >() && newValue.canConvert<QQmlListProperty<QQuickAbstractAnimation> >()) {
            getChanges(qvariant_cast<QQmlListProperty<QQuickAbstractAnimation> >(oldValue), qvariant_cast<QQmlListProperty<QQuickAbstractAnimation> >(newValue), oldVisibleItem, newVisibleItem, path);
        } else if (oldValue.canConvert<QQmlListProperty<QQuickTransform> >() && newValue.canConvert<QQmlListProperty<QQuickTransform> >()) {
            getChanges(qvariant_cast<QQmlListProperty<QQuickTransform> >(oldValue), qvariant_cast<QQmlListProperty<QQuickTransform> >(newValue), oldVisibleItem, newVisibleItem, path);
        } else if (oldValue.canConvert<QObject*>() && newValue.canConvert<QObject*>()) {
            getChanges(qvariant_cast<QObject*>(oldValue), qvariant_cast<QObject*>(newValue), oldVisibleItem, newVisibleItem, path);
        } else if (oldValue != newValue) {
            // qDebug() << "!!!!!!!!!!!!!!";
            modificationsModel.append(ChangeItem(propertyPath, QStringLiteral("property value changed"), oldValue, newValue, oldVisibleItem, newVisibleItem));
        }
    }

    if (qobject_cast<QQuickPropertyChanges*>(oldItem) && qobject_cast<QQuickPropertyChanges*>(newItem)) {
        getChanges(qobject_cast<QQuickPropertyChanges*>(oldItem)->actions(), qobject_cast<QQuickPropertyChanges*>(newItem)->actions(), oldVisibleItem, newVisibleItem, path + ".actions");
    }

    // check children
    for (int i = 0; i < oldItem->children().size(); ++i) {
        getChanges(oldItem->children().at(i), newItem->children().at(i), oldVisibleItem, newVisibleItem, path);
    }


    visitedItems.remove(oldItem);
    visitedItems.remove(newItem);
}

void DiffHelper::getChanges(QObject* oldItem, QObject* newItem)
{
    visitedItems.clear();
    insertionsModel.clear();
    deletionsModel.clear();
    modificationsModel.clear();

    getChanges(oldItem, newItem, 0, 0, QString());
    emit changesModelUpdated();

    Q_ASSERT(visitedItems.isEmpty());
}
