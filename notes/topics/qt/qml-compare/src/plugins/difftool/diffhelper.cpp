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

QString DiffHelper::absolutePath(const QString& path) const
{
    QDir dir(path);
    return dir.absolutePath();
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
        insert(QStringLiteral("source"));
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
    if (oldCount != newCount) {
        modificationsModel.append(ChangeItem(parentPath, QStringLiteral("list size changed"), oldCount, newCount, oldParentItem, newParentItem));
        return;
    }

    for (int i = 0; i < oldCount; ++i) {
        getChanges(oldItem.at(i), newItem.at(i), oldParentItem, newParentItem, parentPath + QStringLiteral("[") + QString::number(i) + QStringLiteral("]"));
    }
}

template <typename T>
void DiffHelper::getChanges(QQmlListProperty<T> oldItem, QQmlListProperty<T> newItem, QQuickItem* oldParentItem, QQuickItem* newParentItem, const QString& parentPath)
{
    int oldCount = oldItem.count(&oldItem), newCount = newItem.count(&newItem);
    if (oldCount != newCount) {
        modificationsModel.append(ChangeItem(parentPath, QStringLiteral("list size changed"), oldCount, newCount, oldParentItem, newParentItem));
        return;
    }

    for (int i = 0; i < oldCount; ++i) {
        getChanges(oldItem.at(&oldItem, i), newItem.at(&newItem, i), oldParentItem, newParentItem, parentPath + QStringLiteral("[") + QString::number(i) + QStringLiteral("]"));
    }
}

void DiffHelper::getChanges(QQuickStateAction oldItem, QQuickStateAction newItem, QQuickItem* oldParentItem, QQuickItem* newParentItem, const QString& parentPath)
{
    if (oldItem.property.name() != newItem.property.name())
        modificationsModel.append(ChangeItem(parentPath + ".name", QStringLiteral("property value changed"), oldItem.property.name(), newItem.property.name(), oldParentItem, newParentItem));
    if (oldItem.fromValue != newItem.fromValue)
        modificationsModel.append(ChangeItem(parentPath + ".fromValue", QStringLiteral("property value changed"), oldItem.fromValue, newItem.fromValue, oldParentItem, newParentItem));
    if (oldItem.toValue != newItem.toValue)
        modificationsModel.append(ChangeItem(parentPath + ".toValue", QStringLiteral("property value changed"), oldItem.toValue, newItem.toValue, oldParentItem, newParentItem));
}

void DiffHelper::getChanges(QObject* oldItem, QObject* newItem, QQuickItem* oldParentItem, QQuickItem* newParentItem, const QString& parentPath)
{
    if (visitedItems.contains(oldItem) || visitedItems.contains(newItem))
        return;

    if (!oldItem && !newItem) {
        return;
    } else if (oldItem && !newItem) {
        // remove old item
        return;
    } else if (!oldItem && newItem) {
        // added new item
        return;
    }

    visitedItems.insert(oldItem);
    visitedItems.insert(newItem);
    QQuickItem* oldVisibleItem = qobject_cast<QQuickItem*>(oldItem); if (!oldVisibleItem) oldVisibleItem = oldParentItem;
    QQuickItem* newVisibleItem = qobject_cast<QQuickItem*>(newItem); if (!newVisibleItem) newVisibleItem = newParentItem;
    QString path = parentPath.isEmpty() ? getObjectName(oldItem) : parentPath;

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

    // get removed properties
    foreach (const QByteArray& name, removedProperties) {
        if (excludedProperties->contains(name))
            continue;

        QString propertyPath = path + QStringLiteral(".") + name;
        QVariant oldValue = oldItem->property(name);
        deletionsModel.append(ChangeItem(propertyPath, QStringLiteral("property removed"), oldValue, QVariant(), oldVisibleItem, 0));
    }

    // get inserted properties
    foreach (const QByteArray& name, insertedProperties) {
        if (excludedProperties->contains(name))
            continue;

        QString propertyPath = path + QStringLiteral(".") + name;
        QVariant newValue = newItem->property(name);
        insertionsModel.append(ChangeItem(propertyPath, QStringLiteral("property inserted"), QVariant(), newValue, 0, newVisibleItem));
    }

    // get modified properties
    foreach (const QByteArray& name, commonProperties) {
        if (excludedProperties->contains(name))
            continue;

        QString propertyPath = path + QStringLiteral(".") + name;
        QVariant oldValue = oldItem->property(name);
        QVariant newValue = newItem->property(name);

        if (oldValue.userType() != newValue.userType()) {
            modificationsModel.append(ChangeItem(propertyPath, QStringLiteral("property type changed"),
                                                 QMetaType::typeName(oldValue.userType()), QMetaType::typeName(newValue.userType()),
                                                 oldVisibleItem, newVisibleItem));
        } else if (oldValue.canConvert<QQuickAnchorLine>() && newValue.canConvert<QQuickAnchorLine>()) {
            QQuickAnchorLine::AnchorLine oldAnchorLine = qvariant_cast<QQuickAnchorLine>(oldValue).anchorLine;
            QQuickAnchorLine::AnchorLine newAnchorLine = qvariant_cast<QQuickAnchorLine>(newValue).anchorLine;
            if (oldAnchorLine != newAnchorLine)
                modificationsModel.append(ChangeItem(propertyPath, QStringLiteral("anchorLine changed"), oldAnchorLine, newAnchorLine, oldVisibleItem, newVisibleItem));
        } else if (oldValue.canConvert<QQmlBinding*>() && newValue.canConvert<QQmlBinding*>()) {
            qWarning() << "TODO: compare" << (int)oldValue.type() << oldValue << (int)newValue.type() << newValue;
            // QQmlBinding* oldBinding = qvariant_cast<QQmlBinding*>(oldValue);
            // QQmlBinding* newBinding = qvariant_cast<QQmlBinding*>(newValue);
            // qDebug() << "------------------" << QQmlBinding::expressionIdentifier(oldBinding) << QQmlBinding::expressionIdentifier(newBinding) << oldBinding->evaluate();
            // qDebug() << oldBinding->expression();
            // qDebug() << newBinding->expression();
        } else if (oldValue.canConvert<QQmlListProperty<QQuickState> >() && newValue.canConvert<QQmlListProperty<QQuickState> >()) {
            getChanges(qvariant_cast<QQmlListProperty<QQuickState> >(oldValue), qvariant_cast<QQmlListProperty<QQuickState> >(newValue), oldVisibleItem, newVisibleItem, propertyPath);
        } else if (oldValue.canConvert<QQmlListProperty<QQuickStateOperation> >() && newValue.canConvert<QQmlListProperty<QQuickStateOperation> >()) {
            getChanges(qvariant_cast<QQmlListProperty<QQuickStateOperation> >(oldValue), qvariant_cast<QQmlListProperty<QQuickStateOperation> >(newValue), oldVisibleItem, newVisibleItem, propertyPath);
        } else if (oldValue.canConvert<QQmlListProperty<QQuickTransition> >() && newValue.canConvert<QQmlListProperty<QQuickTransition> >()) {
            getChanges(qvariant_cast<QQmlListProperty<QQuickTransition> >(oldValue), qvariant_cast<QQmlListProperty<QQuickTransition> >(newValue), oldVisibleItem, newVisibleItem, propertyPath);
        } else if (oldValue.canConvert<QQmlListProperty<QQuickAbstractAnimation> >() && newValue.canConvert<QQmlListProperty<QQuickAbstractAnimation> >()) {
            getChanges(qvariant_cast<QQmlListProperty<QQuickAbstractAnimation> >(oldValue), qvariant_cast<QQmlListProperty<QQuickAbstractAnimation> >(newValue), oldVisibleItem, newVisibleItem, propertyPath);
        } else if (oldValue.canConvert<QQmlListProperty<QQuickTransform> >() && newValue.canConvert<QQmlListProperty<QQuickTransform> >()) {
            getChanges(qvariant_cast<QQmlListProperty<QQuickTransform> >(oldValue), qvariant_cast<QQmlListProperty<QQuickTransform> >(newValue), oldVisibleItem, newVisibleItem, propertyPath);
        } else if (oldValue.canConvert<QObject*>() && newValue.canConvert<QObject*>()) {
            getChanges(qvariant_cast<QObject*>(oldValue), qvariant_cast<QObject*>(newValue), oldVisibleItem, newVisibleItem, propertyPath);
        } else if (oldValue.type() >= QVariant::UserType) {
            qWarning() << "TODO: compare (" << (int)oldValue.type() << ")" << oldValue << "with  (" << (int)newValue.type() << ")" <<  (int)newValue.type() << newValue;
        } else if (oldValue != newValue) {
            modificationsModel.append(ChangeItem(propertyPath, QStringLiteral("property value changed"), oldValue, newValue, oldVisibleItem, newVisibleItem));
        }
    }

    // check actions list
    if (qobject_cast<QQuickPropertyChanges*>(oldItem) && qobject_cast<QQuickPropertyChanges*>(newItem)) {
        getChanges(qobject_cast<QQuickPropertyChanges*>(oldItem)->actions(), qobject_cast<QQuickPropertyChanges*>(newItem)->actions(), oldVisibleItem, newVisibleItem, path + ".actions");
    }

    // check children list
    if (oldItem->children().size() != newItem->children().size()) {
        modificationsModel.append(ChangeItem(parentPath, QStringLiteral("children list size changed"), oldItem->children().size(), newItem->children().size(), oldParentItem, newParentItem));
    } else {
        for (int i = 0; i < oldItem->children().size(); ++i) {
            getChanges(oldItem->children().at(i), newItem->children().at(i), oldVisibleItem, newVisibleItem, path + QStringLiteral(".") + getObjectName(oldItem->children().at(i)));
        }
    }
}

void DiffHelper::getChanges(QObject* oldItem, QObject* newItem)
{
    qDebug() << "getChanges started" ;

    visitedItems.clear();
    insertionsModel.clear();
    deletionsModel.clear();
    modificationsModel.clear();

    getChanges(oldItem, newItem, 0, 0, QString());

    qDebug() << "getChanges exited" ;

    emit changesModelUpdated();
}
