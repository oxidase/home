#ifndef MODEL_H
#define MODEL_H

#include <QAbstractItemModel>
#include <QStringList>
#include <QtQml>

class Model : public QAbstractListModel {
public:
    enum {
        NameRole = Qt::UserRole + 1,
        ColorRole,
    };

    Model(QObject* parent = 0) : QAbstractListModel(parent) {};
    Model(const QStringList& names, const QString& color = QStringLiteral("black"), QObject* parent = 0);
    QHash<int,QByteArray> roleNames() const;
    int rowCount(const QModelIndex &parent = QModelIndex()) const;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;

private:
    QStringList m_names;
    QString m_color;
};

// QML_DECLARE_TYPE(Model)

#endif
