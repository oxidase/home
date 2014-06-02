// -*- compile-command: "make" -*-
#include <QDebug>
#include <QFile>
#include <QCryptographicHash>

#include "diffhelper.h"

DiffHelper::DiffHelper(QObject* parent) :
    QObject(parent)
{
}

DiffHelper::~DiffHelper()
{
}

QString DiffHelper::md5(const QString& fileName) const {
    QFile file(fileName);
    if (file.open(QIODevice::ReadOnly | QIODevice::Text)) {
        return QCryptographicHash::hash(file.readAll(), QCryptographicHash::Md5).toHex();
    }
    return QString("invalid");
}
