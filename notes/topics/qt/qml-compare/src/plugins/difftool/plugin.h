#include <QQmlExtensionPlugin>

#include "changesmodel.h"
#include "diffhelper.h"

class TextBalloonPlugin : public QQmlExtensionPlugin
{
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "org.qt-project.Qt.QQmlExtensionInterface")
public:
    void registerTypes(const char *uri)
    {
        qDebug() << "ok!!!";
        Q_ASSERT(uri == QLatin1String(URI));
        qmlRegisterType<DiffHelper>(uri, 1, 0, "DiffHelper");
        qmlRegisterType<ChangesModel>(uri, 1, 0, "ChangesModel");
    }
};
