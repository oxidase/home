import QtQuick 2.0
import QtWebKit 3.0

// make -C Source/WebKit/qt/declarative -f Makefile.declarative.public install
// sudo cp bin/QtWebProcess /usr/local/bin


WebView {
    id: webview
    url: "http://qt-project.org"
    width: 800
    height: 480
    onNavigationRequested: {
        // detect URL scheme prefix, most likely an external link
        var schemaRE = /^\w+:/;
        if (schemaRE.test(request.url)) {
            request.action = WebView.AcceptRequest;
        } else {
            request.action = WebView.IgnoreRequest;
            // delegate request.url here
        }
    }
}
