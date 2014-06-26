TEMPLATE = lib
CONFIG += staticlib
OBJECTS_DIR = .tmp
DESTDIR = .tmp

OTHER_FILES += qml-compare.in \
            main.qml \
            DirectoryView.qml \
            ScrollIndicator.qml \
            SkinView.qml \
            TabButton.qml \
            assets/List_icon.svg \
            assets/Skins_icon.svg

PRE_TARGETDEPS += $$OTHER_FILES

unix {
QMAKE_POST_LINK += sed "'s\\@QMLSCENE@\\$$[QT_INSTALL_BINS]/qmlscene\\' qml-compare.in" > qml-compare && chmod a+x qml-compare $$escape_expand(\\n\\t)
}

QMAKE_POST_LINK += @$$QMAKE_COPY $$quote(qml-compare) $$quote($$top_builddir) $$escape_expand(\\n\\t)
QMAKE_POST_LINK += @$$QMAKE_COPY $$quote(*.qml) $$quote($$top_builddir) $$escape_expand(\\n\\t)
QMAKE_POST_LINK += @$(COPY_DIR) $$quote(assets) $$quote($$top_builddir) $$escape_expand(\\n\\t)
