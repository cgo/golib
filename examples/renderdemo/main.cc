#include <qapplication.h>
#include <qplatinumstyle.h>
#include <renderapplication.h>
#include <rendermainwindow.h>

int main (int argc, char *argv[])
{
    QApplication app(argc, argv);
    app.setStyle (new QPlatinumStyle);
    // RenderApplication gui;
    RenderMainWindow mainWindow;
    QObject::connect ((const QObject*)&mainWindow, SIGNAL(quit()), (const QObject*)&app, SLOT(quit()));
    mainWindow.show();
    app.setMainWidget (&mainWindow);
    return app.exec();
}
