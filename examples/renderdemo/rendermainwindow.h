#ifndef RENDERMAINWINDOW_H
#define RENDERMAINWINDOW_H

#include <qmainwindow.h>
#include <renderapplication.h>

class
RenderMainWindow : public QMainWindow
{
    Q_OBJECT;
 public:
    RenderMainWindow (QWidget * parent = 0, const char * name = 0, WFlags f = WType_TopLevel);
    virtual ~RenderMainWindow();

    RenderApplication* getRenderApp() { return renderApp; }

 public slots:
     void quitThis();
 signals:
 void quit();

 protected:
    
 private:
    RenderApplication *renderApp;
};

#endif
