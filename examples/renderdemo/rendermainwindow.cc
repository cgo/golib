#include <rendermainwindow.h>
#include <qmenubar.h>
#include <qtoolbar.h>
#include <qstatusbar.h>
#include <qprogressbar.h>

RenderMainWindow::RenderMainWindow (QWidget * parent, const char * name, WFlags f)
    : QMainWindow (parent, name, f)
{
    renderApp = new RenderApplication (this);
    this->setCentralWidget ((QWidget*)renderApp);
    this->setRightJustification (true);

    QToolBar *tb = new QToolBar( this );
    addToolBar( tb, tr( "Menubar" ), Top, FALSE );
    QMenuBar *mb = new QMenuBar( tb );
    mb->setFrameStyle( QFrame::NoFrame );
    tb->setStretchableWidget( mb );
    setDockEnabled( tb, Left, FALSE );
    setDockEnabled( tb, Right, FALSE );

    QPopupMenu *fileMenu = new QPopupMenu;
    fileMenu->insertItem ("Open transformed volume", renderApp, SLOT(setVolumeFileName()));
    fileMenu->insertItem ("Save image", renderApp, SLOT(saveImage()));
    fileMenu->insertSeparator();
    fileMenu->insertItem ("Quit", this, SLOT(quitThis()));
    mb->insertItem ("File",fileMenu);
    QPopupMenu *tfMenu = new QPopupMenu;
    tfMenu->insertItem ("Opacity", renderApp, SLOT(showOpacityTF()));
    tfMenu->insertItem ("Density", renderApp, SLOT(showDensityTF()));
    tfMenu->insertItem ("Image grey values", renderApp, SLOT(showGreyTF()));
    mb->insertItem ("Transfer Functions", tfMenu);
    renderApp->show();
    
    QProgressBar *pb = new QProgressBar (statusBar());
    pb->setTotalSteps (50);
    renderApp->progressBar = pb;
    statusBar()->addWidget ((QWidget*)pb);
    statusBar()->show();
   
    this->resize (renderApp->size().width(), renderApp->size().height() + statusBar()->height());
}

RenderMainWindow::~RenderMainWindow()
{
}

void
RenderMainWindow::quitThis()
{
    emit quit();
}
