#ifndef __GODEPOTFRAME_H__
#define __GODEPOTFRAME_H__

#include <config.h>
#ifdef HAVE_LIBQT

#include <qmenubar.h>
#include <qframe.h>
#include <qpushbutton.h>
#include <qlistbox.h>
#include <qgroupbox.h>
#include <qpopupmenu.h>
#include <qfiledialog.h>
#include <godepot.h>
#include <gopaper.h>
#include <goarray.h>
#include <gostring.h>
#include <non-free/depot/gopaperframe.h>

class goDepotFrame : public QFrame {
  Q_OBJECT
 public:
  goDepotFrame (QWidget *parent = 0);
  virtual ~goDepotFrame ();

  void setDepot      (goDepot& d);
  void readDepotFile (const char* filename);

  public slots:
  void menuOpenFile ();
  void menuSaveFile ();
  void menuSaveFileAs ();
  void menuQuit ();

  public slots:
  void addPaper ();
  void addPaperCallback ();
  void sellPaper ();
  void splitPaper ();
  void mergePapers ();
  void updateDepot ();		
  void nameDoubleClicked (int);
  void paperSelectionChanged  (int idx);

 protected:
          void makeMenu ();
  virtual void resizeEvent (QResizeEvent *e);

  int xyOffset;

  goDepot  *depot;
  goString *fileName;

  goPaper      *paper;
  goPaperFrame *paperFrame;

  QMenuBar    *menubar;
  QPopupMenu  *fileMenu;
  QPopupMenu  *actionMenu;

  QListBox    *nameList;
  
  QGroupBox   *infoBox;
  QLabel      *gainLabel, *gainDisplay;
  QLabel      *valueLabel, *valueDisplay;
  QLabel      *paperGainLabel, *paperGainDisplay;
  QLabel      *paperValueLabel, *paperValueDisplay;

  QGroupBox   *actionBox;
  QPushButton *addButton;
  //QPushButton *viewButton;
  QPushButton *sellButton;
  QPushButton *splitButton;
  QPushButton *mergeButton;
};

#endif /* __GODEPOTFRAME_H__ */

#endif /* HAVE_LIBQT */














