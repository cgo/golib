#ifndef __GOPAPERFRAME_H__
#define __GOPAPERFRAME_H__

#include <config.h>

#ifdef HAVE_LIBQT

#include <qcheckbox.h>
#include <qframe.h>
#include <qmenubar.h>
#include <qpopupmenu.h>
#include <qfiledialog.h>
#include <qlistbox.h>
#include <qgroupbox.h>
#include <qpushbutton.h>
#include <qradiobutton.h>
#include <qbuttongroup.h>
#include <qlabel.h>
#include <qlineedit.h>
#include <gopaper.h>
#include <goarray.h>
#include <gostring.h>

class goPaperFrame : public QFrame {
  Q_OBJECT
 public:
  /*
   * You have to supply a paper where this class stores its data in
   */
  goPaperFrame (QWidget *parent = 0, goPaper* paperPtr = 0);
  virtual ~goPaperFrame ();

  /*
   * Sets the edits of the frame to the contents of p.
   */
  void         	setEdits      (goPaper& p);

  /*
   * Sets the contents of local paper pointer to p.
   */
  void         	setPaper      (goPaper& p);

  /*
   * Sets the local paper pointer.
   */
  void	       	setPaperPtr   (goPaper* p) { paper = p; }
  QPushButton* 	getAddButton  () { return addButton; }
  QPushButton*	getQuitButton () { return quitButton; }

  public slots:
  void   acceptPaper ();
  void   closeWindow ();

 signals:
  void paperChanged ();

 protected:
          void makeMenu ();
  virtual void resizeEvent (QResizeEvent *e);

  int xyOffset;

  goPaper  *paper;

  QMenuBar    *menubar;
  QPopupMenu  *paperMenu;

  QGroupBox           *paperBox;
  QFrame              *labelFrame;
  QFrame	      *editFrame;
  goArray<QLabel*>    *paperLabels;
  goArray<QLineEdit*> *paperEdits;
  QCheckBox           *sold;
  QButtonGroup	      *typeGroup;
  QRadioButton	      *typeLoan;
  QRadioButton	      *typeShare;

  QGroupBox   *actionBox;
  QPushButton *addButton;
  QPushButton *quitButton;
  QPushButton *viewButton;
};

#endif /* __GOPAPERFRAME_H__ */
#endif /* HAVE_LIBQT */



