#ifndef __GOINPUTWIDGET_H__
#define __GOINPUTWIDGET_H__

#include <config.h>

#ifdef HAVE_LIBQT

#include <qdialog.h>
#include <qpushbutton.h>
#include <qlineedit.h>
#include <qlabel.h>
#include <qstring.h>

class goInputWidget : public QDialog {
  Q_OBJECT
 public:
  goInputWidget (QWidget* parent = 0, const char* name = 0, bool modal = true,
		 const char* text = 0);

  static QString getInput (QWidget *parent = 0, const char* text = 0);
            void setLabel (const char* text);        
  QLineEdit*     getEdit () { return amountEdit; }
 protected:
  int	      xyOffset;
  QPushButton *okButton;
  QPushButton *cancelButton;
  QLineEdit   *amountEdit;
  QLabel      *label;

  virtual void resizeEvent (QResizeEvent *e);
};

#endif /* __GOINPUTWIDGET_H__ */

#endif /* HAVE_QT */
