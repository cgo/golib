#include <config.h>

#ifdef HAVE_LIBQT
#include <non-free/qtwidgets/goinputwidget.h>

goInputWidget::goInputWidget (QWidget* parent, const char* name, bool modal,
			      const char* text)
  : QDialog (parent, name, modal) {
  okButton     = new QPushButton ("Ok",this);
  cancelButton = new QPushButton ("Cancel",this);
  amountEdit   = new QLineEdit   (this);
  label        = new QLabel      (text,this);
  xyOffset     = 4;

  connect (okButton, SIGNAL (clicked()),
	   this, SLOT (accept()));
  connect (cancelButton, SIGNAL (clicked()),
	   this, SLOT (reject()) );

  this->resize (200,80);
}

QString
goInputWidget::getInput (QWidget* parent, const char* text) {
  QString retVal;
  goInputWidget *inp = new goInputWidget (parent,0,true,text);
  if (inp->exec() == QDialog::Accepted) {
    retVal = inp->getEdit()->text();
  }
  delete inp;
  return retVal;
}

void
goInputWidget::resizeEvent (QResizeEvent *e) {
  okButton->resize (okButton->sizeHint());
  okButton->resize (okButton->width(),20);
  cancelButton->resize (cancelButton->sizeHint());
  cancelButton->resize (cancelButton->width(),20);
  amountEdit->resize   (amountEdit->sizeHint());
  label->resize        (label->sizeHint());
  
  label->move ( (e->size().width() - label->width()) >> 1,
		xyOffset);
  amountEdit->move ( (e->size().width() - amountEdit->width()) >> 1,
		     label->y() + label->height() + xyOffset);
  okButton->move (xyOffset,
		  amountEdit->height() + amountEdit->y() + xyOffset);
  cancelButton->move (e->size().width() - cancelButton->width() - xyOffset,
		      okButton->y());
}

#endif /* USE_QT */
