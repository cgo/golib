
#include <config.h>
#ifdef HAVE_LIBQT

#include <non-free/depot/gopaperframe.h>

#define NR_OF_ENTRIES 16

goPaperFrame::goPaperFrame (QWidget* parent,
			    goPaper* paperPtr) 
  : QFrame (parent) {
  
  goIndex_t i = 0;

  xyOffset = 4;

  paper = paperPtr;

  paperBox    = new QGroupBox("Papier",this);
  actionBox   = new QGroupBox  ("Aktionen",paperBox);
  addButton   = new QPushButton("Hinzufügen",actionBox);
  quitButton  = new QPushButton("Schließen" ,actionBox);
  labelFrame  = new QFrame (paperBox);
  editFrame   = new QFrame (paperBox);
  paperLabels = new goArray<QLabel*>;
  paperEdits  = new goArray<QLineEdit*>;
  paperLabels->resize (NR_OF_ENTRIES);
  paperEdits->resize (NR_OF_ENTRIES);
  
  for (i = 0; i < paperLabels->size(); i++) {
    (*paperLabels)[i] = new QLabel(labelFrame);
  }
  (*paperLabels)[0]->setText("Name");
  (*paperLabels)[1]->setText("Ticker");  
  (*paperLabels)[2]->setText("Nr");
  (*paperLabels)[3]->setText("Anzahl");
  (*paperLabels)[4]->setText("Kaufpreis");
  (*paperLabels)[5]->setText("Verkaufspreis");
  (*paperLabels)[6]->setText("Kaufdatum");
  (*paperLabels)[7]->setText("Verkaufsdatum");
  (*paperLabels)[8]->setText("Sonstige Kosten");
  (*paperLabels)[9]->setText("Währung");
  (*paperLabels)[10]->setText("Fremdwährung");
  (*paperLabels)[11]->setText("Verfallsdatum");
  (*paperLabels)[12]->setText("Zinssatz");
  (*paperLabels)[13]->setText("Vorlaufzinsen");
  (*paperLabels)[14]->setText("Wechselkurs (Kauf) (Preis d. Fremdwährung)");
  (*paperLabels)[15]->setText("Wechselkurs (Verkauf)");


  for (i = 0; i < paperEdits->size(); i++) {
    (*paperEdits)[i] = new QLineEdit(editFrame);
  }
  
  sold = new QCheckBox("Verkauft",labelFrame);

  typeGroup = new QButtonGroup ("Typ", editFrame);
  typeLoan  = new QRadioButton ("Anleihe",typeGroup);
  typeShare = new QRadioButton ("Aktie",typeGroup);
  
  paperBox  ->show();
  labelFrame->show();
  editFrame ->show();
  actionBox ->show();

  setEdits(*paper);

  connect (addButton,  SIGNAL (clicked()),
	   this,         SLOT (acceptPaper()) );
  connect (quitButton, SIGNAL (clicked()),
	   this,	 SLOT (closeWindow()) );

  this->resize (471,510);
} 

goPaperFrame::~goPaperFrame() {}

/*
 * Sets the edit lines.
 */
void
goPaperFrame::setEdits (goPaper& p) {
  goString temp;
  QString  tempq;
  temp = p.getName();
  (*paperEdits)[0]->setText(temp.toCharPtr());
  temp = p.getTicker();
  (*paperEdits)[1]->setText(temp.toCharPtr());
  temp = p.getNr();
  (*paperEdits)[2]->setText(temp.toCharPtr());
  tempq.setNum(p.getAmount());
  (*paperEdits)[3]->setText(tempq);

  tempq.setNum(p.getBuyPrice(),'f',2);
  (*paperEdits)[4]->setText(tempq);
  tempq.setNum(p.getSellPrice(),'f',2);
  (*paperEdits)[5]->setText(tempq);
  temp = p.getBuyDate();
  tempq = temp.toCharPtr();
  (*paperEdits)[6]->setText(tempq);
  temp = p.getSellDate();
  tempq = temp.toCharPtr();
  (*paperEdits)[7]->setText(tempq);
  tempq.setNum(p.getCosts(),'f',2);
  (*paperEdits)[8]->setText(tempq);
  temp = p.getCurrency();
  (*paperEdits)[9]->setText(temp.toCharPtr());
  sold->setChecked(p.isSold());

  temp = p.getForeignCurrency ();
  (*paperEdits)[10]->setText (temp.toCharPtr());
  temp = p.getTimeOut();
  (*paperEdits)[11]->setText (temp.toCharPtr());
  tempq.setNum(p.getInterest(),'f',2);
  (*paperEdits)[12]->setText (tempq);
  tempq.setNum(p.getPrePaid(),'f',2);
  (*paperEdits)[13]->setText (tempq);
  tempq.setNum(p.getExchangeRateBuy(),'f',3);
  (*paperEdits)[14]->setText (tempq);
  tempq.setNum(p.getExchangeRateSell(),'f',3);
  (*paperEdits)[15]->setText (tempq);

  if (p.getType() == goPaper::LOAN) {
    typeLoan->setChecked(true);
  } else { typeShare->setChecked (true); }

  temp = p.getName();
  this->setCaption(temp.toCharPtr());
}

/*
 * Makes deep copy of paper p into the local paper.
 */
void
goPaperFrame::setPaper (goPaper& p) {
  if (paper) {	
    *paper = p;
  }
  setEdits (p);
}

/*
 * Puts the line edits into the local paper.
 */
void
goPaperFrame::acceptPaper () {
  goDate tmpDate;
  /*  (*paperLabels)[0]->setText("Name");
  (*paperLabels)[1]->setText("Ticker");  
  (*paperLabels)[2]->setText("Nr");
  (*paperLabels)[3]->setText("Anzahl");
  (*paperLabels)[4]->setText("Kaufpreis");
  (*paperLabels)[5]->setText("Verkaufspreis");
  (*paperLabels)[6]->setText("Kaufdatum");
  (*paperLabels)[7]->setText("Verkaufsdatum");
  (*paperLabels)[8]->setText("Sonstige Kosten");
  (*paperLabels)[9]->setText("Währung");
  */
  paper->setName     ((*paperEdits)[0]->text());
  paper->setTicker   ((*paperEdits)[1]->text());
  paper->setNr       ((*paperEdits)[2]->text());
  paper->setAmount   ((goInt32)QString((*paperEdits)[3]->text()).toInt());
  paper->setBuyPrice ((goFloat)QString((*paperEdits)[4]->text()).toFloat());
  paper->setSellPrice((goFloat)QString((*paperEdits)[5]->text()).toFloat());
  tmpDate = goString((*paperEdits)[6]->text()).toDate();
  paper->setBuyDate  (tmpDate);
  tmpDate = goString((*paperEdits)[7]->text()).toDate();
  paper->setSellDate (tmpDate);
  paper->setCosts    ((goFloat)QString ((*paperEdits)[8]->text()).toFloat());
  paper->setCurrency ((*paperEdits)[9]->text());
  paper->setSold     (sold->isChecked());

  paper->setForeignCurrency 	((*paperEdits)[10]->text());
  tmpDate = goString((*paperEdits)[11]->text()).toDate();
  paper->setTimeOut		(tmpDate);
  paper->setInterest ((goFloat)QString ((*paperEdits)[12]->text()).toFloat());
  paper->setPrePaid  ((goFloat)QString ((*paperEdits)[13]->text()).toFloat());
  paper->setExchangeRateBuy  ((goFloat)QString ((*paperEdits)[14]->text()).toFloat());
  paper->setExchangeRateSell ((goFloat)QString ((*paperEdits)[15]->text()).toFloat());

  if (typeLoan->isChecked()) {
    paper->setType (goPaper::LOAN);
  } else { paper->setType (goPaper::SHARE); }
  
  emit (paperChanged());
}

void
goPaperFrame::closeWindow () {
  this->close (false);
}

/*
 * Macro to place w2 vertically in the middle of w1
 */
#define PLACE_MIDDLE(w1,w2) (w1.y() + (w1.height() - (w2.height()) >> 1) )

void
goPaperFrame::resizeEvent (QResizeEvent *e) {
  goIndex_t i = 0;
  int 	    labelFrameWidth = 0;

  /*
   * paperBox
   */
  paperBox->resize (this->width() - 2*xyOffset,this->height() - xyOffset - fontMetrics().height());
  paperBox->move (xyOffset,fontMetrics().height());



  /*
   * actions
   */
  addButton->resize (addButton->sizeHint());
  addButton->resize (addButton->width(), 20);
  addButton->move   (xyOffset, fontMetrics().height() + xyOffset);
  quitButton->resize (addButton->size());
  quitButton->move (addButton->x(), addButton->y() + addButton->height() + xyOffset);


  /*
   * entries
   */
  (*paperLabels)[0]->resize ((*paperLabels)[0]->sizeHint());
  (*paperLabels)[0]->move (xyOffset,fontMetrics().height() + xyOffset);
  (*paperEdits)[0]->resize ( (*paperEdits)[0]->sizeHint() );
  (*paperEdits)[0] ->move (xyOffset, PLACE_MIDDLE((*(*paperLabels)[0]),(*(*paperEdits)[0])));
  
  for (i = 1; i < paperLabels->size(); i++) {
    (*paperLabels)[i]->resize ((*paperLabels)[i]->sizeHint());
    if ((*paperLabels)[i]->width() > labelFrameWidth) {
      labelFrameWidth = (*paperLabels)[i]->width();
    }
    (*paperLabels)[i]->move (xyOffset,(*paperLabels)[i-1]->height()
			               +(*paperLabels)[i-1]->y() + 2*xyOffset);
    (*paperEdits)[i]->resize ( (*paperEdits)[i]->sizeHint() );
    (*paperEdits)[i]->resize ( (*paperEdits)[i]->width(),20 );
    (*paperEdits)[i] ->move (xyOffset, PLACE_MIDDLE( (*(*paperLabels)[i]), (*(*paperEdits)[i]) ) );
  }
  sold->resize (sold->sizeHint());
  sold->move   (xyOffset,
		(*paperLabels)[i-1]->height() + (*paperLabels)[i-1]->y()
		+ xyOffset);
  typeLoan->resize   (typeLoan->sizeHint());
  typeShare->resize  (typeShare->sizeHint());
  typeShare->move    (xyOffset, fontMetrics().height());
  typeLoan->move     (typeShare->x(), typeShare->y() + typeShare->height());
  typeGroup ->resize (typeLoan->width() + 2*xyOffset,
		      typeLoan->y() + typeLoan->height() + xyOffset);

  /*
   * frames / groupboxes
   */
  /*  labelFrame->resize ((paperBox->width() >> 1) - 2*xyOffset, 
      paperBox->height() - xyOffset - fontMetrics().height() ); */
  labelFrame->resize (labelFrameWidth + xyOffset, 
		      paperBox->height() - xyOffset - fontMetrics().height() );
  actionBox->resize  (editFrame->width(), 
		      (int)(labelFrame->height() / (float)4.0) - xyOffset);
  editFrame ->resize ((paperBox->width() - labelFrameWidth) - 4*xyOffset, 
		      labelFrame->height() - xyOffset - actionBox->height() );


  labelFrame->move (xyOffset,fontMetrics().height());
  editFrame ->move (labelFrame->x() + labelFrame->width() + xyOffset,labelFrame->y());
  typeGroup ->move ((*paperEdits)[0]->x() + (*paperEdits)[0]->width() + xyOffset,
		    (*paperEdits)[0]->y() );
  actionBox ->move (editFrame->x(), editFrame->height() + editFrame->y() + xyOffset);
}

#endif /* USE_QT */





