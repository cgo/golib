/* --*- C++ -*-- */

#include <config.h>

#ifdef HAVE_LIBQT
#include <non-free/depot/godepotframe.h>
#include <non-free/depot/gopaperframe.h>
#include <iostream.h>
#include <qmessagebox.h>
#include <non-free/qtwidgets/goinputwidget.h>

void
goDepotFrame::makeMenu () {
  menubar->insertItem ("Datei",fileMenu);
  menubar->insertItem ("Aktionen",actionMenu);
  fileMenu->insertItem ("Öffnen",this,SLOT(menuOpenFile()));
  fileMenu->insertItem ("Speichern",this,SLOT(menuSaveFile()));
  fileMenu->insertItem ("Speichern unter",this,SLOT(menuSaveFileAs()));
  fileMenu->insertSeparator();
  fileMenu->insertItem ("Beenden",this,SLOT(menuQuit()));

  actionMenu->insertItem ("Neu",this,SLOT(addPaper()));
  actionMenu->insertItem ("Verkaufen",this,SLOT(sellPaper()));
  actionMenu->insertItem ("Aufspalten",this,SLOT(splitPaper()));
}

goDepotFrame::goDepotFrame (QWidget *parent) 
  : QFrame (parent) {

  xyOffset = 4;

  depot      = new goDepot;
  fileName   = new goString;
  paper      = new goPaper;
  paperFrame = new goPaperFrame (0,paper);
  paperFrame->hide();

  menubar     = new QMenuBar (this);
  menubar->setFrameStyle (WinPanel|Raised);
  fileMenu    = new QPopupMenu;
  actionMenu  = new QPopupMenu;
  nameList    = new QListBox(this);
  nameList    ->setMultiSelection (false);
  actionBox   = new QGroupBox ("Aktionen",this);
  addButton   = new QPushButton ("Neu",actionBox);
  sellButton  = new QPushButton ("Verkaufen",actionBox);
  splitButton = new QPushButton ("Aufteilen",actionBox);
  mergeButton = new QPushButton ("Zusammenfügen",actionBox);

  infoBox     		= new QGroupBox ("Info",this);
  gainLabel   		= new QLabel 	("Depotgewinn:",infoBox);
  gainDisplay		= new QLabel	("0",infoBox);
  valueLabel   		= new QLabel 	("Depotwert:",infoBox);
  valueDisplay		= new QLabel	("0",infoBox);
  paperGainLabel	= new QLabel 	("Gewinn:",infoBox);
  paperGainDisplay	= new QLabel	("0",infoBox);
  paperValueLabel   	= new QLabel 	("Wert:",infoBox);
  paperValueDisplay	= new QLabel	("0",infoBox);


  makeMenu();
  menubar->show();
  nameList->show();
  actionBox->show();

  connect (paperFrame, SIGNAL (paperChanged()),
	   this, 	 SLOT (addPaperCallback()) );
  connect (nameList, SIGNAL (selected (int)),
	   this,       SLOT (nameDoubleClicked (int)) );
  connect (nameList, SIGNAL (highlighted(int)),
	   this,       SLOT (paperSelectionChanged(int)) );
  
  /*
   * Buttons
   */
  connect (addButton, SIGNAL (clicked()),
	   this,        SLOT (addPaper()) );
  connect (sellButton, SIGNAL(clicked()),
	   this,	SLOT (sellPaper()) );
  connect (splitButton, SIGNAL (clicked()),
	   this,	SLOT (splitPaper()) );
  connect (mergeButton, SIGNAL (clicked()),
	   this,	SLOT (mergePapers()) );

  mergeButton->setEnabled (false);
}

goDepotFrame::~goDepotFrame () {};

void	
goDepotFrame::setDepot (goDepot& d) {
  *depot = d;
}

void 
goDepotFrame::readDepotFile (const char* filename) {
  goString temp;
  depot->read (filename);
  updateDepot();
}

void
goDepotFrame::menuOpenFile () {
  QString temp;

  if (QMessageBox::warning (this,
			    "Öffnen",
			    "Momentanes Depot ersetzen (Datei wird nicht berührt) ?",
			    "Ja",
			    "Nein") == 0) {
    nameList->clear();
    /*
     * Bug alert: this could be bad because the memory is not freed explicitly.
     * (eventually add a clear() member to depot class)
     */
    depot->getPapers().resize (0);
  }

  temp = QFileDialog::getOpenFileName ();
  (*fileName) = (const char*) temp;
  readDepotFile ((const char*) temp);
}

void
goDepotFrame::menuSaveFile () {
  if (fileName->getSize() == 0) {
    menuSaveFileAs();
  } else {
    if (QMessageBox::warning (this,
			      "Speichern",
			      "Wirklich speichern ?",
			      "Ja",
			      "Nein") == 0) {
      depot->write (fileName->toCharPtr());
    }
  }
}

void
goDepotFrame::menuSaveFileAs () {
  QString fname;
  fname = QFileDialog::getSaveFileName();

  *fileName = (const char*)fname;
  if (fileName->getSize() > 0) {
    menuSaveFile();
  }
}

void
goDepotFrame::menuQuit () {
  if (QMessageBox::warning (this,
			    "Beenden",
			    "Sind Sie sicher, daß alle Daten gesichert sind\nund Sie das Fenster schließen wollen ?",
			    "Ja",
			    "Nein") == 0) {
    this->close (true);
  }
}

void
goDepotFrame::addPaper () {
  paperFrame->show();
}

void
goDepotFrame::addPaperCallback () {
  depot->addPaper (*paper);
  updateDepot();
}

void
goDepotFrame::sellPaper () {
  QString tmpString;
  int amount = 0;
  goIndex_t idx = 0;

  tmpString = goInputWidget::getInput(this,"Anzahl zu verkaufen:");
  if (!tmpString.isEmpty()) {
    amount = tmpString.toInt();
    idx = depot->splitPaper (nameList->currentItem(), amount);
    depot->getPapers()[idx]->setSold(true);
  }
  updateDepot();
}

void
goDepotFrame::splitPaper () {
  QString tmpString;
  int amount = 0;
  goIndex_t idx = 0;
  tmpString = goInputWidget::getInput(this,"Anzahl:");
  if (!tmpString.isEmpty()) {
    amount = tmpString.toInt();
    idx = depot->splitPaper (nameList->currentItem(), amount);
  }
  updateDepot();
}

void
goDepotFrame::mergePapers () {

}

void
goDepotFrame::nameDoubleClicked (int idx) {
  //goIndex_t i = (goIndex_t)depot->findPaperByName ((*nameList).text(idx));
  goIndex_t i = (goIndex_t)idx; 
  goPaperFrame *f = new goPaperFrame (0, depot->getPapers()[i]);
  f->show();
  f->getAddButton()->setText ("Übernehmen");
  connect (f->getQuitButton(), SIGNAL (clicked()),
	   this,		 SLOT (updateDepot()) );
}	

void
goDepotFrame::paperSelectionChanged (int idx) {
  QString tempq;

  tempq.setNum ((float)depot->getPapers()[nameList->currentItem()]->getGain(), 'f', 2);
  paperGainDisplay ->setText (tempq);
  tempq.setNum ((float)depot->getPapers()[nameList->currentItem()]->getValue(), 'f', 2);
  paperValueDisplay->setText (tempq);

  paperGainDisplay ->resize (paperGainDisplay->sizeHint());
  paperValueDisplay->resize (paperValueDisplay->sizeHint());
  this->update();
}

/*
 *  Updates the displays / lists etc.
 */
void
goDepotFrame::updateDepot () {
  goIndex_t 	i = 0;
  goString 	temp;
  QString  	tempq;
  int		oldHighlit = 0;
  nameList->clear();
  for (i = 0; i < depot->getPapers().getSize(); i++) {
    temp = depot->getPapers()[i]->getName();
    if (temp.toCharPtr()) {
      nameList->insertItem (temp.toCharPtr(),i);
    }
  }
  nameList->setCurrentItem (oldHighlit);

  tempq.setNum ((float)depot->overallGain(),'f',2);
  gainDisplay->setText (tempq);
  tempq.setNum ((float)depot->totalValue(),'f',2);
  valueDisplay->setText (tempq);
  gainDisplay->resize (gainDisplay->sizeHint());
  valueDisplay->resize (valueDisplay->sizeHint());

  this->setCaption(fileName->toCharPtr());
  this->update();
  this->resize (this->size());
}

void
goDepotFrame::resizeEvent (QResizeEvent *e) {
  
  gainLabel->move 	(xyOffset,fontMetrics().height());
  gainLabel->resize 	(gainLabel->sizeHint());
  gainDisplay->move 	(gainLabel->x() + gainLabel->width() + xyOffset,
			 gainLabel->y());
  gainDisplay->resize 	(gainDisplay->sizeHint());
  valueLabel->move 	(gainLabel->x(), gainLabel->y() + gainLabel->height());
  valueLabel->resize 	(valueLabel->sizeHint());
  valueDisplay->move 	(valueLabel->x() + valueLabel->width() + xyOffset,
			 valueLabel->y());
  valueDisplay->resize 	(valueDisplay->sizeHint());

  paperGainLabel->move 		(xyOffset,valueDisplay->height() + valueDisplay->y());
  paperGainLabel->resize	(paperGainLabel->sizeHint());
  paperGainDisplay->move	(paperGainLabel->x() + paperGainLabel->width() + xyOffset,
				 paperGainLabel->y());
  paperGainDisplay->resize 	(paperGainDisplay->sizeHint());
  paperValueLabel->move 	(paperGainLabel->x(), paperGainLabel->y() + paperGainLabel->height());
  paperValueLabel->resize 	(paperValueLabel->sizeHint());
  paperValueDisplay->move 	(paperValueLabel->x() + paperValueLabel->width() + xyOffset,
				 paperValueLabel->y());
  paperValueDisplay->resize 	(paperValueDisplay->sizeHint());
  

  nameList->resize (e->size().width() >> 1, 
		    e->size().height() - 2*xyOffset - menubar->height());
  nameList->move (xyOffset,xyOffset + menubar->height() + menubar->y());

  addButton->resize (addButton->sizeHint());
  addButton->resize (addButton->width(),20);
  addButton->move   (xyOffset,fontMetrics().height() + xyOffset);

  sellButton->resize (sellButton->sizeHint());
  sellButton->resize (sellButton->width(),20);
  sellButton->move   (addButton->x(), 
		      addButton->height() + addButton->y() + xyOffset);

  splitButton->resize (splitButton->sizeHint());
  splitButton->resize (splitButton->width(),20);
  splitButton->move   (sellButton->x(), 
		       sellButton->height() + sellButton->y() + xyOffset);

  mergeButton->resize (mergeButton->sizeHint());
  mergeButton->resize (mergeButton->width(),20);
  mergeButton->move   (splitButton->x(), 
		       splitButton->height() + splitButton->y() + xyOffset);

  infoBox->move (nameList->x() + xyOffset + nameList->width(), 
		 nameList->y());
  infoBox->resize (e->size().width() - infoBox->x() - xyOffset,
		   (nameList->height() >> 1) - xyOffset * 2 );
  actionBox->move (infoBox->x(),
		   infoBox->y() + infoBox->height() + xyOffset);
  actionBox->resize (e->size().width() - actionBox->x() - xyOffset,
		     e->size().height() - infoBox->y() - infoBox->height() - xyOffset * 2);
}

#endif /* USE_QT */











