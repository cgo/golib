/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <misc/godate.h>
#include <financial/gopaper.h>
#include <financial/godepot.h>
#include <non-free/depot/godepotframe.h>
#include <gostring.h>
#include <goarray.h>
#include <qapplication.h>

int main (int argc,char *argv[]) {
  QApplication::setStyle (WindowsStyle);
  QApplication qapp (argc,argv);
  goDepotFrame depotFrame;
  depotFrame.setCaption("Depot -- 1999 Christian Gosch");
  qapp.setMainWidget (&depotFrame);
  depotFrame.show();
  return qapp.exec();




  goDate d1 (11,12,1998);
  goDate d2 (10,12,1998);
  goDate d3 (10,12,1997);
  goDate d4 (14,10,1999);
  goString s1;
  goString s2;

  goPaper paper;
  goDepot depot;

  depot.read ("depotfile.txt");
  
  cout << "Gain SMS: " << depot.gain (depot.findPaperByTicker ("SMS")) << "\n";
  cout << "Gain INTC: " << depot.gain (depot.findPaperByTicker ("INTC")) << "\n";

}





