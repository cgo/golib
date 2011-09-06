/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goconfigfileaccess.h>
#include <gotypes.h>
#include <gostring.h>


goConfigFileAccess::goConfigFileAccess () : goConfigFile () {
  floatValue = 0;
}

goConfigFileAccess::~goConfigFileAccess () {
}

float
goConfigFileAccess::sum (goString& entryName) {
  goIndex_t i = 0;
  goIndex_t i2 = 0;

  goString	tmp;

  floatValue = 0;

  for (i = 0; i < chapters.getSize(); i++) {
    for (i2 = 0; i2 < chapters[i]->getSectionArray().getSize(); i2++) {
      tmp = chapters[i]->getSectionArray()[i2]->get (entryName);
      if (!fail()) {
	floatValue += tmp.toFloat();
	last_failed = false;
      }		
    }
  }	
  return floatValue;
}

float
goConfigFileAccess::sum (goString& chapterName, goString& entryName) {
  goIndex_t i = 0;
  goIndex_t i2 = 0;

  goString	tmp;

  floatValue = 0;

  i = find (chapterName);
  if (!fail()) {
    for (i2 = 0; i2 < chapters[i]->getSectionArray().getSize(); i2++) {
      tmp = chapters[i]->getSectionArray()[i2]->get (entryName);
      if (!fail()) {
	floatValue += tmp.toFloat();
	last_failed = false;
      }		
    }
  return floatValue;
  }
  return 0;
}
