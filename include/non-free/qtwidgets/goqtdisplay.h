/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOQTDISPLAY_H
#define GOQTDISPLAY_H

#include <qwidget.h>
#include <gotypes.h>

/**
 * Preliminary Qt display (quick-hack). It is VERY slow,
 * I just needed one fast and didn't care about speed. The next one will be
 * native X11 ;-)
 * You have to call setBufferSize() before you can display the buffer, otherwise you might
 * get a segmentation fault.
 */
class goQtDisplay 
: public QWidget {
 Q_OBJECT;
 public:
  ///
  goQtDisplay (QWidget* parent = 0, const char* name = 0, WFlags f = 0);
  ///
  virtual ~goQtDisplay ();
  ///
  inline void	setBufferPtr	(int** ptr) { buffer = ptr; }
  ///
  void		displayBuffer	();
  ///
  inline void   setBufferSize	(int x, int y) { sizeX = x; sizeY = y; }
  
  public slots:
   void repaint ();
   void update  ();
 protected:
  ///
  int** buffer;
  ///
  int   sizeX;
  ///
  int   sizeY;

  void paintEvent (QPaintEvent*);
};

#endif /* GOQTDISPLAY_H */

