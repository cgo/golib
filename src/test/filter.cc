/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <signal/gosignal2d.h>
#include <signal/gofilterfir.h>
#include <signal/gofiltergauss.h>
#include <defs/gotypes.h>
#include <gostring.h>
#include <qimage.h>

#include <time.h>

void main (int argc, char* argv[]) {
  goDouble mask[25] = {1,4,6,4,1,
		      4,8,10,8,4,
		      6,10,12,10,6,
		      4,8,10,8,4,
		      1,4,6,4,1};
  goDouble maskHi[25] = {12,6,4,6,12,
		         6,4,2,4,6,
		         4,2,1,2,4,
		         6,4,2,4,6,
		         12,6,4,6,12};
  /*
  goDouble mask[9] = {0.0735120, 0.872676, 0.0735120,
		       0.872676,  1       , 0.872676, 
		       0.0735120, 0.872676, 0.0735120};
  goDouble maskHi[9] = {-1, 0, 1,
			-2, 0, 2,
			-1, 0, 1};
  goDouble maskHi2[9] = {-1, -2, -1,
			 0, 0, 0,
			 1, 2, 1};
  */
  goFilterFIR<goInt32> filter (5,5);
  goFilterGauss<goInt32> gaussFilter (3);
  QImage *image = new QImage();
  image->load (argv[1]);
  goSignal2D<goInt32> *s = new goSignal2D<goInt32>(image->width(), image->height());
  goSignal2D<goInt32> *sFilt = new goSignal2D<goInt32>(image->width(), image->height());
  goSignal2D<goInt32> *sFilt2 = new goSignal2D<goInt32>(image->width(), image->height());
  goSignal2D<goInt32> *temp;
  
  goString filename;
  filename = "filt";
  filename += argv[1];
  
  goIndex_t x,y;
  for ( y = 0; y < s->getSizeY(); y++) {
    for (x = 0; x < s->getSizeX(); x++) {
      *s->getPtr (x,y) = (goInt32)image->pixelIndex(x,y);
    }
  }

  int i;
  goDouble sum = 0;
  goDouble sumHi = 0;
  for (i = 0; i < 25; i++) {
    sum += fabs (mask[i]);
    sumHi += fabs (maskHi[i]);
  }
  for (i = 0; i < 25; i++) {
    mask[i] /= sum;
    maskHi[i] /= sumHi;
  }

  // Hi pass x and y
  /*
  filter.setMask (maskHi);
  filter.filter (*s, *sFilt);
  filter.setMask (maskHi2);
  filter.filter (*s, *sFilt2);
  *sFilt += *sFilt2;
  *s += *sFilt;
  */

  
  // filter.setMask (mask);
  // filter.filter  (*s, *sFilt);

  *sFilt2 = *s;
  filter.setMask (mask);
  clock_t t1 = clock();
  for (i = 0; i < 1; i++) {
    gaussFilter.filter (*s, *sFilt);
    //temp = s;
    //s = sFilt;
    //sFilt = temp;
  }
  clock_t t2 = clock();
  cout << "time used: " << (t2-t1)/ (float)CLOCKS_PER_SEC << " s" << endl;
  // *s = *sFilt;
  // *s -= *sFilt2;
  // sFilt = s;

  int pixel;
  for ( y = 0; y < s->getSizeY(); y++) {
    for (x = 0; x < s->getSizeX(); x++) {
      pixel = (unsigned int)(*s->getPtr (x,y));
      if (pixel < 0) pixel = 255 - ((-pixel)%255);
      if (pixel > 255) pixel = pixel%255;
      image->setPixel (x, y, (unsigned int)pixel);
    }
  }
  
  image->save (filename.toCharPtr(),"PPM");

  delete image;
  // delete s;
  // delete sFilt;
}












