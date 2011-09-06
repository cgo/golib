/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <math/gonvector.h>
#include <math/goimagecorr.h>
#include <graphics/goima.h>
#include <defs/gotypes.h>
#include <iostream.h>
#include <stdlib.h>


int main (int argc, char* argv[]) {
  goNVector<goInt16> *v;
  goIndex_t i = 0;
  goIndex_t x = 0, y = 0;
  goIma<unsigned char> image;

  goImageCorr<goUInt8> corr;

  cout << sizeof(int) << "," << sizeof(unsigned char) << "\n";
  
  if (argc < 4) {
    cout << argv[0] << " <file> <x> <y>\n";
    exit (1);
  }

  image.setWidth  (atoi(argv[2]));
  image.setHeight (atoi(argv[3]));
  image.setSkip   (12288);
  image.load      (argv[1]);

  image.swapBytes();

  v     = new goNVector<goInt16>[image.height()] (0);

  cout << "v: " << v << "\n";

  if (image.fail()) {
    cout << "file open failed\n";
    exit (1);
  }
  cout << image.width() << "," << image.height() << " read.\n";

  printf("%d\n", v[1][2]);

  for (y = 0; y < image.height(); y++) {
    v[y].setVectorPtr((goInt16*)(&image.rawData()[(image.width() << 1) * y]));
    v[y].setSize (image.width());
  }

  goNVector<goInt16> tmpVec (image.width());
  
  cout << v[0] << "\n" << v[1] << "\n";
  v[0] -= v[1];
  cout << v[0];

  
  /* 
   * Here begins my first test for the correlation class
   */
  corr.setImage (image);

}




