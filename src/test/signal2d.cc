#include <signal/gosignal2d.h>
#include <signal/gofilterfir.h>
#include <signal/gofiltergauss.h>
#include <math/gomatrix.h>
#include <defs/gotypes.h>
#include <gostring.h>
#include <qimage.h>

#include <time.h>

void main (int argc, char* argv[]) {
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
  
  *sFilt2 = *s;

  goInt32 diff  = s->meanDiff (*sFilt2, 50, 20);

  goMatrix<goDouble> m(3,3);
  m[0][0] = 1.0;
  m[0][1] = 0;
  m[0][2] = 1;
  m[1][0] = 0;
  m[1][1] = 1.0;
  m[1][2] = 1;
  m[2][0] = 0;
  m[2][1] = 0;
  m[2][2] = 1;
  goInt32 diffM = s->meanDiff (*sFilt2, m);
  
  cout << "Calculated mean difference: " << diff << endl;
  cout << "Calculated mean difference using matrix: " << diffM << endl;

  for (i = 0; i < 1; i++) {
    gaussFilter.filter (*s, *sFilt);
  }
  
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












