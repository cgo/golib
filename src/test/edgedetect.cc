#include <signal/gosignal2d.h>
#include <signal/gofilterfir.h>
#include <signal/goedgedetect.h>
#include <defs/gotypes.h>
#include <gostring.h>
#include <qimage.h>

#include <time.h>

void main (int argc, char* argv[]) {
  QImage *image = new QImage();
  image->load (argv[1]);

  goString filename;
  filename = "filt";
  filename += argv[1];
  
  goIndex_t x,y;

  goEdgeDetect<goInt32> edgeDetect;

  {
    goSignal2D<goInt32> *s = new goSignal2D<goInt32>(image->width(), image->height());
    
    
    /* Copy image to signal2d, slow */
    for ( y = 0; y < s->getSizeY(); y++) {
      for (x = 0; x < s->getSizeX(); x++) {
	*s->getPtr (x,y) = (goInt32)image->pixelIndex(x,y);
      }
    }
    
    edgeDetect.run (s, 0.1);
    
    delete s;
  }

  goSignal2D<goFloat> *s = 0;
  s = edgeDetect.getHiPass();


  /* Save data back to image and save it */
  int pixel;
  int min = 6500000;
  int max = -6500000;
  for ( y = 0; y < s->getSizeY(); y++) {
    for (x = 0; x < s->getSizeX(); x++) {
      pixel = (int)(*s->getPtr (x,y));
      if (pixel > max) max = pixel;
      if (pixel < min) min = pixel;
    }
  }
  
  float f = fabs(max - min);

  for ( y = 0; y < s->getSizeY(); y++) {
    for (x = 0; x < s->getSizeX(); x++) {
      pixel = (int)(*s->getPtr (x,y));
      pixel = (int)( (pixel - min) / f * 255);
      image->setPixel (x, y, pixel);
    }
  }

  image->save (filename.toCharPtr(),"PPM");


  goSignal2D<goInt32> *hist = edgeDetect.getHistogram();
  min = 0;
  max = 0;
  for ( y = 0; y < hist->getSizeY(); y++) {
    for (x = 0; x < hist->getSizeX(); x++) {
      pixel = (int)(*hist->getPtr (x,y));
      if (pixel > max)
	max = pixel;
    }
  }

  QImage image2 ((int)hist->getSizeX(), (int)hist->getSizeY(), 32);

  f = fabs((float)max);
  for ( y = 0; y < hist->getSizeY(); y++) {
    for (x = 0; x < hist->getSizeX(); x++) {
      pixel = (int)(*s->getPtr (x,y));
      pixel = (int)( (pixel) / (float)max * 255);
      image2.setPixel (x, y, pixel);
    }
  }
  image2.save ("parameterspace.pgm","PPM");

  delete image;
  // delete s;
  // delete sFilt;
}












