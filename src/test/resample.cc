#include <signal/gosignal2d.h>
#include <signal/gofiltergauss.h>
#include <signal/goresample.h>
#include <defs/gotypes.h>
#include <gostring.h>
#include <qimage.h>

#include <time.h>

void main (int argc, char* argv[]) {
  goFilterGauss<goInt32> gaussFilter (5);
  goInt32 M = 2;
  goResample<goInt32>	 resample(M);
  QImage *image = new QImage();

  image->load (argv[1]);
  goSignal2D<goInt32> *s = new goSignal2D<goInt32>(image->width(), image->height());
  goSignal2D<goInt32> *sFilt = new goSignal2D<goInt32>(image->width(), image->height());
  goSignal2D<goInt32> *downSampled = 
    new goSignal2D<goInt32>(image->width(), image->height());
  goSignal2D<goInt32> *temp = 
    new goSignal2D<goInt32>(image->width(), image->height());
  goSignal2D<goInt32> *temp2 = 
    new goSignal2D<goInt32>(image->width(), image->height());

  goSignal2D<goInt32> *upSampled = 
    new goSignal2D<goInt32>((goSize_t)(image->width() * M), (goSize_t)(image->height() * M));
   goSignal2D<goInt32> *upFilt = 
     new goSignal2D<goInt32>((goSize_t)(image->width() * M), (goSize_t)(image->height() * M));

  goString filename;
  filename = "filt";
  filename += argv[1];
  
  goIndex_t x,y;
  for ( y = 0; y < s->getSizeY(); y++) {
    for (x = 0; x < s->getSizeX(); x++) {
      *s->getPtr (x,y) = (goInt32)image->pixelIndex(x,y);
    }
  }

  /**********************************************/
  int i;
  *temp = *s;
  gaussFilter.filter (*temp, *sFilt);
  resample.down (*temp, *s);
  temp2->resize ((goSize_t)(temp2->getSizeX() / (float)M), 
		 (goSize_t)(temp2->getSizeY() / (float)M));
  resample.down (*temp, *temp2);
  
  gaussFilter.filter (*temp2, *sFilt);
  resample.down (*temp2, *s);
  
  // resample.up (*s, *upSampled);
  // gaussFilter.filter (*upSampled, *upFilt);
  /**********************************************/
  // s = downSampled;

  QImage *outImage = new QImage (s->getSizeX(), s->getSizeY(), 8, image->numColors());
  for (int i = 0; i < image->numColors(); i++) {
    outImage->setColor (i, image->color(i));
  }
  int pixel;
  for ( y = 0; y < s->getSizeY(); y++) {
    for (x = 0; x < s->getSizeX(); x++) {
      pixel = (unsigned int)(*s->getPtr (x,y));
      if (pixel < 0) pixel = 255 - ((-pixel)%255);
      if (pixel > 255) pixel = pixel%255;
      outImage->setPixel (x, y, (goUInt8)pixel);
    }
  }
  
  outImage->save (filename.toCharPtr(),"PPM");

  delete image;
  delete outImage;
  delete s;
  // delete sFilt;
}












