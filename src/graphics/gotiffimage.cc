#include <gotiffimage.h>
#ifdef HAVE_LIBTIFF
#include <iostream.h>

goTIFFImage::goTIFFImage () {
  buffer = 0;
}

goTIFFImage::~goTIFFImage () {

}

bool
goTIFFImage::read (const char *filename) {
  TIFF* tiff = 0;
  tiff = TIFFOpen (filename, "r");
  
  if (!tiff) {
    return false;
  }
  char emsg[1024];
  if ( TIFFRGBAImageOK (tiff, emsg) != 1 ) {
    cout << "image OK returned message " << emsg << "\n";
    return false;
  }
  
  if ( TIFFRGBAImageBegin (&tiffImage, tiff, 0, emsg) != 1 ) {
    cout << "imageBegin returned message " << emsg << "\n";
    return false;
  }

  
  int width = tiffImage.width;
  int height = tiffImage.height;

  if (buffer) {
    delete buffer;
  }

  buffer = new uint32[width * height];

  if ( TIFFRGBAImageGet	(&tiffImage, buffer, width, height) != 1) {
    cout << "imageGet returned message " << emsg << "\n";
    return false;
  }

  uint16 pc = 0;
  TIFFGetField (tiff, TIFFTAG_PLANARCONFIG, &pc);
  cout << "planar config: " << pc << endl;

  TIFFRGBAImageEnd	(&tiffImage);
  TIFFClose		(tiff);

  return true;
}

bool
goTIFFImage::write (const char *filename, uint32 width, uint32 height) {
  TIFF *tiff = 0;
  tiff = TIFFOpen (filename,"w");
  if (!tiff) return false;
  unsigned int y;
  tiffImage.width = width;
  tiffImage.height = height;
  tiffImage.tif = tiff;
  TIFFSetField (tiff, TIFFTAG_IMAGEWIDTH, width);
  TIFFSetField (tiff, TIFFTAG_IMAGELENGTH, height);
  TIFFSetField (tiff, TIFFTAG_BITSPERSAMPLE, 8);
  TIFFSetField (tiff, TIFFTAG_SAMPLESPERPIXEL, 3);
  TIFFSetField (tiff, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
  TIFFSetField (tiff, TIFFTAG_STRIPOFFSETS, 1);
  
  TIFFSetField (tiff, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  for (y = 0; y < height; y++) {
    TIFFWriteScanline(tiff, (tdata_t)buffer, (uint32)y);
  }
  TIFFClose (tiff);
  return true;
}

goTIFFImage&
goTIFFImage::operator= (goTIFFImage& other) {
  return *this;
}

#endif /* HAVE_LIBTIFF */















