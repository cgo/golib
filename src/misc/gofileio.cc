#include <gofileio.h>
#include <stdio.h>
#include <config.h>
#include <goerror.h>
#ifdef HAVE_LIBJPEG
	#include <jpeglib.h>
	#include <jerror.h>
	#include <jconfig.h>
	#include <jmorecfg.h>
#endif	

static void readLine (FILE* f, goString& str) {
  str.resize (0);
  char c = 0;
  c = fgetc(f);
  while ( !feof(f) && (c != '\n') ) {
    str += c;
    c = fgetc(f); 
  }
}

#define GOFILE_PGM_NEXTLINE(f,line) {\
  readLine (f, line);\
  while ( (line[0] == '#') || (line.getSize() == 0) ) {\
    readLine (f, line);\
  }\
}

void
goFileIO::readPGM (goString& filename, goSignal2D<goInt32>*& signal) {
  FILE* f;
  goString line;
  goString tempStr;
  int width, height;
  int maxval;
  f = fopen (filename.toCharPtr(), "r");

  GOFILE_PGM_NEXTLINE(f,line);
  if ( !(line == "P5") ) {
      std::cout << "goFileIO::readPGM(): no raw 8 bit PGM file !" << std::endl;
      std::cout << "ID read was " << line << std::endl;
    return;
  }
  GOFILE_PGM_NEXTLINE(f,line);
  tempStr.resize(0);
  goIndex_t i = 0;
  while (line[i] != ' ') {
    tempStr += line[i++];
  }
  width = tempStr.toInt();
  i++;
  tempStr.resize(0);
  while (i < line.getSize()) {
    tempStr += line[i++];
  }
  height = tempStr.toInt();

  GOFILE_PGM_NEXTLINE(f,line);  
  maxval = line.toInt();
  
  signal = new goSignal2D<goInt32> ((goSize_t)width, (goSize_t)height);
  goIndex_t x, y;
  goUInt8 *scanline = new goUInt8[width];
  for (y = 0; y < height; y++) {
    fread ((void*)scanline, sizeof (goUInt8), width, f);
    for (x = 0; x < width; x++) {
      *signal->getPtr (x, y) = (goInt32)scanline[x];
    }
  }

  fclose(f);
  delete[] scanline;
}

void
goFileIO::readPGM (const char* filename, goSignal2D<goInt32>*& signal) {
  goString s;
  s = filename;
  goFileIO::readPGM (s, signal);
}

void
goFileIO::writePGM (goString& filename, goSignal2D<goInt32>& signal) {
  FILE* f;
  goString line;
  goString tempStr;
  int width = signal.getSizeX();
  int height = signal.getSizeY();
  int maxval = 255;
  char tempChar[255];
  f = fopen (filename.toCharPtr(), "w");


  line = "P5";
  fprintf(f,"%s\n",line.toCharPtr());
  line = "# File written by ";
  line += GO_PROJECT_NAME;
  line += " ";
  line += GO_PROJECT_VERSION;
  fprintf(f,"%s\n",line.toCharPtr());
  line.resize(0);
  sprintf(tempChar, "%d %d\n%d\n", signal.getSizeX(), signal.getSizeY(), maxval);
  fprintf(f,"%s",&tempChar[0]);
  
  goIndex_t x, y;
  goUInt8 *scanline = new goUInt8[width];
  for (y = 0; y < height; y++) {
    for (x = 0; x < width; x++) {
      scanline[x] = (goUInt8)*signal.getPtr (x, y);
    }
    fwrite ((void*)scanline, sizeof(goUInt8), width, f);
  }

  fclose(f);
  delete[] scanline;
}

void
goFileIO::writePGM (const char* filename, goSignal2D<goInt32>& signal) {
  goString s;
  s = filename;
  goFileIO::writePGM (s, signal);
}

void
goFileIO::readJPEG (const char* filename, goSignal2D<goInt32>*& signal)
{
#ifdef HAVE_LIBJPEG
	FILE *file;
	struct jpeg_decompress_struct cinfo;
    struct jpeg_error_mgr jerr;
    cinfo.err = jpeg_std_error(&jerr);
    jpeg_create_decompress(&cinfo);

	if ((file = fopen(filename, "rb")) == NULL) 
	{
		goError::print("goFileIO::readJPEG()","Could not open file");
    }
    jpeg_stdio_src(&cinfo, file);
	jpeg_read_header(&cinfo, TRUE);
	
	jpeg_start_decompress(&cinfo);

	signal = new goSignal2D<goInt32> (cinfo.output_width, cinfo.output_height);
	int lineBufferHeight = cinfo.rec_outbuf_height;
	int colorComponents  = cinfo.output_components;
	if (colorComponents > 1)
	{
		goError::print("goFileIO::readJPEG()","Currently only supports one\
				color component images (gray scale)");
		jpeg_finish_decompress (&cinfo);
		return;
	}
	
	goInt32 	*ptr 	 = signal->getPtr (0,0);
	goInt32		*xPtr    = 0;
	JSAMPROW    rows[4];
	JSAMPARRAY  buffer;
	unsigned int i;
	for (i = 0; i < 4; i++)
	{
		rows[i] = new JSAMPLE[cinfo.output_width * colorComponents];
	}
	buffer = &rows[0];
	
	goPtrdiff_t lineDiff = signal->getOffsetY();
	unsigned int j;
	unsigned int k,x;
	for (i = 0; i < cinfo.output_height; )
	{
		 j = jpeg_read_scanlines (&cinfo, buffer, 4);
		 for (k = 0; k < j; k++)
		 {	
			 xPtr = ptr;
			 for (x = 0; x < signal->getSizeX(); x++)
			 {
				 *xPtr = (goInt32)rows[k][x];
				 xPtr++;
			 }
			 ptr += lineDiff;
		 }
		 i += j;
	}
	jpeg_finish_decompress(&cinfo);
	for (i = 0; i < 4; i++)
	{
		delete[] rows[i];
	}

#endif	
}
