#include <gofileio.h>
#include <stdio.h>
#include <config.h>
#include <goerror.h>
#include <goglobal.h>
#ifdef HAVE_LIBJPEG
	#include <jpeglib.h>
	#include <jerror.h>
	#include <jconfig.h>
	#include <jmorecfg.h>
#endif	

#ifdef HAVE_LIBIL
# include <IL/il.h>
#endif

#ifndef GOSIGNAL3D_H
# include <gosignal3d.h>
#endif

#ifndef GOSIGNALMACROS_H
# include <gosignalmacros.h>
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

#ifdef HAVE_LIBIL
/**
 * @brief Reads an image file into the provided signal object.
 *
 * The goSignal3D object given as parameter <code>signal</code>
 * determines the type of the loaded data.
 * The image will be converted from the original type to the
 * type given with the signal parameter.
 * 
 * @param filename  Name of the file containing the image.
 * @param signal    Object of type goSignal3D that will contain the image after
 *                  the method successfully returns.
 *
 * @return  True if successful, false otherwise.
 **/
bool
goFileIO::readImage (const char* filename, goObjectBase* signal)
{
    if (!signal)
    {
        return false;
    }
    if (!goGlobal::ILInitialized)
    {
        ilInit ();
        goGlobal::ILInitialized = true;
    }
    ILuint imageName = 0;
    ilGenImages (1, &imageName);
    ilBindImage (imageName);

    // DevIL stupidly asks for char* instead of const char*, so convert it.
    goString fn = filename;
    if (ilLoadImage (fn.getPtr()) == IL_FALSE)
    {
        ilDeleteImages (1, &imageName);
        return false;
    }
    ILint width  = 0;
    ILint height = 0;
    width  = ilGetInteger (IL_IMAGE_WIDTH);
    height = ilGetInteger (IL_IMAGE_HEIGHT);

    //  NOTE: It is quietly assumed that the caller provides us with a goSignal3D object.
    /// \todo Add a sanity check here.
    goSignal3D<goInt8>* tempSignal = (goSignal3D<goInt8>*)signal;
    switch (tempSignal->getDataType().getID())
    {
        case GO_INT8:
            {
                goSignal3D<goInt8>* s = tempSignal;
                s->destroy ();
                s->make (width, height, 1, 32, 32, 1, 32, 32, 0);
                if (ilConvertImage (IL_LUMINANCE, IL_BYTE) == IL_FALSE)
                {
                    ilDeleteImages (1, &imageName);
                    return false;
                }
                goInt8* data = (goInt8*)ilGetData ();
                GO_SIGNAL3D_EACHELEMENT (*__ptr = *data; ++data, (*s), goInt8);
            }
            break;
        case GO_UINT8:
            {
                goSignal3D<goUInt8>* s = (goSignal3D<goUInt8>*)tempSignal;
                s->destroy ();
                s->make (width, height, 1, 32, 32, 1, 32, 32, 0);
                if (ilConvertImage (IL_LUMINANCE, IL_UNSIGNED_BYTE) == IL_FALSE)
                {
                    ilDeleteImages (1, &imageName);
                    return false;
                }
                goUInt8* data = (goUInt8*)ilGetData ();
                GO_SIGNAL3D_EACHELEMENT (*__ptr = *data; ++data, (*s), goUInt8);
            }
            break;
        case GO_INT16:
            {
                goSignal3D<goInt16>* s = (goSignal3D<goInt16>*)tempSignal;
                s->destroy ();
                s->make (width, height, 1, 32, 32, 1, 32, 32, 0);
                if (ilConvertImage (IL_LUMINANCE, IL_SHORT) == IL_FALSE)
                {
                    ilDeleteImages (1, &imageName);
                    return false;
                }
                goInt16* data = (goInt16*)ilGetData ();
                GO_SIGNAL3D_EACHELEMENT (*__ptr = *data; ++data, (*s), goInt16);
            }
            break;
        case GO_UINT16:
            {
                goSignal3D<goUInt16>* s = (goSignal3D<goUInt16>*)tempSignal;
                s->destroy ();
                s->make (width, height, 1, 32, 32, 1, 32, 32, 0);
                if (ilConvertImage (IL_LUMINANCE, IL_UNSIGNED_SHORT) == IL_FALSE)
                {
                    ilDeleteImages (1, &imageName);
                    return false;
                }
                goUInt16* data = (goUInt16*)ilGetData ();
                GO_SIGNAL3D_EACHELEMENT (*__ptr = *data; ++data, (*s), goUInt16);
            }
            break;
        case GO_INT32:
            {
                goSignal3D<goInt32>* s = (goSignal3D<goInt32>*)tempSignal;
                s->destroy ();
                s->make (width, height, 1, 32, 32, 1, 32, 32, 0);
                if (ilConvertImage (IL_LUMINANCE, IL_INT) == IL_FALSE)
                {
                    ilDeleteImages (1, &imageName);
                    return false;
                }
                goInt32* data = (goInt32*)ilGetData ();
                GO_SIGNAL3D_EACHELEMENT (*__ptr = *data; ++data, (*s), goInt32);
            }
            break;
        case GO_UINT32:
            {
                goSignal3D<goUInt32>* s = (goSignal3D<goUInt32>*)tempSignal;
                s->destroy ();
                s->make (width, height, 1, 32, 32, 1, 32, 32, 0);
                if (ilConvertImage (IL_LUMINANCE, IL_UNSIGNED_INT) == IL_FALSE)
                {
                    ilDeleteImages (1, &imageName);
                    return false;
                }
                goUInt32* data = (goUInt32*)ilGetData ();
                GO_SIGNAL3D_EACHELEMENT (*__ptr = *data; ++data, (*s), goUInt32);
            }
            break;
        case GO_FLOAT:
            {
                goSignal3D<goFloat>* s = (goSignal3D<goFloat>*)tempSignal;
                s->destroy ();
                s->make (width, height, 1, 32, 32, 1, 32, 32, 0);
                if (ilConvertImage (IL_LUMINANCE, IL_FLOAT) == IL_FALSE)
                {
                    ilDeleteImages (1, &imageName);
                    return false;
                }
                goFloat* data = (goFloat*)ilGetData ();
                GO_SIGNAL3D_EACHELEMENT (*__ptr = *data; ++data, (*s), goFloat);
            }
            break;
        case GO_DOUBLE:
            {
                goSignal3D<goDouble>* s = (goSignal3D<goDouble>*)tempSignal;
                s->destroy ();
                s->make (width, height, 1, 32, 32, 1, 32, 32, 0);
                if (ilConvertImage (IL_LUMINANCE, IL_DOUBLE) == IL_FALSE)
                {
                    ilDeleteImages (1, &imageName);
                    return false;
                }
                goDouble* data = (goDouble*)ilGetData ();
                GO_SIGNAL3D_EACHELEMENT (*__ptr = *data; ++data, (*s), goDouble);
            }
            break;
        default:
            {
                ilDeleteImages (1, &imageName);
                return false;
            }
            break;
    }
    ilDeleteImages (1, &imageName);
    return true;
}
#else
bool
goFileIO::readImage (const char*, goObjectBase*)
{
    return false;
}
#endif
