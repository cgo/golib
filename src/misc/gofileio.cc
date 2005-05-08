#include <gofileio.h>
#include <stdio.h>
#include <goconfig.h>
#include <goerror.h>
#include <goglobal.h>
#include <golog.h>
#ifdef HAVE_LIBJPEG
	#include <jpeglib.h>
	#include <jerror.h>
	#include <jconfig.h>
	#include <jmorecfg.h>
#endif	
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
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

#if 0
static void readLine (FILE* f, goString& str) {
  str.resize (0);
  char c = 0;
  c = fgetc(f);
  while ( !feof(f) && (c != '\n') ) {
    str += c;
    c = fgetc(f); 
  }
}
#endif

#define GOFILE_PGM_NEXTLINE(f,line) {\
  readLine (f, line);\
  while ( (line[0] == '#') || (line.getSize() == 0) ) {\
    readLine (f, line);\
  }\
}

#if 0
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
#endif

#ifdef HAVE_LIBIL

template <class T> 
static inline bool ILtoGOSIGNAL (ILint format, ILint type, ILuint imageName, int width, int height, goSignal3D<void>* s)
{
    s->destroy ();
    if (format == IL_RGBA)
    {
        s->make (width, height, 1, 32, 32, 1, 32, 32, 1, 4);
    }
    else
    {
        s->make (width, height, 1, 32, 32, 1, 32, 32, 1, 1);
    }
    if (ilConvertImage (format, type) == IL_FALSE)
    {
        ilDeleteImages (1, &imageName);
        return false;
    }
    T* data = (T*)ilGetData ();
    if (s->getChannelCount() == 1)
    {
        GO_SIGNAL3D_EACHELEMENT_GENERIC (*(T*)__ptr = *data; ++data, (*s));
    }
    if (s->getChannelCount() == 4)
    {
        data = (T*)ilGetData();
        s->setChannel(0);
        GO_SIGNAL3D_EACHELEMENT_GENERIC (*(T*)__ptr = *data; data+=4, (*s));
        data = (T*)ilGetData() + 1;
        s->setChannel(1);
        GO_SIGNAL3D_EACHELEMENT_GENERIC (*(T*)__ptr = *data; data+=4, (*s));
        data = (T*)ilGetData() + 2;
        s->setChannel(2);
        GO_SIGNAL3D_EACHELEMENT_GENERIC (*(T*)__ptr = *data; data+=4, (*s));
        data = (T*)ilGetData() + 3;
        s->setChannel(3);
        GO_SIGNAL3D_EACHELEMENT_GENERIC (*(T*)__ptr = *data; data+=4, (*s));
    }
    return true;
}

/**
 * @brief Reads an image file into the provided signal object.
 *
 * The goSignal3D<void> object given as parameter <code>signal</code>
 * determines the type of the loaded data.
 * The image will be converted from the original type to the
 * type given with the signal parameter.
 * The supported image types depend on the types supported by libIL.
 * At the time of this being written, those were
 *     - .bmp
 *     - .cut
 *     - .dcx
 *     - .dds
 *     - .ico
 *     - .jpg
 *     - .lbm
 *     - .lif
 *     - .mdl
 *     - .pcd
 *     - .pcx
 *     - .pic
 *     - .png
 *     - .pnm
 *     - .psd
 *     - .psp
 *     - .raw
 *     - .sgi
 *     - .tga
 *     - .tif
 *     - .wal
 *     - .act
 *     - .pal
 *     - Doom graphics
 *
 * The image type is selected by the filename's suffix.
 * 
 * @param filename  Name of the file containing the image.
 * @param signal    Must be a pointer to a goSignal3D<void>.
 *                  The data type of signal determines
 *                  the type to which the image data are
 *                  converted <b>if</b> the image is not RGB/RGBA.
 *                  In that case, signal will always contain
 *                  GO_UINT8, 4 channel data containing
 *                  red, green, blue, and alpha values.
 *        
 *        
 * \note This method only works when libGo was compiled with 
 *       libIL support (http://openil.sourceforge.net). If not,
 *       it always returns false.
 *
 * \see goSignal3D
 * @return  True if successful, false otherwise.
 **/
bool
goFileIO::readImage (const char* filename, goObjectBase* signal)
{
    if (!signal)
    {
        return false;
    }
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
    ILint imageFormat = ilGetInteger (IL_IMAGE_FORMAT);
    ILint imageType   = ilGetInteger (IL_IMAGE_TYPE);
    ILint imageNumBytes = ilGetInteger (IL_IMAGE_BYTES_PER_PIXEL);
    goString msg = "goFileIO::readImage(): Bytes per pixel: ";
    msg += (int)imageNumBytes;
    goLog::message(msg);

    //  NOTE: It is quietly assumed that the caller provides us with a goSignal3D object.
    /// \todo Add a sanity check here.
    goSignal3D<void>* s = (goSignal3D<void>*)signal;
    switch (imageFormat)
    {
        case IL_LUMINANCE: break;
        case IL_RGBA:
        case IL_RGB:
        case IL_BGR:
        case IL_BGRA:
                imageFormat = IL_RGBA;
                s->setDataType (GO_UINT8);
                break;
        default:
                imageFormat = IL_LUMINANCE;
                break;
    }
   
    if (imageFormat == IL_LUMINANCE)
    {
        switch (imageType)
        {
            case IL_BYTE:           s->setDataType (GO_INT8);   break;
            case IL_UNSIGNED_BYTE:  s->setDataType (GO_UINT8);  break;
            case IL_SHORT:          s->setDataType (GO_INT16);  break;
            case IL_UNSIGNED_SHORT: s->setDataType (GO_UINT16); break;
            case IL_INT:            s->setDataType (GO_INT32);  break;
            case IL_UNSIGNED_INT:   s->setDataType (GO_UINT32); break;
            case IL_FLOAT:          s->setDataType (GO_FLOAT);  break;
            case IL_DOUBLE:         s->setDataType (GO_DOUBLE); break;
            default: 
                    goLog::warning("goFileIO::readImage(): Unknown data type."); break;
        }
    }
    switch (s->getDataType().getID())
    {
        case GO_INT8:
            {
                if (!ILtoGOSIGNAL<goInt8> (imageFormat, IL_BYTE, imageName, width, height, s))
                {
                    return false;
                }
            }
            break;
        case GO_UINT8:
            {
                if (!ILtoGOSIGNAL<goUInt8> (imageFormat, IL_UNSIGNED_BYTE, imageName, width, height,  s))
                {
                    return false;
                }
            }
            break;
        case GO_INT16:
            {
                if (!ILtoGOSIGNAL<goInt16> (imageFormat, IL_SHORT, imageName, width, height, s))
                {
                    return false;
                }
            }
            break;
        case GO_UINT16:
            {
                if (!ILtoGOSIGNAL<goUInt16> (imageFormat, IL_UNSIGNED_SHORT, imageName, width, height, s))
                {
                    return false;
                }
            }
            break;
        case GO_INT32:
            {
                if (!ILtoGOSIGNAL<goInt32> (imageFormat, IL_INT, imageName, width, height, s))
                {
                    return false;
                }
            }
            break;
        case GO_UINT32:
            {
                if (!ILtoGOSIGNAL<goUInt32> (imageFormat, IL_UNSIGNED_INT, imageName, width, height, s))
                {
                    return false;
                }
            }
            break;
        case GO_FLOAT:
            {
                if (!ILtoGOSIGNAL<goFloat> (imageFormat, IL_FLOAT, imageName, width, height, s))
                {
                    return false;
                }
            }
            break;
        case GO_DOUBLE:
            {
                if (!ILtoGOSIGNAL<goDouble> (imageFormat, IL_DOUBLE, imageName, width, height, s))
                {
                    return false;
                }
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

/**
 * @brief  Write an image file from a goSignal3DBase<void> object.
 *
 * This function needs goLib to be compiled with support for the libIL
 * image library.
 * The supported image types depend on the types supported by libIL.
 * At the time of this being written, those were
 * - .bmp
 * - .dds
 * - .jpg
 * - .pcx
 * - .png
 * - .pnm
 * - .raw
 * - .sgi
 * - .tga
 * - .tif
 * - .pal
 *
 * Note that these differ from the types supported for reading.
 * The image type is selected by the filename's suffix.
 * 
 * @param filename  Name for the image file.
 * @param signal    goSignal3DBase<void> to be saved (currently only GO_FLOAT).
 *
 * \note This method only works when libGo was compiled with 
 *       libIL support (http://openil.sourceforge.net). If not,
 *       it always returns false.
 *  
 * \note This does not yet work for multichannel data.
 * 
 * \todo This works only for float signals. Add other types. Add multichannel.
 *
 * @return True if successful, false otherwise.
 **/
#ifdef HAVE_LIBIL
bool
goFileIO::writeImage (const char* filename, const goObjectBase* signal)
{
    if (!signal)
    {
        goLog::warning("goFileIO::writeImage(): no signal.");
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
    
    const goSignal3DBase<void>* s = (const goSignal3DBase<void>*)signal;
    /// \todo FIXME: add other types
    if (s->getDataType().getID() != GO_FLOAT)
    {
        goLog::warning("goFileIO::writeImage(): signal is not float.");
        return false;
    }
    ilTexImage (s->getSizeX(), s->getSizeY(), s->getSizeZ(), 24, IL_LUMINANCE, IL_FLOAT, NULL);
    if (ilGetError() != IL_NO_ERROR)
    {
        ilDeleteImages (1, &imageName);
        goLog::warning("goFileIO::writeImage(): IL ERROR.");
        return false;
    }
    ILfloat* data = (ILfloat*)ilGetData();
    GO_SIGNAL3D_EACHELEMENT_GENERIC (*(data++) = *(const goFloat*)__ptr, (*s));

    // DevILish non-const char*
    goString fname (filename);
    ilSaveImage (fname.getPtr());
    if (ilGetError() != IL_NO_ERROR)
    {
        ilDeleteImages (1, &imageName);
        goLog::warning("goFileIO::writeImage(): IL ERROR.");
        return false;
    }
    ilDeleteImages (1, &imageName);
    return true;
}
#else
bool
goFileIO::writeImage (const char*, const goObjectBase*)
{
    return false;
}
#endif

FILE*
goFileIO::createTempFile (goString& filenameRet)
{
#ifdef HAVE_TMPNAM_R
    filenameRet.resize (L_tmpnam);
    if (tmpnam_r(filenameRet.getPtr()) == NULL)
    {
        return NULL;
    }
    return fopen (filenameRet.toCharPtr(), "w");
#else
    goLog::warning ("goFileIO::createTempFile(): golib was compiled without tmpnam_r().");
    return NULL;
#endif
}

bool
goFileIO::remove (const goString& filename)
{
#ifndef HAVE_UNLINK
    return false;
#else
    if (unlink (filename.toCharPtr()) != 0)
    {
        return false;
    }
    return true;
#endif
}

goSize_t goFileIO::fileSize (const char* filename)
{
    FILE* f = NULL;
    f = fopen (filename, "r");
    if (!f)
        return 0;
    fseek (f, 0, SEEK_END);
    int pos = ftell (f);
    fclose (f);
    return (goSize_t)pos;
}

bool  
goFileIO::readASCII (const char* filename, goString& target)
{
    goSize_t sz = goFileIO::fileSize (filename);
    FILE* f = NULL;
    f = fopen (filename, "r");
    if (!f)
        return false;
    target.resize (sz);
    fread (target.getPtr(), 1, sz, f);
    fclose (f);
    return true;
}

bool  
goFileIO::writeASCII (const char* filename, const goString& str)
{
    FILE* f = NULL;
    f = fopen (filename, "w");
    if (!f)
        return false;
    fwrite (str.getPtr(), 1, str.getSize(), f);
    fclose (f);
    return true;
}

bool
goFileIO::fileExists (const char* filename)
{
#ifdef HAVE_STAT
    struct stat buffer;
    if (stat(filename, &buffer) == -1)
    {
        return false;
    }
    return true;
#else
    goLog::warning ("goFileIO::fileExists(): golib was compiled without HAVE_STAT.");
    return false;
#endif
}
