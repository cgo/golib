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

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif

#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif

#ifdef HAVE_LIBIL
# include <IL/il.h>
# include <IL/ilu.h>
#endif

#ifndef GOSIGNAL3D_H
# include <gosignal3d.h>
#endif

#ifndef GOSIGNALMACROS_H
# include <gosignalmacros.h>
#endif

#include <gosignal3dgenericiterator.h>

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
        s->make (width, height, 1, 32, 32, 1, 32, 32, 0, 4);
    }
    else
    {
        s->make (width, height, 1, 32, 32, 1, 32, 32, 0, 1);
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
        s->setChannel(0);
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
goFileIO::readImage (const char* filename, goSignal3D<void>* signal) throw (goFileIOException, goTypeException)
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
        throw goFileIOException(goFileIOException::FAILED);
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
    if (goString(signal->getClassName()) != "goSignal3D")
    {
        goString msg = "goFileIO::readImage(): signal must be a goSignal3D but is ";
        msg += signal->getClassName();
        msg += ". Not loading.";
        goLog::warning (msg);
        return false;
    }
    goSignal3D<void>* s = signal;
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
                    goLog::warning("goFileIO::readImage(): Unknown data type."); 
                    throw goTypeException(goTypeException::UNKNOWN_TYPE); break;
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
                throw goTypeException(goTypeException::UNKNOWN_TYPE);
                return false;
            }
            break;
    }
    ilDeleteImages (1, &imageName);
    return true;
}
#else
bool
goFileIO::readImage (const char*, goSignal3D<void>*) throw (goFileIOException, goTypeException)
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
template <class T, class ilT, int iltype>
static bool copySignalToIL (const goSignal3DBase<void>* s)
{
    if (s->getChannelCount() == 1)
    {
        ilTexImage (s->getSizeX(), s->getSizeY(), s->getSizeZ(), sizeof(ilT), IL_LUMINANCE, iltype, NULL);
        if (ilGetError() != IL_NO_ERROR)
        {
            goLog::warning("goFileIO::writeImage(): IL ERROR.");
            throw goFileIOException(goFileIOException::FAILED);
            return false;
        }
        ilT* data = (ilT*)ilGetData();
        GO_SIGNAL3D_EACHELEMENT_GENERIC (*(data++) = *(const T*)__ptr, (*s));
        return true;
    }
    if (s->getChannelCount() == 4)
    {
        ilTexImage (s->getSizeX(), s->getSizeY(), s->getSizeZ(), sizeof(ilT) * 4, IL_RGBA, iltype, NULL);
        if (ilGetError() != IL_NO_ERROR)
        {
            goLog::warning("goFileIO::writeImage(): IL ERROR.");
            throw goFileIOException(goFileIOException::FAILED);
            return false;
        }
        goIndex_t chan = s->getChannel();
        const_cast<goSignal3DBase<void>*>(s)->setChannel(0);
        ilT* data = (ilT*)ilGetData();
        //= NOTE: This assumes the channel data are stored linearly for each element.
        goSignal3DGenericConstIterator it (s);
        while (!it.endZ())
        {
            it.resetY();
            while (!it.endY())
            {
                it.resetX();
                while (!it.endX())
                {
                    *data = *(const T*)*it;
                    *(data + 1) = *((const T*)*it + 1);
                    *(data + 2) = *((const T*)*it + 2);
                    *(data + 3) = *((const T*)*it + 3);
                    data += 4;
                    it.incrementX();
                }
                it.incrementY();
            }
            it.incrementZ();
        }
        const_cast<goSignal3DBase<void>*>(s)->setChannel(chan);
        return true;
    }
    if (s->getChannelCount() == 3)
    {
        ilTexImage (s->getSizeX(), s->getSizeY(), s->getSizeZ(), sizeof(ilT) * 4, IL_RGBA, iltype, NULL);
        if (ilGetError() != IL_NO_ERROR)
        {
            goLog::warning("goFileIO::writeImage(): IL ERROR.");
            throw goFileIOException(goFileIOException::FAILED);
            return false;
        }
        goIndex_t chan = s->getChannel();
        const_cast<goSignal3DBase<void>*>(s)->setChannel(0);
        ilT* data = (ilT*)ilGetData();
        //= NOTE: This assumes the channel data are stored linearly for each element.
        goSignal3DGenericConstIterator it (s);
        while (!it.endZ())
        {
            it.resetY();
            while (!it.endY())
            {
                it.resetX();
                while (!it.endX())
                {
                    *data = *(const T*)*it;
                    *(data + 1) = *((const T*)*it + 1);
                    *(data + 2) = *((const T*)*it + 2);
                    *(data + 3) = 0;
                    data += 4;
                    it.incrementX();
                }
                it.incrementY();
            }
            it.incrementZ();
        }
        const_cast<goSignal3DBase<void>*>(s)->setChannel(chan);
        return true;
    }
    return false;
}

bool
goFileIO::writeImage (const char* filename, const goSignal3DBase<void>* signal) throw (goFileIOException, goTypeException)
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
    //= Trying iluFlipImage() instead below ...
//    ilEnable (IL_ORIGIN_SET);
//    ilOriginFunc (IL_ORIGIN_LOWER_LEFT);
    ILuint imageName = 0;
    ilGenImages (1, &imageName);
    ilBindImage (imageName);
    
    const goSignal3DBase<void>* s = (const goSignal3DBase<void>*)signal;
    /// \todo FIXME: add other types
//    if (s->getDataType().getID() != GO_FLOAT)flip
//    {
//        goLog::warning("goFileIO::writeImage(): signal is not float.");
//        throw goTypeException(goTypeException::WRONG_TYPE);
//        return false;
//    }

    bool ok = false;
    try 
    {
        switch (s->getDataType().getID())
        {
            case GO_INT8: ok = copySignalToIL<goInt8, ILbyte, IL_BYTE> (s); break;
            case GO_UINT8: ok = copySignalToIL<goUInt8, ILubyte, IL_UNSIGNED_BYTE> (s); break;
            case GO_INT16: ok = copySignalToIL<goInt16, ILshort, IL_SHORT> (s); break;
            case GO_UINT16: ok = copySignalToIL<goUInt16, ILushort, IL_UNSIGNED_SHORT> (s); break;
            case GO_INT32: ok = copySignalToIL<goInt32, ILint, IL_INT> (s); break;
            case GO_UINT32: ok = copySignalToIL<goUInt32, ILuint, IL_UNSIGNED_INT> (s); break;
            case GO_FLOAT: ok = copySignalToIL<goFloat, ILfloat, IL_FLOAT> (s); break;
            case GO_DOUBLE: ok = copySignalToIL<goDouble, ILdouble, IL_DOUBLE> (s); break;
            default:
                goLog::warning ("goFileIO::writeImage(): unknown data type.");
                return false;
                break;
        }
    }
    catch (goFileIOException ex)
    {
        ilDeleteImages (1, &imageName);
        return false;
    }
    

    iluFlipImage();
    // DevILish non-const char*
    goString fname (filename);
    ilSaveImage (fname.getPtr());
    ILenum er = ilGetError();
    if (er != IL_NO_ERROR)
    {
        ilDeleteImages (1, &imageName);
        goString msg = "goFileIO::writeImage(): IL ERROR.\n";
        msg += iluErrorString (er);
        goLog::warning(msg);
        if (er == IL_FILE_ALREADY_EXISTS)
        {
            throw goFileIOException(goFileIOException::EXISTS);
        }
        throw goFileIOException(goFileIOException::FAILED);
        return false;
    }
    ilDeleteImages (1, &imageName);
    return true;
}
#else
bool
goFileIO::writeImage (const char*, const goSignal3DBase<void>*) throw (goFileIOException, goTypeException)
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
goFileIO::readASCII (FILE* f, goString& target, goSize_t sz)
{
    if (!f)
        return false;
    target.resize (sz);
    if (fread (target.getPtr(), 1, sz, f) != sz)
        return false;
    return true;
}

/**
* @brief Reads up to <code>max</code> characters or until 0.
*
* The trailing 0 will be dropped.
* 
* @param f  
* @param target  
* @param max  
*
* @return True if successful, false otherwise.
**/
bool  
goFileIO::readASCIIMax (FILE* f, goString& target, goSize_t max)
{
    if (!f)
        return false;
    goSize_t n = 0;
    goSize_t total = 0;
    char c = 0;
    while (true && total < max)
    {
        n = fread(&c, sizeof(char), 1, f);
        if (n != 1)
            return false;
        if (c == 0)
            return true;
        target += c;
        total += n;
    }
    return true;
}

/** 
 * @brief Read a line of ascii text.
 * 
 * @note The trailing newline is overwritten with a 0 character.
 * 
 * @todo Uses a gnu extension function. Replace by something own 
 * some time (very low priority).
 * 
 * @param f FILE pointer.
 * @param target Target string.
 * 
 * @return 
 */
bool
goFileIO::readASCIILine (FILE* f, goString& target)
{
    //= This is a gnu extension. Use fgets() on systems without gnu libc.
    char* lineptr = 0;
    size_t N = 0;
    ssize_t numRead = getline (&lineptr, &N, f);
    if (-1 == numRead)
    {
        return false;
    }
    lineptr[numRead-1] = 0;
    target = lineptr;
    free(lineptr);
    lineptr = 0;
    return true;
}

/**
 * @brief Read until 0 is read or nothing else can be read.
 *
 * The trailing 0 will be dropped.
 * 
 * @param f  FILE*
 * @param target  String is stored here
 *
 * @return  True if string was read an trailed by 0, false otherwise.
 **/
bool  
goFileIO::readASCII (FILE* f, goString& target)
{
    if (!f)
        return false;
    goSize_t n = 0;
    char c = 0;
    while (true)
    {
        n = fread(&c, sizeof(char), 1, f);
        if (n != 1)
            return false;
        if (c == 0)
            return true;
        target += c;
    }
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
goFileIO::writeASCII (FILE* f, const goString& str)
{
    if (!f)
        return false;
    if (fwrite (str.getPtr(), 1, str.getSize(), f) != (size_t)str.getSize())
        return false;
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

bool
goFileIO::mkdir (const char* pathname)
{
    if (!pathname)
        return false;
#ifdef HAVE_MKDIR
    int retval = ::mkdir (pathname, S_IRUSR | S_IWUSR | S_IXUSR | S_IRGRP | S_IWGRP | S_IXGRP);
    switch (retval)
    {
        case 0:
            return true;
            break;
        default:
            {
                if (errno == EEXIST)
                {
                    return true;
                }
                return false;
            }
            break;
    }
#endif
    return false;
}
