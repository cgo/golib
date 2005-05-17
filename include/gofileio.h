#ifndef GOFILEIO_H
#define GOFILEIO_H

#ifdef HAVE_CONFIG_H
#include <goconfig.h>
#endif

#include <gotypes.h>
#include <gostring.h>
#ifndef GOSIGNAL3D_H
# include <gosignal3d.h>
#endif

class goObjectBase;

/*!
 * \addtogroup misc
 * @{
 */
/*!
 * \brief Reading/writing specific file formats.
 *
 * Provides file reading/writing facilities. <br>
 * This class contains static members to read and write various file formats into 
 * different data types. Currently, the only files handled are 8 bit raw PGM files.
 */
class goFileIO {
 public:
  /*!
   * Reads a PGM (8 bit raw) file into the signal. Signal is newly created.
   * \todo Fix readPGM etc. for signal3d
   */
//  static void readPGM (goString& filename, goSignal2D<goInt32>*& signal);
  ///
//  static void readPGM (const char* filename, goSignal2D<goInt32>*& signal);

  ///
//  static void readJPEG (const char* filename, goSignal2D<goInt32>*& signal);
  /*!
   * Writes the signal as 8 bit raw PGM file. The signal should contain only values 
   * up to 255, since the values are simply casted to goInt8.
   */
//  static void writePGM (goString& filename, goSignal2D<goInt32>& signal);
  ///
//  static void writePGM (const char* filename, goSignal2D<goInt32>& signal);

  /** @addtogroup signal */
  /** @{ */
  static bool  readImage  (const char* filename, goSignal3D<void>* signal);
  static bool  writeImage (const char* filename, const goObjectBase* signal);
  /** @} */
  static FILE* createTempFile (goString& filenameRet);
  static bool  remove         (const goString& filename);
  static goSize_t fileSize    (const char* filename);
  static bool  readASCII      (const char* filename, goString& target);
  static bool  writeASCII     (const char* filename, const goString& str);
  static bool  fileExists     (const char* filename);
};
/*!
 * @}
 */
#endif
