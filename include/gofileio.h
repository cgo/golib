#ifndef GOFILEIO_H
#define GOFILEIO_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gotypes.h>
#include <gosignal2d.h>
#include <gostring.h>

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
   */
  static void readPGM (goString& filename, goSignal2D<goInt32>*& signal);
  ///
  static void readPGM (const char* filename, goSignal2D<goInt32>*& signal);

  ///
  static void readJPEG (const char* filename, goSignal2D<goInt32>*& signal);
  /*!
   * Writes the signal as 8 bit raw PGM file. The signal should contain only values 
   * up to 255, since the values are simply casted to goInt8.
   */
  static void writePGM (goString& filename, goSignal2D<goInt32>& signal);
  ///
  static void writePGM (const char* filename, goSignal2D<goInt32>& signal);
};
/*!
 * @}
 */

#endif
