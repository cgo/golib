#ifndef GOFILEIO_H
#define GOFILEIO_H

#include <gotypes.h>
#include <gosignal2d.h>
#include <gostring.h>

/*!
 * Provides file reading/writing facilities. <br>
 * This class contains static members to read and write various file formats into 
 * different data types. Currently, the only files handles are 8 bit raw PGM files.
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
  static goSignal2D<goInt32>*
    readPGM (const char* filename);
  /*!
   * Writes the signal as 8 bit raw PGM file. The signal should contain only values 
   * up to 255, since the values are simply casted to goInt8.
   */
  static void writePGM (goString& filename, goSignal2D<goInt32>& signal);
  ///
  static void writePGM (const char* filename, goSignal2D<goInt32>& signal);
};

#endif
