/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
#ifndef GOEXCEPTION_H
# include <goexception.h>
#endif
#include <stdio.h>   // FILE*

#ifndef GOMATRIX_H
# include <gomatrix.h>
#endif

class goObjectBase;

/*!
 * \addtogroup misc
 * @{
 */
/*!
 * \brief Reading/writing files and some utility functions.
 *
 * Provides file reading/writing facilities. <br>
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
  static bool  readImage  (const char* filename, goSignal3D<void>* signal, bool linear = false) throw (goFileIOException, goTypeException);
  static bool  writeImage (const char* filename, const goSignal3DBase<void>* signal) throw (goFileIOException, goTypeException);
  /** @} */
  template <class T> static bool  writeBinaryMatrix (const goMath::Matrix<T>&, FILE* f);
  template <class T> static bool  writeBinaryMatrix (const goMath::Matrix<T>&, const char* filename);
  static FILE* createTempFile (goString& filenameRet);
  static bool  remove         (const goString& filename);
  static goSize_t fileSize    (const char* filename);
  static bool  readASCII      (const char* filename, goString& target);
  static bool  readASCII      (FILE* f, goString& target, goSize_t sz);
  static bool  readASCIIMax   (FILE* f, goString& target, goSize_t max);
  static bool  readASCII      (FILE* f, goString& target);
  static bool  readASCIILine  (FILE* f, goString& target);
  static bool  writeASCII     (const char* filename, const goString& str);
  static bool  writeASCII     (FILE* f, const goString& str);
  static bool  fileExists     (const char* filename);
  static bool  mkdir          (const char* pathname);
};
/*!
 * @}
 */
#endif
