/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOCONFIGFILEACCESS_H
#define GOCONFIGFILEACCESS_H


#include <goconfigfile.h>
#include <gostring.h>

/**
 * Provides some functionality for a goConfigFile.
 * @see goConfigFile
 * @author Christian Gosch
 */
class goConfigFileAccess : public goConfigFile {
 public:
  ///
  goConfigFileAccess ();
  ///
  virtual ~goConfigFileAccess ();

  /// Assumes all entries with name entryName to be floats and adds them up (ALL of them)
  float		sum (goString& entryName);
  /// Adds up all entries called entryName (assuming float values) in all sections in chapter chapterName.
  float		sum (goString& chapterName, goString& entryName);

 protected:
  ///
  float		floatValue;
};

#endif /* GOCONFIGFILEACCESS_H */
