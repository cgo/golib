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
