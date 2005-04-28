#ifndef GODIRECTORY_H
#define GODIRECTORY_H

#include <goconfig.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#include <gostring.h>
#include <goarray.h>

/*! 
 * \addtogroup system
 * @{
 */
/**
 * \brief Handling of file directories.
 * @author Christian Gosch 
 */
class goDirectory {
 public:
  ///
  goDirectory ();
  ///
  virtual ~goDirectory ();

  ///
  void	open (goString& s);
  ///
  void	open (const char* s);
  ///
  void	close ();
  ///
  void	readEntries ();
  /// Returns true if last operation failed.
  bool	fail () { return last_failed; }

  ///
  goArray<goString*>&	getEntries () { return entries; }
  ///
  bool	isUnknown (int index);
  /// This does not work all times, uses S_ISDIR POSIX macro.
  bool	isDir (int index);
  /// This does not work all times, uses S_ISREG POSIX macro.
  bool	isFile (int index);
  /// This does not work all times, uses S_ISLNK POSIX macro.
  bool	isLink (int index);

  /// Returns true if dir_name can be accessed as a directory.
  static bool test (const char* dir_name);
 protected:
  ///
  bool			last_failed;
  ///
  DIR*			dir;
  ///
  goString		dirName;
  ///
  goArray<goString*>	entries;
  ///
  goArray<int>		types;
};
/*!
 * @}
 */


#endif




