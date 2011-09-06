/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef __GOCONFIGFILE_H__
#define __GOCONFIGFILE_H__

#include <gotypes.h>
#include <goarray.h>
#include <gostring.h>

///
class goConfigFileEntry {
public:
  ///
  goConfigFileEntry ();
  ///
  virtual ~goConfigFileEntry ();
  ///
  goString&	key () { return keyData; }
  ///
  goString&	value () { return valueData; }

  ///
  goString*	keyPtr () { return &keyData; }
  ///
  goString*	valuePtr () { return &valueData; }
protected:
  ///
  goString 	keyData;
  ///
  goString 	valueData;
};

///
class goConfigFileSection {
public:
  ///
  goConfigFileSection ();
  ///
  virtual ~goConfigFileSection ();
  ///
  goString&	name () { return nameData; }
  ///  
  bool		add  (goConfigFileEntry& entry);
  ///
  bool		add  (goString& keyname, goString& valuename);
  ///
  goIndex_t	find (const goString& keyname);
  ///
  // goString&	get  (goString& keyname);
  ///
  const goString& get (const goString& keyname);
  ///
  bool		fail () { return last_failed; }
  ///
  goArray<goConfigFileEntry*>&
    getEntries () { return entries; }

protected:
  ///
  bool				last_failed;
  ///
  goString 			nameData;
  ///
  goString			dummy;
  ///
  goArray<goConfigFileEntry*>	entries;
};

///
class goConfigFileChapter {
 public:
  ///
  goConfigFileChapter();
  ///
  virtual ~goConfigFileChapter();
  ///
  goString&	name () { return nameData; }
  ///
  bool		add  (goString& sectionname, goString& keyname, goString& valuename);
  ///
  goIndex_t	find (const goString& sectionname);
  ///
  const goString& get  (const goString& sectionname, const goString& keyname);
  ///
  bool		fail () { return last_failed; }
  ///
  goArray<goConfigFileSection*>&
    getSectionArray() { return sections; }


 protected:
  ///
  bool				last_failed;
  ///
  goString			nameData;
  ///
  goString			dummy;
  ///
  goArray<goConfigFileSection*>	sections;
};

/**
 * "Standard" config file.
 * This is a file which may contain chapters, sections and key/value pairs as follows:

  \begin{verbatim}
  # comment [...]
  (CHAPTERNAME)
  [SECTIONNAME]
  KEYNAME = VALUE
  \end{verbatim}
 *
 * The values are interpreted as strings. If they are not delimited by 's, 
 * any blank characters are removed.
 * Upper-/Lowercase characters are distinguished.
 */
class goConfigFile {
public:
  ///
  goConfigFile ();
  ///
  virtual ~goConfigFile ();
  ///
  goIndex_t	find (const goString& chaptername);
  
  ///
  bool		add  (goString& chaptername);
  ///
  bool		add  (const char* chapter);
  ///
  bool		add  (goString& chaptername, goString& sectionname, 
		      goString& keyname, goString& valuename);
  ///
  bool		add  (const char* chapter, const char* sectionname,
		      const char* key, const char* value);
  ///
  const goString& get  (const goString& chaptername, const goString& sectionname, const goString& keyname);
  ///
  const goString& get  (const char* chapter, const char* section, const char* key);
  ///
  goArray<goConfigFileSection*>&
    get	(goString& chapter);
  ///
  goArray<goConfigFileSection*>&
    get	(const char* chapter);

  ///

  goArray<goConfigFileChapter*>&
    getChapterArray ();
  bool		fail () { return last_failed; }

  ///
  bool		read  (goString& file);
  ///
  bool		read  (const char* file);
  ///
  bool		write (goString& file);
  ///
  bool		write (const char* file);

protected:
  ///
  bool				last_failed;
  ///
  goArray<goConfigFileChapter*>	chapters;
  ///
  goArray<goConfigFileSection*> dummyChapter;
  ///
  goString			dummy;

  /**
   * Control of output using the operator<<
   * (not yet implemented)
   */
  bool				out_chapters, out_sections, out_names;
  /** Control of output using the operator<<
   * @see out_chapters
   */
  int				out_mode;
};


#endif /* __GOCONFIGFILE_H__ */
