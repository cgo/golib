/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goconfigfile.h>
#include <fstream>
#include <string.h>
#include <iostream>
#include <ctype.h>

// #ifndef isblank
// #define isblank(c)      __isctype((c), _ISblank)
// #endif

static  unsigned char equal = '=';
static  unsigned char comment = '#';
static  unsigned char section_start = '[';
static  unsigned char section_end   = ']';
static  unsigned char chapter_start = '(';
static  unsigned char chapter_end   = ')';
static  unsigned char string_delim  = (unsigned char) 39;   // '

goConfigFileEntry::
goConfigFileEntry () {
  keyData = "";
  valueData = "";
  keyData.resize (0);
  valueData.resize (0);
}

goConfigFileEntry::
~goConfigFileEntry () {
}

goConfigFileSection::
goConfigFileSection () {
  nameData.resize(0);
  dummy   = "0";
  last_failed = false;
  entries.resize (0);
}
		      
goConfigFileSection::
~goConfigFileSection () {
  goIndex_t i = 0;
  for (i = 0; i < entries.getSize(); i++) {
    delete entries[i];
  }
}

goIndex_t
goConfigFileSection::
find (const goString& keyname) {
  goIndex_t i = 0;
  for (i = 0; i < entries.getSize(); i++) {
    if (entries[i]->key() == keyname) {
      last_failed = false;
      return i;
    }
  }
  last_failed = true;
  return entries.getSize();
}

const goString&
goConfigFileSection::
get (const goString& keyname) {
  goIndex_t pos = find (keyname);
  if (!fail()) {
    return entries[pos]->value();
  }
  return dummy;
}

bool
goConfigFileSection::
add (goConfigFileEntry& entry) {
  goIndex_t pos = find (entry.key());
  
  if (fail()) {
    entries.resize (entries.getSize() + 1);
    pos = entries.getSize() - 1;
    entries[pos] = new goConfigFileEntry;
    last_failed = false;
  }
  if (pos >= entries.getSize()) {
      std::cout << "GOCONFIGFILE ERROR -- ENTRY INDEX TOO LARGE" << std::endl;
    return false;
  }
  if (!entries[pos]) {
      std::cout << "GOCONFIGFILE ERROR -- ENTRY INDEX SEEMS NOT TO EXIST" << std::endl;
    return false;
  }
  *entries[pos]->keyPtr()   = entry.key();
  *entries[pos]->valuePtr() = entry.value();
  return true;
}

bool
goConfigFileSection::
add (goString& keyname, goString& valuename) {
  goConfigFileEntry entry;
  entry.key() = keyname;
  entry.value() = valuename;
  return (add (entry));
}


goConfigFileChapter::
goConfigFileChapter() {
  last_failed 	= false;
  nameData.resize(0);
  dummy 	= "0";
  sections.resize (0);
}

goConfigFileChapter::
~goConfigFileChapter() {
  goIndex_t i = 0;
  for (i = 0; i < sections.getSize(); i++) {
    delete sections[i];
  }
}

bool
goConfigFileChapter::
add (goString& sectionname, goString& keyname, goString& valuename) {
  goIndex_t pos = find (sectionname);
  
  if (fail()) {
    sections.resize (sections.getSize() + 1);
    pos = sections.getSize() - 1;
    sections[pos] = new goConfigFileSection;
    last_failed = false;
    sections[pos]->name() = sectionname;
  }
  if (pos < sections.getSize()) {
    return sections[pos]->add (keyname, valuename);
  } else {
      std::cout << "GOCONFIGFILE ERROR -- SECTION NOT FOUND" << std::endl;
  }
  return false;
}

goIndex_t
goConfigFileChapter::
find (const goString& sectionname) {
  goIndex_t i = 0;
  for (i = 0; i < sections.getSize(); i++) {
    if (sections[i]->name() == sectionname) {
      last_failed = false;
      return i;
    }
  }
  last_failed = true;
  return sections.getSize();
}

const goString&
goConfigFileChapter::
get (const goString& sectionname, const goString& keyname) {
  goIndex_t pos = find (sectionname);
  if (!fail()) {
    return sections[pos]->get (keyname);
  }
  return dummy;
}

goConfigFile::
goConfigFile () {
  last_failed = false;
  dummy = "0";
  chapters.resize (0);
}

goConfigFile::
~goConfigFile () {
  goIndex_t i = 0;
  for (i = 0; i < chapters.getSize(); i++) {
    delete chapters[i];
  }
}

goIndex_t
goConfigFile::
find (const goString& chaptername) {
  goIndex_t i = 0;
  for (i = 0; i < chapters.getSize(); i++) {
    if (chapters[i]->name() == chaptername) {
      last_failed = false;
      return i;
    }
  }
  last_failed = true;
  goIndex_t c = chapters.getSize();
  return ( (c > 0) ? (c - 1) : 0 );
}

bool
goConfigFile::
add (goString& chaptername) {
  goIndex_t pos = find (chaptername);
  
  if (fail()) {
    chapters.resize (chapters.getSize() + 1);	
    pos = chapters.getSize() - 1;
    chapters[pos] = new goConfigFileChapter;
    last_failed = false;
    chapters[pos]->name() = chaptername;
  } 
  return true;
}

bool
goConfigFile::
add (goString& chapter, goString& section, goString& key, goString& value) {
  add (chapter);
  goIndex_t pos = find (chapter);
  if (pos < chapters.getSize()) {
    chapters[pos]->add (section,key,value);
  } else {
      std::cout << "GOCONFIGFILE ERROR -- CHAPTER NOT FOUND" << std::endl;
  }
  return fail();
}


bool
goConfigFile::
add (const char* chapter) {
  goString s = chapter;
  return add (s);
}

bool
goConfigFile::
add (const char* chapter, const char* section, const char* key, const char* value) {
  goString c = chapter, s = section, k = key, v = value;
  return add (c,s,k,v);
}

const goString&
goConfigFile::
get (const goString& chaptername, const goString& sectionname, const goString& keyname) {
  goIndex_t pos = find (chaptername);
  if (!fail()) {
    return chapters[pos]->get (sectionname,keyname);
  }
  return dummy;
}

const goString&
goConfigFile::
get (const char* chapter, const char* section, const char* key) {
  goString c = chapter, s = section, k = key;
  return get (c,s,k);
}

goArray<goConfigFileSection*>&
goConfigFile::
get (goString& chapter) {
  goIndex_t i = find (chapter);
  if (!fail()) {
    return (chapters[i]->getSectionArray());
  }
  return dummyChapter;
}

goArray<goConfigFileSection*>&
goConfigFile::
get (const char* chapter) {
  goString tmp = chapter;
  return get(tmp);
}

bool
goConfigFile::
read (goString& file) {
    std::ifstream f;

  unsigned char c = 0;
  
  goIndex_t	line = 1;
  bool		skipBlanks = true;   // whether to skip blanks in values
  goString 	chapter="", section="", key="", value="";
  
  enum {
    start_line,
    skip_line,
    read_chapter,
    read_section,
    read_key,
    read_value,
    read_comment,
    error,
    end
  } state;
  state = start_line;
  
  f.open (file.toCharPtr());
  if (f.fail()) {
    last_failed = true;
    return false;
  }
  
  while (!f.eof()) {
    c = f.get();
    switch (state) {
    case start_line:
      // cout << "Starting line " << line << endl;
      if (isblank(c)) {
	break;
      }
      if (c == '\n') {
	line++;
	break;
      }
      if (c == chapter_start) {
	state = read_chapter;
	chapter.resize(0);
	break;
      }
      if (c == section_start) {
	state = read_section;
	section.resize(0);
	break;
      }
      if (c == comment) {
	state = read_comment;
	break;
      }
      state = read_key;
      f.unget();
      key.resize(0);
      break;
    case read_chapter:
      if (c == '\n') {
	line++;
	state = start_line;
	break;
      }
      if (c == chapter_end) {
	state = skip_line;
	add (chapter);
	break;
      }
      chapter += c;
      break;
    case read_section:
      if (c == '\n') {
	line++;
	state = start_line;
	break;
      }
      if (c == section_end) {
	state = skip_line;
	break;
      }
      section += c;
      break;
    case read_key:
      if (c == '\n') {
	line++;
	state = start_line;
	break;
      }
      if (c == equal) {
	if (key.getSize() == 0) { state = error; break; }
	state = read_value;
	value.resize(0);
	break;
      }
      if (isblank(c)) {
	break;
      }
      key += c;
      break;
    case read_value:
      if (c == '\n') {
	line++;
	state = start_line;
	add ((goString&)chapter,(goString&)section,(goString&)key,(goString&)value);
	// cout << "Added " << chapter << ":" << section << ":" << key << ":" << value << endl;
	break;
      }
      if (c == string_delim) {
	skipBlanks = !skipBlanks;
	break;
      }
      if (skipBlanks && isblank(c)) {
	break;
      }	
      value += c;
      break;
    case read_comment:
      state = skip_line;
      break;
    case skip_line:
      if (c == '\n') {
	line++;
	state = start_line;
      }
      break;
    case error:
      std::cout << "Error in line " << line << " -- restarting line.\n";
      state = start_line;
      break;
    case end:
      // f.close();
      break;
    }
  }
  // cout << "Closing file." << endl;
  f.close();
  last_failed = false;
  return true;
}

bool
goConfigFile::
read (const char* file) {
  if (strlen(file) > 0) {
    goString f = file;
    return read (f);
  }
  last_failed = true;
  return false;
}

bool
goConfigFile::
write (goString& file) {
    std::ofstream f;

  f.open (file.toCharPtr());
  if (f.fail()) {
    return false;
  }
  goArray<goConfigFileEntry*> tmpEntries;
  int i,i2,i3;
  for (i = 0; i < chapters.getSize(); i++) {
    //cout << "i = " << i << "\n";
    if (chapters[i]) { 
      //cout << chapters[i]->name() << "\n";
      f << chapter_start << chapters[i]->name() << chapter_end << "\n";
      for (i2 = 0; i2 < chapters[i]->getSectionArray().getSize(); i2++) {
	//cout << "i2 = " << i2 << "\n";
	if (chapters[i]->getSectionArray()[i2]) { 
	  //cout << chapters[i]->getSectionArray()[i2]->name() << "\n";
	  f << section_start << chapters[i]->getSectionArray()[i2]->name() << section_end << "\n";
	  tmpEntries = chapters[i]->getSectionArray()[i2]->getEntries();
	  for (i3 = 0; i3 < tmpEntries.getSize(); i3++) {
	    if (tmpEntries[i3]) {
	      //cout << tmpEntries[i3]->key() << "=" << tmpEntries[i3]->value() << "\n";
	      f << tmpEntries[i3]->key() << equal << string_delim << tmpEntries[i3]->value() << string_delim << "\n";
	    }
	  }
	}
      }
    }
  }
  f.close();
  return true;
}

bool
goConfigFile::
write (const char* file) {
  goString f = file;
  return write (f);
}

goArray<goConfigFileChapter*>&
goConfigFile::
getChapterArray () {
  return chapters;
}







