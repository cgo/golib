#include <godirectory.h>

#ifdef HAVE_DIRENT_H

goDirectory::goDirectory () {
  last_failed = false;
}

goDirectory::~goDirectory () {
}

void
goDirectory::open (goString& s) {
  open (s.toCharPtr());
}

void
goDirectory::open (const char* s) {
  last_failed = false;
  dir = opendir (s);
  if (!dir) {
    last_failed = true;
    return;
  }
  dirName = s;
  
  readEntries();
}

void
goDirectory::close () {
  last_failed = false;
  closedir (dir);
}


void
goDirectory::readEntries () {
  for (int i = 0; i < entries.getSize(); i++) {
    delete entries[i];
  }
  entries.resize (0);
  types.resize (0);
  struct dirent* d;
  while ( (d = readdir (dir)) != NULL ) {
    entries.resize (entries.getSize() + 1);
    entries[entries.getSize() - 1] = new goString (d->d_name);
    types.resize (types.getSize() + 1);
    types[types.getSize() - 1] = d->d_type;
  }
}

bool
goDirectory::isUnknown (int index) {
  if (types[index] == DT_UNKNOWN) {
    return true;
  }
  return false;
}

bool
goDirectory::isDir (int index) {
    // struct stat statbuf;
  
  /*
  stat (entries[index]->toCharPtr(), &statbuf);
  if (S_ISDIR(statbuf.st_mode)) {
    return true;
  }
  return false;
  */
  if (types[index] == DT_DIR) {
    return true;
  }
  return false;
}

bool
goDirectory::isFile (int index) {
    //  struct stat statbuf;

  /*
  stat (entries[index]->toCharPtr(), &statbuf);
  if (S_ISREG(statbuf.st_mode)) {
    return true;
  }
  return false;
  */

  if (types[index] == DT_REG) {
    return true;
  }
  return false;
}

bool
goDirectory::isLink (int index) {
    //  struct stat statbuf;

  /*
  stat (entries[index]->toCharPtr(), &statbuf);
  if (S_ISLNK(statbuf.st_mode)) {
    return true;
  }
  return false;
  */

  if (types[index] == DT_LNK) {
    return true;
  }
  return false;
}

bool
goDirectory::test (const char* dir_name) {
  DIR* t = opendir (dir_name);
  if (t == NULL) {
    closedir (t);
    return false;
  }
  closedir (t);
  return true;
}

#endif /* HAVE_DIRENT_H */









