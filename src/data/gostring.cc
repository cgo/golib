#include <gostring.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>

goString::
goString() {
  thisString.resize (1);
  thisString[0] = 0;
  dummyChar = 0;
  charPtr = (char*) malloc ( (sizeof(char)) * 1);
  charPtr[0] = 0;
}

goString::
goString(const char* n) {
  thisString.resize (1);
  thisString[0] = 0;
  dummyChar = 0;
  charPtr = (char*) malloc ( (sizeof(char)) * 1);
  charPtr[0] = 0;
  
  (*this) = n;
}

goString::
goString(const goString& n) {
    thisString.resize (1);
    thisString[0] = 0;
    dummyChar = 0;
    charPtr = (char*) malloc ( (sizeof(char)) * 1);
    charPtr[0] = 0;
    
    (*this) = n;
}

char&
goString::operator[](goIndex_t i) {
  if (i < thisString.getSize()) {
    return thisString[i];
  }
  return dummyChar;
}

const char&
goString::operator[](goIndex_t i) const 
{
    if (i < thisString.getSize()) 
    {
        return thisString[i];
    }
    return (char&)dummyChar;
}

/*
unsigned char& 
goString::operator[](goIndex_t i) const {
  if (i < thisString.getSize()) {
    return thisString[i];
  }
  return (unsigned char&)dummyChar;
}
*/

goString::
~goString() {
  /* Hopefully, we don't need this any longer */
  // if (charPtr) {
    // free (charPtr);
    // cout << "FIX FREE() PROBLEM IN GOSTRING.CC !!!" << endl;
  // }
}

goIndex_t
goString::getSize () const {
  return (thisString.getSize() - 1);
}

/*
goIndex_t
goString::getSize () const {
  goIndex_t retval = thisString.getSize();
  return retval;
}
*/

void
goString::resize (goIndex_t newsize) {
  thisString.resize (newsize + 1);
  thisString[newsize] = 0; // add terminating \0 character
}

/**
 * @brief Get the file name portion of a 
 *        string describing a file name with full path.
 *
 * @param fileNameRet  Contains the file name portion.
 **/
void
goString::getFileName (goString& fileNameRet) const
{
    fileNameRet = "";
    if (this->getSize() == 0)
    {
        return;
    }
    goIndex_t i = this->findLast ('/');   // FIXME: This is platform dependent.
    if (i < (this->getSize()-1))
    {
        ++i;
        this->copy (fileNameRet, i, this->getSize()-1);
    }
}

/**
 * @brief Get the path portion of a 
 *        string describing a file name with full path.
 *
 * @param pathRet  Contains the path portion of the full file name.
 **/
void
goString::getPathName (goString& pathRet) const
{
    pathRet = "";
    if (this->getSize() == 0)
    {
        return;
    }
    goIndex_t i = this->findLast ('/');   // FIXME: This is platform dependent.
    if (i >= 0 && i < this->getSize())
    {
        this->copy (pathRet, 0, i);
    }
}

/**
 * @brief Copy a part of this string to another string.
 *
 * @param target  Target string.
 * @param start   Start index in this string.
 * @param end     End index in this string.
 *
 * @return True if successful, false otherwise.
 **/
bool
goString::copy (goString& target, goIndex_t start, goIndex_t end) const
{
    if (start >= 0 && start < this->getSize() &&
        end >= 0 && end >= start && end < this->getSize())
    {
        target.resize (end - start + 1);
        goIndex_t i;
        memcpy (target.getPtr(), this->getPtr() + start, sizeof(char) * target.getSize());
        return true;
    }
    return false;
}

goIndex_t 
goString::findFirst (char c) const
{
    if (this->getSize() == 0)
    {
        return -1;
    }
    goIndex_t i = 0;
    while (i < this->getSize() && (*this)[i] != c)
        ++i;
    return i;
}

goIndex_t 
goString::findLast (char c) const
{
    if (this->getSize() == 0)
    {
        return -1;
    }
    goIndex_t i = this->getSize() - 1;
    while (i >= 0 && (*this)[i] != c)
        --i;
    return i;
}

int
goString::toInt () const 
{
  goIndex_t i = 0;
  char tmp[255];
  memset (tmp,0,255);
  for (i = 0; (i < this->getSize()) &&
	      ( i < 255); i++) {
    tmp[i] = (*this)[i];
  }
  return atoi(tmp);
}

float
goString::toFloat () const
{
  goIndex_t i = 0;
  char tmp[255];
  memset (tmp,0,255);
  for (i = 0; (i < this->getSize()) &&
	      ( i < 255); i++) {
    tmp[i] = (*this)[i];
  }
  return (float)atof(tmp);
}

double
goString::toDouble () const 
{
  goIndex_t i = 0;
  char tmp[255];
  memset (tmp,0,255);
  for (i = 0; (i < this->getSize()) &&
	 ( i < 255); i++) {
    tmp[i] = (*this)[i];
  }
  return (double)atof(tmp);
}

bool
goString::toBool () const 
{
  if (this->getSize() >= 1) {
    if ((*this)[0] == '0') {
      return false;
    }
    else return true;
  }
  return false;	
}


const char*
goString::toCharPtr () const {
  if (!thisString.getPtr()) {
    return &dummyChar; 
  }
  // char c = 0;
  // thisString += c;
  return thisString.getPtr();
  /*
  goIndex_t i = 0;
  if (thisString.getSize() > 0) {
    charPtr = (char*) realloc ((char*) charPtr, (thisString.getSize() + 1) * sizeof(char));
    for (i = 0; i < thisString.getSize(); i++) {
      charPtr[i] = (char)thisString[i];
    }
    charPtr[i] = 0;
    return (const char*) charPtr;
  } else { 
    if (charPtr) {
      charPtr[0] = 0;
      return charPtr;
    }
    return &dummyChar;
  }
  */
}

goDate&
goString::toDate () {
  char tmp[5];
  goIndex_t i     = 0,
            tmp_i = 0;

  while ( (i < getSize()) && ((*this)[i] != '.') &&
	  (tmp_i < 2) ) {
    tmp[tmp_i] = (*this)[i];
    i++; tmp_i++;
  }
  tmp[tmp_i] = 0;
  thisDate.setDay(atoi(tmp));

  tmp_i = 0;
  i++;
  while ( (i < getSize()) && ((*this)[i] != '.') &&
	  (tmp_i < 2) ) {
    tmp[tmp_i] = (*this)[i];
    i++; tmp_i++;
  }
  tmp[tmp_i] = 0;
  thisDate.setMonth(atoi(tmp));

  tmp_i = 0;
  i++;
  while ( (i < getSize()) && ((*this)[i] != '.') &&
	  (tmp_i < 4) ) {
    tmp[tmp_i] = (*this)[i];
    i++; tmp_i++;
  }
  tmp[tmp_i] = 0;
  thisDate.setYear (atoi(tmp));
  return thisDate;
}


void
goString::toUpper () {
  for (int i = 0; i < getSize(); i++) {
    operator[](i) = toupper (operator[](i));
  }
}

void
goString::toLower () {
  for (int i = 0; i < getSize(); i++) {
    operator[](i) = tolower (operator[](i));
  }
}

goString&
goString::operator= (const goString& other) {
  goIndex_t i;
  goIndex_t s = other.getSize();
  this->resize (s);
  assert (s == this->getSize());
  for (i = 0; i < s; i++) {
    (*this)[i] = other[i];
  }
  return *this;
}


goString&
goString::operator= (const char* other) {
  goIndex_t i;
  this->resize (strlen(other));
  goIndex_t s = this->getSize();
  for (i = 0; i < s; i++) {
    (*this)[i] = other[i];
  }
  return *this;
}

goString&
goString::operator= (goDate& d) {
  char *s;
  s = (char*) calloc (255,sizeof (char));
  this->resize (0);
  sprintf (s,"%d.%d.%d",(int)(d.getDay()), (int)(d.getMonth()), (int)(d.getYear()) );
  (*this) = s;
  free ((void*)s);
  return (*this);
}


goString&	  
goString::operator+= (const char c) {
  char c2 = c;
  // thisString.resize(thisString.getSize() + 1);
  thisString[thisString.getSize() - 1] = c2;
  c2 = 0;
  thisString += c2;
  return (*this);
}

goString&
goString::operator+= (goString& s) {
  goIndex_t i = 0;
  for (i = 0; i < s.getSize(); i++) { 
    (*this) += s[i];
  }
  return (*this);
}

goString&
goString::operator+= (const char* s) {
  goString str = s;
  (*this) += str;
  return (*this);
}

goString&
goString::operator+= (int i) {
    char s[250];
    sprintf(&s[0], "%d", i);
    (*this) += &s[0];
    return (*this);
}

goString&
goString::operator+= (float i) {
    char s[250];
    sprintf(&s[0], "%f", i);
    (*this) += &s[0];
    return (*this);
}

bool
operator== (const goString& str,const char* s) {
  goIndex_t i = 0;
  if (strlen (s) != (unsigned)str.getSize()) {
    return false;
  } else {
    for (i = 0; i < str.getSize(); i++) {
      if ( str[i] != s[i]) {
	return false;
      }	
    }
  }
  return true;
}

bool
operator== (const goString& str, const goString& str2) {
  goString tmpStr = str2.toCharPtr();
  return (str == tmpStr);
}

bool	  
operator== (const goString& str, goString& str2) {
  goIndex_t i = 0;
  if (str.getSize() != str2.getSize()) { return false; }
  for (i = 0; i < str.getSize(); i++) {
    if (str[i] != str2[i]) { return false; }
  }
  return true;
}

bool
operator!= (const goString& str,const char* s) {
  return !operator==(str,s);
}

bool
operator!= (const goString& str, const goString& str2) {
  return !operator==(str,str2);
}

bool	  
operator!= (const goString& str, goString& str2) {
  return !operator==(str,str2);
}

std::ostream&
operator<< (std::ostream& o, const goString& s) {
  goIndex_t i;
  if (s.getSize() > 0) {
    for (i = 0; i < s.getSize(); i++) {
      o << s[i];
    }
  }
  return o;
}
