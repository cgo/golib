#include <gostring.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

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
  unsigned int i;
  dummyChar = 0;
  unsigned int s = (unsigned int)strlen(n);
  charPtr = 0;
  if (s > 0) {
    thisString.resize((goIndex_t)s + 1);
    for (i = 0; i < s; i++) {
      thisString[i] = n[i];
    }
    thisString[i] = 0;
    charPtr = (char*) malloc ( (sizeof(char)) * 1);
    charPtr[0] = 0;
  } else {
    charPtr = 0;
  } 
  
}

char&
goString::operator[](goIndex_t i) {
  if (i < thisString.getSize()) {
    return thisString[i];
  }
  return dummyChar;
}

char&
goString::operator[](goIndex_t i) const {
  if (i < thisString.getSize()) {
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

goIndex_t
goString::getSize () {
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

int
goString::toInt () {
  goIndex_t i = 0;
  char tmp[255];
  memset (tmp,0,255);
  for (i = 0; (i < this->getSize()) &&
	      ( i < 255); i++) {
    tmp[i] = (*this)[i];
  }
  return atoi(tmp);
}

int
goString::toInt () const {
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
goString::toFloat () {
  goIndex_t i = 0;
  char tmp[255];
  memset (tmp,0,255);
  for (i = 0; (i < this->getSize()) &&
	      ( i < 255); i++) {
    tmp[i] = (*this)[i];
  }
  return (float)atof(tmp);
}

float
goString::toFloat () const {
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
goString::toDouble () {
  goIndex_t i = 0;
  char tmp[255];
  memset (tmp,0,255);
  for (i = 0; (i < this->getSize()) &&
	 ( i < 255); i++) {
    tmp[i] = (*this)[i];
  }
  return (double)atof(tmp);
}

double
goString::toDouble () const {
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
goString::toBool () {
  if (this->getSize() >= 1) {
    if ((*this)[0] == '0') {
      return false;
    }
    else return true;
  }
  return false;	
}

bool
goString::toBool () const {
  if (this->getSize() >= 1) {
    if ((*this)[0] == '0') {
      return false;
    }
    else return true;
  }
  return false;	
}


const char*
goString::toCharPtr () {
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

const char*
goString::toCharPtr () const {
  if (!thisString.getPtr()) {
    return &dummyChar; 
  }
  // char c = 0;
  // thisString += c;
  return thisString.getPtr();
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
goString::operator= (goString& other) {
  goIndex_t i;
  this->resize (other.getSize());
  for (i = 0; i < this->getSize(); i++) {
    (*this)[i] = other[i];
  }
  return *this;
}


goString&
goString::operator= (const goString& other) {
  goIndex_t i;
  goIndex_t s = other.getSize();
  this->resize (s);
  for (i = 0; i < this->getSize(); i++) {
    (*this)[i] = other[i];
  }
  return *this;
}


goString&
goString::operator= (const char* other) {
  goIndex_t i;
  this->resize (strlen(other));   
  for (i = 0; i < this->getSize(); i++) {
    (*this)[i] = other[i];
  }
  return *this;
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

/*
goString&
goString::operator+= (const goString& s) {
  goIndex_t i = 0;
  for (i = 0; i < s.getSize(); i++) { 
    (*this) += s[i];
  }
  return (*this);
}
*/ 

/* THIS IS NOT NEEDED
bool
goString::operator== (const char* s) {
  goIndex_t i = 0;
  if (strlen (s) < this->getSize()) {
    return false;
  } else {
    for (i = 0; i < this->getSize(); i++) {
      if ( (*this)[i] != s[i]) {
	return false;
      }	
    }
  }
  return true;
}	
*/

bool
operator== (goString& str,const char* s) {
  goIndex_t i = 0;
  if (strlen (s) < (unsigned)str.getSize()) {
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
operator== (goString& str, const goString& str2) {
  goString tmpStr = str2.toCharPtr();
  return (str == tmpStr);
}

bool	  
operator== (goString& str, goString& str2) {
  goIndex_t i = 0;
  if (str.getSize() != str2.getSize()) { return false; }
  for (i = 0; i < str.getSize(); i++) {
    if (str[i] != str2[i]) { return false; }
  }
  return true;
}

ostream&
operator<< (ostream& o, goString& s) {
  goIndex_t i;
  if (s.getSize() > 0) {
    for (i = 0; i < s.getSize(); i++) {
      o << s[i];
    }
  }
  return o;
}

/*
ostream&
operator<< (ostream& o, const goString& s) {
  goIndex_t i;
  if (s.getSize() > 0) {
    for (i = 0; i < s.getSize(); i++) {
      o << s[i];
    }
  }
  return o;
}
*/



