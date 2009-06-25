#include <gostring.h>
#include <goconfig.h>
#include <golog.h>
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
}

goString::
goString(const char* n) {
  thisString.resize (1);
  thisString[0] = 0;
  dummyChar = 0;
  
  (*this) = n;
}

goString::
goString(const goString& n) {
    thisString.resize (1);
    thisString[0] = 0;
    dummyChar = 0;
    
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
~goString() 
{
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
 * @brief Find occurrence of str.
 * 
 * @param str   String to search for.
 * @param start Start index.
 * 
 * @return Start index of str in this string. -1 on failure.
 */
goIndex_t goString::find (const char* str, goIndex_t start)
{
    goIndex_t i = 0;
    goIndex_t j = 0;
    goIndex_t sz = static_cast<goIndex_t> (this->getSize());
    goIndex_t sz_str = strlen(str);
    goIndex_t currentStart = start;
    while (currentStart <= sz - sz_str)
    {
        i = findFirst (str[0], currentStart);
        j = 0;
        while ((i+j) < sz && j < sz_str)
        {
            if (str[j] != (*this)[i+j])
            {
                break;
            }
            ++j;
        }
        if (j == sz_str)
        {
            //= Found!
            return i;
        }
        ++currentStart;
    }
    //= Failure
    return -1;
}

/**
 * @brief 
 *
 * @todo Inefficient. Rework when there's need and time.
 * 
 * @param str  
 * @param replacement  
 *
 * @return 
 **/
goIndex_t goString::replace (const char* str, const char* replacement)
{
    assert (str && replacement);
    if (strlen(str) == 0)
    {
        return 0;
    }
    goString temp;
    goString result = "";
    goIndex_t startPosition = 0;
    goIndex_t i = 0;
    goIndex_t sz_str = strlen(str);
    goIndex_t count = 0;
    goIndex_t lastFoundIndex = 0;
    while (i >= 0 && (startPosition + sz_str) <= this->getSize())
    {
        i = this->find (str, startPosition);
        if (i >= 0)
        {
            //= Somewhat inefficient, but ok for now.
            if (i > 0)
            {
                this->copy (temp, startPosition, i-1);
                result += temp.toCharPtr();
            } 
            result += replacement;
            ++count;
            lastFoundIndex = i;
            startPosition = i + sz_str;
        }
    }
    //= Copy the rest
    if (count > 0)
    {
        if (lastFoundIndex < (this->getSize() - sz_str))
        {
            this->copy (temp, lastFoundIndex + sz_str, this->getSize() - 1);
            result += temp.toCharPtr();
        }
        *this = result;
    }
    return count;
}

goIndex_t goString::getLine (goString& ret, goIndex_t start)
{
    goIndex_t i = this->findFirst ('\n', start);
    if (i < 0)
    {
        ret = "";
        return this->getSize() + 1;
    }
    if (i >= this->getSize())
    {
        this->copy (ret, start, this->getSize() - 1);
        return i;
    }
    if (i == start)
    {
        ret = "";
    }
    else
    {
        this->copy (ret, start, i - 1);
    }
    return i;
}

#if 0
bool goString::getLine (goIndex_t n, goString& ret)
{
    goIndex_t start = 0;
    goIndex_t lastStart = 0;
    goIndex_t line = 0;
    goIndex_t i;
    i = this->findFirst ('\n', start);
    if (i >= this->getSize())
    {
        if (n == 0)
        {
            ret = *this;
            return true;
        }
        else
        {
            return false;
        }
    }
    for (line = 0; line <= n && start < this->getSize(); ++line)
    {
        i = this->findFirst ('\n', start);
        if (i < 0)
        {
            ret = "";
            return false;
        }
        lastStart = start;
        start = i+1;
    }
    printf ("getSize() == %d\n", this->getSize());
    printf ("line      == %d\n", line);
    printf ("lastStart == %d\n", lastStart);
    printf ("i         == %d\n", i);
    if (line < n+1)
    {
        ret = "";
        return false;
    }
    if (line == n+1)
    {
        i = this->findFirst ('\n', lastStart);
        if (i < 0)
        {
            ret = "";
            return false;
        }
        if (i >= this->getSize())
        {
            this->copy (ret, lastStart, this->getSize() - 1);
            return true;
        }
        this->copy (ret, lastStart, i - 1);
        return true;
    }
    return false;
}
#endif

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
        memcpy (target.getPtr(), this->getPtr() + start, sizeof(char) * target.getSize());
        return true;
    }
    return false;
}

bool
goString::getWords (goList<goString>& wordsRet) const
{
    goIndex_t index = 0;
    goIndex_t sz = static_cast<goIndex_t>(this->getSize());
    while (index < sz)
    {
        while (index < sz && (*this)[index] == ' ' || (*this)[index] == '\n') 
        {
            ++index;
        }
        goIndex_t indexStart = index;
        while (index < sz && (*this)[index] != ' ' && (*this)[index] != '\n')
        {
            ++index;
        }
        goString word;
        this->copy (word,indexStart,index-1);
        if (word.getSize() > 0)
        {
            wordsRet.append(word);
        }
    }
    return true;
}

void
goString::fill (char c)
{
    if (!this->getPtr())
    {
        return;
    }
    memset (this->getPtr(), (int)c, sizeof(char) * this->getSize());
}

goIndex_t 
goString::findFirst (char c, goIndex_t start) const
{
    if (this->getSize() == 0)
    {
        return -1;
    }
    goIndex_t i = start;
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

goUInt32
goString::toUInt32 () const
{
    goUInt32 ret = 0;
    sscanf (this->toCharPtr(), "%u", &ret);
    return ret;
}

goUInt64
goString::toUInt64 () const
{
    goUInt64 ret = 0;
#if SIZEOF_LONG_INT == 8
    sscanf (this->toCharPtr(), "%lu", &ret);
#elif SIZEOF_LONG_LONG_INT == 8
    sscanf (this->toCharPtr(), "%llu", &ret);
#else
    goLog::error ("goString::toUInt64(): neither long int nor long long int are 8 bytes long.");
#endif
    return ret;
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
goString::toCharPtr () const 
{
    if (!thisString.getPtr()) 
    {
        return &dummyChar; 
    }
    return thisString.getPtr();
}

goDate
goString::toDate () {
  char tmp[5];
  goDate thisDate;
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
goString::operator+= (const goString& s) {
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
goString::operator+= (goUInt32 i) {
    char s[250];
    sprintf(&s[0], "%u", i);
    (*this) += &s[0];
    return (*this);
}

goString&
goString::operator+= (goUInt64 i) {
    char s[250];
#if SIZEOF_LONG_INT == 8
    sprintf(&s[0], "%lu", i);
#elif SIZEOF_LONG_LONG_INT == 8
    sprintf(&s[0], "%llu", i);
#else
    goLog::error ("goString::operator+= (goUInt64): neither long int nor long long int are 8 bytes long.");
    return *this;
#endif
    (*this) += &s[0];
    return *this;
}

goString&
goString::operator+= (float i) {
    char s[250];
    sprintf(&s[0], "%f", i);
    (*this) += &s[0];
    return (*this);
}

goString&
goString::operator+= (double i) {
    char s[250];
    sprintf(&s[0], "%lf", i);
    (*this) += &s[0];
    return (*this);
}

goString goString::operator+ (const goString& o) const
{
    goString ret = *this;
    ret += o;
    return ret;
}

goString goString::operator+ (const char* s) const
{
    goString ret = *this;
    ret += s;
    return ret;
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
operator== (const goString& str, const goString& str2) 
{
  goIndex_t i = 0;
  if (str.getSize() != str2.getSize()) { return false; }
  goIndex_t sz = str.getSize();
  for (i = 0; i < sz; ++i) 
  {
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
