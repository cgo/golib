/* --*- C++ -*-- */


#ifndef GOSTRING_H
#define GOSTRING_H

#include <goarray.h>
#include <iostream>
#include <gotypes.h>
#include <godate.h>
#ifndef GOLIST_H
# include <golist.h>
#endif
//#include <linux/types.h>

/** @addtogroup data
 * @{
 */
/** 
 * @brief String class 
 */
class goString {
public:
  ///
  goString  ();
  ///
  goString  (const char* n);
  ///
  goString  (const goString& n);
  ///
  ~goString ();

  ///
  goIndex_t getSize () const;
  ///
  void      resize (goIndex_t newsize);

  void      getPathName (goString& pathRet) const;
  void      getFileName (goString& fileNameRet) const;
  goIndex_t findFirst   (char c, goIndex_t start = 0) const;
  goIndex_t findLast    (char c) const;
  goIndex_t find        (const char* str, goIndex_t start = 0);
  goIndex_t replace     (const char* str, const char* replacement);
  goIndex_t getLine     (goString& ret, goIndex_t start);
  bool      copy        (goString& target, goIndex_t start, goIndex_t end) const;
  bool      getWords    (goList<goString>& wordsRet) const;
  
  void      fill        (char c);
  
  ///
  int         toInt () const;
  ///
  goUInt32    toUInt32 () const;
  ///
  goUInt64    toUInt64 () const;
  ///
  float	      toFloat () const;
  ///
  double      toDouble () const;
  ///
  bool	      toBool () const;
  ///
  const char* toCharPtr () const;
  ///
  goDate      toDate ();

  ///
  void		toUpper ();
  ///
  void		toLower ();

  ///
  char&		operator[] (goIndex_t i);
  ///
  const char& operator[] (goIndex_t i) const;
  ///
  goString& 	  operator=  (const goString& other);
  ///
  goString&	  operator=  (const char* other);
  ///
  goString&	  operator=  (goDate& d);
  ///
  goString&	  operator+= (const char c);
  ///
  goString&	  operator+= (const goString& s);
  ///
  goString&	  operator+= (const char* s);
  ///
  goString&   operator+= (int i);
  ///
  goString&   operator+= (goUInt32 i);
  ///
  goString&   operator+= (goUInt64 i);
  ///
  goString&   operator+= (float f);

  // ONLY USE THIS IF YOU KNOW EXACTLY WHAT YOU ARE DOING. 
  // ALWAYS AVOID USING THIS METHOD IF THERE'S A WAY TO DO IT IN A CLEAN WAY!
  char*		  getPtr () { return thisString.getPtr(); }
  const char* getPtr () const { return thisString.getPtr(); }
  
  // goString&	  operator+= (const goString& s);
  // bool		  operator== (const char* s);
  friend bool	  operator== (const goString& str,const char* s);
  ///
  // friend bool	  operator== (const goString& str,const char* s);
  ///
  friend bool	  operator== (const goString& str, const goString& str2);
  ///
  friend bool	  operator!= (const goString& str,const char* s);
  ///
  // friend bool	  operator!= (const goString& str,const char* s);
  ///
  friend bool	  operator!= (const goString& str, const goString& str2);
  ///
  friend std::ostream& operator<< (std::ostream& o, const goString& s);
  ///
  // friend ostream& operator<< (ostream& o,const goString& s);

protected:
  ///
  goArray<char>  thisString;
  ///
  char            dummyChar;
};
/** @} */
#endif 





