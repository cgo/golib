/* --*- C++ -*-- */


#ifndef __GOSTRING_H__
#define __GOSTRING_H__

#include <goarray.h>
#include <ostream.h>
#include <gotypes.h>
#include <godate.h>
//#include <linux/types.h>

/** 
 * String class 
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
  goIndex_t getSize ();
  ///
  goIndex_t getSize () const;
  ///
  void      resize (goIndex_t newsize);

  ///
  int         toInt ();
  ///
  int         toInt () const;
  ///
  float	      toFloat ();
  ///
  float	      toFloat () const;
  ///
  double      toDouble ();
  ///
  double      toDouble () const;
  ///
  bool	      toBool ();
  ///
  bool	      toBool () const;
  ///
  const char* toCharPtr ();
  ///
  const char* toCharPtr () const;
  ///
  goDate&     toDate ();

  ///
  void		toUpper ();
  ///
  void		toLower ();

  ///
  char&		operator[] (goIndex_t i);
  ///
  char&         operator[] (goIndex_t i) const;
  ///
  goString& 	  operator=  (goString& other);
  ///
  goString& 	  operator=  (const goString& other);
  ///
  goString&	  operator=  (const char* other);
  ///
  goString&	  operator=  (goDate& d);
  ///
  goString&	  operator+= (const char c);
  ///
  goString&	  operator+= (goString& s);
  ///
  goString&	  operator+= (const char* s);

  // ONLY USE THIS IF YOU KNOW EXACTLY WHAT YOU ARE DOING. 
  // ALWAYS AVOID USING THIS METHOD IF THERE'S A WAY TO DO IT IN A CLEAN WAY!
  char*		  getPtr () { return thisString.getPtr(); }
  
  // goString&	  operator+= (const goString& s);
  // bool		  operator== (const char* s);
  friend bool	  operator== (goString& str,const char* s);
  ///
  // friend bool	  operator== (const goString& str,const char* s);
  ///
  friend bool	  operator== (goString& str, goString& str2);
  ///
  friend bool	  operator== (goString& str, const goString& str2);
  ///
  friend bool	  operator!= (goString& str,const char* s);
  ///
  // friend bool	  operator!= (const goString& str,const char* s);
  ///
  friend bool	  operator!= (goString& str, goString& str2);
  ///
  friend bool	  operator!= (goString& str, const goString& str2);
  ///
  friend ostream& operator<< (ostream& o,goString& s);
  ///
  friend ostream& operator<< (ostream& o, const goString& s);
  ///
  // friend ostream& operator<< (ostream& o,const goString& s);

protected:
  ///
  goArray<char>  thisString;
  ///
  char*			  charPtr;
  ///
  char                    dummyChar;
  ///
  goDate		  thisDate;
};

#endif /* __GOSTRING_H__ */





