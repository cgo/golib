/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


/*
 * goDate | Provides simple date handling.
 * Dates can be compared ( <, >, == ), copied ( = ) and written to a string.
 */

#ifndef __GO_DATE_H__
#define __GO_DATE_H__

#include <gotypes.h>
#include <iostream>
/*!
 * \addtogroup misc
 * @{
 */
/*!
 * \brief Simple date handling.
 * 
 * Dates can be compared ( <, >, == ), copied ( = ) and written to a string.
 * \author Christian Gosch
 */
class goDate 
{
 public:
  goDate ();
  goDate (goInt16 Day,
	      goInt16 Month,
	      goInt16 Year);
  ~goDate ();

  goInt16 getDay   () const { return day; }
  goInt16 getMonth () const { return month; }
  goInt16 getYear  () const { return year; }
  
  void 	setDay   (goInt16 d) { day = d; }
  void	setMonth (goInt16 m) { month = m; }
  void 	setYear  (goInt16 y) { year = y; }

  bool operator<  (const goDate& other) const;
  bool operator>  (const goDate& other) const;
  bool operator<= (const goDate& other) const;
  bool operator>= (const goDate& other) const;
  bool operator== (const goDate& other) const;
  bool operator!= (const goDate& other) const;
  void operator=  (const goDate& other);
  void operator=  (const char* s);
  //friend operator++ ();
  //friend operator-- ();	
  friend std::ostream& operator<< (std::ostream& outstr, const goDate& d);
  
 protected:
  goInt16 day;
  goInt16 month;
  goInt16 year;	
};
/*!
 * @}
 */

#endif 















