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

  goInt16 getDay   () { return day; }
  goInt16 getMonth () { return month; }
  goInt16 getYear  () { return year; }
  
  void 	setDay   (goInt16 d) { day = d; }
  void	setMonth (goInt16 m) { month = m; }
  void 	setYear  (goInt16 y) { year = y; }

  bool operator<  (goDate& other);
  bool operator>  (goDate& other);
  bool operator<= (goDate& other);
  bool operator>= (goDate& other);
  bool operator== (goDate& other);
  bool operator!= (goDate& other);
  void operator=  (goDate& other);
  void operator=  (const char* s);
  //friend operator++ ();
  //friend operator-- ();	
  friend std::ostream& operator<< (std::ostream& outstr, goDate& d);
  
 protected:
  goInt16 day;
  goInt16 month;
  goInt16 year;	
};
/*!
 * @}
 */

#endif 















