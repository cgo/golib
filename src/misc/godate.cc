/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */



#include <godate.h>
#include <string.h>
#include <gostring.h>

goDate::goDate () {
  day = 1;
  month = 1;
  year = 1999;
}

goDate::goDate (goInt16 Day,
		goInt16 Month,
		goInt16 Year) {
  if ( (Day > 0) && (Day < 32) ) {
    day = Day;
  }
  if ( (Month > 0) && (Month < 13) ) {
    month = Month;
  }
  if ( Year > 0 ) {
    year = Year;
  }
}

goDate::~goDate ()
{
}

bool
goDate::operator< (const goDate& other) const {

  if ( year < other.getYear()) {
    return true;
  } else {
    if ( (year == other.getYear()) && (month < other.getMonth()) ) {
      return true;
    } else {
      if ( (month == other.getMonth()) && (day < other.getDay()) ) {
	return true;
      }
    }
  }
  return false;
}	

bool
goDate::operator== (const goDate& other) const {
  if ( (day == other.getDay()) && (month == other.getMonth()) && (year == other.getYear()) ) {
    return true;
  }
  return false;
}

bool
goDate::operator!= (const goDate& other) const {
  return !( (*this) == other );
}

bool
goDate::operator> (const goDate& other) const {
  if ( !(*this < other) && !(*this == other) ) {
    return true;
  }
  return false;
}

bool
goDate::operator>= (const goDate& other) const {
  if ( (*this > other) || (*this == other) ) {
    return true;
  }
  return false;
}

bool
goDate::operator<= (const goDate& other) const {
  if ( (*this < other) || (*this == other) ) {
    return true;
  }
  return false;
}

void
goDate::operator= (const goDate& other) {
  setDay   (other.getDay());
  setMonth (other.getMonth());
  setYear  (other.getYear());
}

void
goDate::operator= (const char* s) {
  goIndex_t i = 0, sizeS;
  goString tmp;

  sizeS = strlen (s);

  enum {DAY,MONTH,YEAR,DONE,FAULT} state;
  const unsigned char dot = '.';
  state = DAY;
  tmp.resize(0);
  while (i < sizeS) {
    switch (state) {
      case DAY: while ( (i < sizeS) &&
			s[i] != dot) { tmp+=s[i];
				       i++;
			}
                if (s[i] == dot) state = MONTH;
                  else state = FAULT;
                break;
      case MONTH: 
      this->setDay (tmp.toInt());
      tmp.resize(0);
      i++;
      while ( (i < sizeS) &&
	      s[i] != dot) {
		tmp+=s[i++];
	      }
      if (s[i] == dot) state = YEAR;
       else state = FAULT;
      break;
      case YEAR:
      this->setMonth (tmp.toInt());
      tmp.resize(0);
      i++;
      while ( i < sizeS ) {
		tmp+=s[i++];
	      }
      state = DONE;
      break;
      case FAULT: break;
      case DONE: break;
    }
  }	
}	

std::ostream&
operator<< (std::ostream& outstream, goDate& d) {
  outstream << (int)(d.getDay()) << "." << (int)(d.getMonth()) << "." << (int)(d.getYear());
  return outstream;
}



