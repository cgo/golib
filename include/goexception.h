#ifndef GOEXCEPTION_H
#define GOEXCEPTION_H

#include <iostream>
#include <gostring.h>


/*!
 * @author Christian Gosch
 */
class goException
{
 public:
  goException();
  virtual ~goException();
  
  virtual void print();
};

class goExceptionString : public goException
{
 public:
    goExceptionString (goString &s) : st(s) { }
    goExceptionString (const char* s) : st(s) { }
    virtual ~goExceptionString();
    virtual void print();
    
    goString st;
};

/*!
 * \example exception.cc
 * Example and test program for the goException class.
 * @author Christian Gosch
 */
#endif

