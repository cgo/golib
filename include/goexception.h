#ifndef GOEXCEPTION_H
#define GOEXCEPTION_H

#include <iostream>
#include <gostring.h>


/*!
 * \brief Exception base class.
 *
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

#endif
