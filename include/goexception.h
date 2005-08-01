#ifndef GOEXCEPTION_H
#define GOEXCEPTION_H

#include <iostream>
#include <gostring.h>


/*!
 * \addtogroup misc
 * @{
 */
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

class goMemoryException
{
};

class goFileIOException
{
    public:
    enum
    {
        NOT_FOUND,
        FAILED,
        EXISTS
    };

    goFileIOException (int _code = FAILED) : code (_code) {};
    ~goFileIOException () {};

    int code;
};

class goTypeException
{
    public:
    enum
    {
        UNKNOWN_TYPE,
        WRONG_TYPE
    };
    goTypeException (int t = WRONG_TYPE) : code (t) {};
    ~goTypeException () {};

    int code;
};

class goNullException
{
};

class goMathException
{
    public:
        enum
        {
            SIZE_MISMATCH,
            OTHER
        };
        goMathException (int t = OTHER) : code (t) {};
        ~goMathException () {};

        int code;
};

/*!
 * @}
 */

#endif
