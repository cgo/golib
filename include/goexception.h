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

class goFileIOException : public goException
{
    public:
    enum
    {
        NOT_FOUND,
        FAILED,
        EXISTS
    };

    goFileIOException (int _code = FAILED) : goException(), code (_code) {};
    ~goFileIOException () {};

    int code;
};

class goTypeException : public goException
{
    public:
    enum
    {
        UNKNOWN_TYPE,
        WRONG_TYPE
    };
    goTypeException (int t = WRONG_TYPE) : goException(), code (t) {};
    ~goTypeException () {};

    int code;
};

class goNullException
{
};

class goMathException : public goException
{
    public:
        enum
        {
            SIZE_MISMATCH,
            OTHER
        };
        goMathException (int t = OTHER) : goException(), code (t) {};
        ~goMathException () {};

        int code;
};

/*!
 * @}
 */

#endif
