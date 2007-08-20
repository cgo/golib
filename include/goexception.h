#ifndef GOEXCEPTION_H
#define GOEXCEPTION_H

#include <iostream>

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
        EXISTS,
        UNEXPECTED_DATA
    };

    goFileIOException (int _code = FAILED) : goException(), code (_code) {};
    virtual ~goFileIOException () {};

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
    virtual ~goTypeException () {};

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
        virtual ~goMathException () {};

        int code;
};

/*!
 * @}
 */

#endif
