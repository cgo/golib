#include <goexception.h>
#include <gostring.h>

goException::goException()
{
}

goException::~goException()
{
}

void
goException::print () 
{
  std::cout << "libGo exception" << std::endl;
}

#if 0
goExceptionString::goExceptionString (const goString& s)
    : st (s)
{
}

goExceptionString::goExceptionString (const char* s)
    : st (s)
{
}

goExceptionString::~goExceptionString()
{
    st.resize(0);
}

void
goExceptionString::print()
{
    goException::print();
    std::cout << "String: " << st << std::endl;
}
#endif
