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
  std::cout << "libGo exception" << endl;
}


goExceptionString::~goExceptionString()
{
    st.resize(0);
}

void
goExceptionString::print()
{
    goException::print();
    std::cout << "String: " << st << endl;
}
