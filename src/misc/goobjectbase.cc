#include <goobjectbase.h>

const char*
goObjectBase::getClassName()
{
    return className.toCharPtr();
}

void
goObjectBase::setClassName (const char* name)
{
    className = name;
}

void
goObjectBase::setClassName (goString& name)
{
    className = name;
}

void
goObjectBase::printClassMessage (const char* msg)
{
    std::cout << getClassName() << " message: " << msg << std::endl;
}

void
goObjectBase::printClassMessage (goString& msg)
{
    printClassMessage (msg.toCharPtr());
}
