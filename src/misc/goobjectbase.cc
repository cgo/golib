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
    cout << getClassName() << " message: " << msg << endl;
}

void
goObjectBase::printClassMessage (goString& msg)
{
    printClassMessage (msg.toCharPtr());
}
