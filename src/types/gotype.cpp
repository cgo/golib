#include <gotype.h>
#include <assert.h>

class goTypePrivate
{
    public:
        goTypePrivate (goTypeEnum t);
        ~goTypePrivate ();

    public:
        goTypeEnum typeEnum;
        goSize_t   size;       // in bytes
        bool       signedness;
        goString   string;     // descriptive string
};

goTypePrivate::goTypePrivate (goTypeEnum t)
    : typeEnum (t),
      size     (goType::getSize (t)),
      signedness (goType::isSigned (t)),
      string     ("")
{
    goType::getString (t, string);
}

goTypePrivate::~goTypePrivate ()
{
}

/**
 * @brief Constructor.
 *
 * @param t  Type enumerator for this object. See goTypeEnum
 **/
goType::goType (goTypeEnum t)
    : goObjectBase (),
      myPrivate    (NULL)
{
    setClassName ("goType");
    myPrivate = new goTypePrivate (t);
    assert (myPrivate);
}

goType::~goType ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = NULL;
    }
}

/**
 * @brief Get size of type t.
 *
 * @param t  Type enumerator.
 *
 * @return The size of the type t in bytes.
 **/
goSize_t 
goType::getSize (goTypeEnum t)
{
    switch (t)
    {
        case GO_INT8:
            {
                return sizeof (goInt8);
            }
            break;
        case GO_INT16:
            {
                return sizeof (goInt16);
            }
            break;
        case GO_INT32:
            {
                return sizeof (goInt32);
            }
            break;
        case GO_INT64:
            {
                return sizeof (goInt64);
            }
            break;
        case GO_UINT8:
            {
                return sizeof (goUInt8);
            }
            break;
        case GO_UINT16:
            {
                return sizeof (goUInt16);
            }
            break;
        case GO_UINT32:
            {
                return sizeof (goUInt32);
            }
            break;
        case GO_FLOAT:
            {
                return sizeof (goFloat);
            }
            break;
        case GO_DOUBLE:
            {
                return sizeof (goDouble);
            }
            break;
        case GO_VOID_POINTER:
            {
                return sizeof (void*);
            }
            break;
        default:
            {
                assert ("UNKNOWN TYPE ENUM" == NULL);
            }
            break;
    }
    return 0;
}

/**
 * @brief Returns signedness of type t.
 *
 * @param t  Type enumerator.
 *
 * @return True if t is signed, false otherwise.
 **/
bool
goType::isSigned (goTypeEnum t)
{
    switch (t)
    {
        case GO_INT8:
        case GO_INT16:
        case GO_INT32:
        case GO_INT64:
        case GO_FLOAT:
        case GO_DOUBLE:
            {
                return true;
            }
            break;
        case GO_UINT8:
        case GO_UINT16:
        case GO_UINT32:
        case GO_VOID_POINTER:
            {
                return false;
            }
            break;
        default:
            {
                assert ("UNKNOWN TYPE ENUM" == NULL);
            }
            break;
    }
    return false;
}

/**
 * @brief Get description string for type t.
 *
 * @param t          Type enumerator.
 * @param stringRet  String containing the description after the method
 *                   returned.
 **/
void 
goType::getString (goTypeEnum t, goString& stringRet)
{
    switch (t)
    {
        case GO_INT8:
            {
                stringRet = "signed integer 8 bit";
            }
            break;
        case GO_INT16:
            {
                stringRet = "signed integer 16 bit";
            }
            break;
        case GO_INT32:
            {
                stringRet = "signed integer 32 bit";
            }
            break;
        case GO_INT64:
            {
                stringRet = "signed integer 64 bit";
            }
            break;
        case GO_UINT8:
            {
                stringRet = "unsigned integer 8 bit";
            }
            break;
        case GO_UINT16:
            {
                stringRet = "unsigned integer 16 bit";
            }
            break;
        case GO_UINT32:
            {
                stringRet = "unsigned integer 32 bit";
            }
            break;
        case GO_FLOAT:
            {
                stringRet = "float";
            }
            break;
        case GO_DOUBLE:
            {
                stringRet = "double";
            }
            break;
        case GO_VOID_POINTER:
            {
                stringRet = "void pointer";
            }
            break;
        default:
            {
                assert ("UNKNOWN TYPE ENUM" == NULL);
                stringRet = "unknown type";
            }
            break;
    }
}

/**
 * @brief Get size of this type.
 *
 * @return Size of the type set in this object.
 **/
goSize_t
goType::getSize () const
{
    assert (myPrivate);
    return myPrivate->size;
}

/**
 * @brief Query signedness of this type.
 *
 * @return True if the type set in this object is 
 *         signed, false otherwise.
 **/
bool
goType::isSigned () const
{
    assert (myPrivate);
    return myPrivate->signedness;
}

/**
 * @brief Get description string of this type.
 *
 * @return Const reference to a goString containing the description
 *         for the type set in this object.
 **/
const goString&
goType::getString () const
{
    assert (myPrivate);
    return myPrivate->string;
}

/**
 * @brief Get enumerator for the type set in this object.
 *
 * @return The enumerator t of the type set in the constructor
 *         goType(t).
 **/
goTypeEnum
goType::getID () const
{
    assert (myPrivate);
    return myPrivate->typeEnum;
}
