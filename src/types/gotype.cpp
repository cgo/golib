#include <gotype.h>
#include <assert.h>
#include <limits>

template <class T> static bool lowerThanFunction   (const void* v1, const void* v2);
template <class T> static bool greaterThanFunction (const void* v1, const void* v2);
template <class T> static bool equalFunction       (const void* v1, const void* v2);

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

        goCompareFunction lowerThan;
        goCompareFunction equal;
        goCompareFunction greaterThan;

        goDouble minimum;
        goDouble maximum;
};

goTypePrivate::goTypePrivate (goTypeEnum t)
    : typeEnum     (t),
      size         (goType::getSize (t)),
      signedness   (goType::isSigned (t)),
      string       (""),
      lowerThan    (NULL),
      greaterThan  (NULL),
      equal        (NULL),
      minimum      (0.0),
      maximum      (1.0)
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
    setID (t);
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

bool
goType::setID (goTypeEnum t)
{
    switch (t)
    {
        case GO_INT8:
            {
                myPrivate->size        = sizeof (goInt8);
                myPrivate->signedness  = true;
                myPrivate->string      = "signed integer 8 bit";
                myPrivate->lowerThan   = lowerThanFunction<goInt8>;
                myPrivate->greaterThan = greaterThanFunction<goInt8>;
                myPrivate->equal       = equalFunction<goInt8>;
                myPrivate->minimum     = std::numeric_limits<goInt8>::min();
                myPrivate->maximum     = std::numeric_limits<goInt8>::max();
            }
            break;
        case GO_INT16:
            {
                myPrivate->size        = sizeof (goInt16);
                myPrivate->signedness  = true;
                myPrivate->string      = "signed integer 16 bit";
                myPrivate->lowerThan   = lowerThanFunction<goInt16>;
                myPrivate->greaterThan = greaterThanFunction<goInt16>;
                myPrivate->equal       = equalFunction<goInt16>;
                myPrivate->minimum     = std::numeric_limits<goInt16>::min();
                myPrivate->maximum     = std::numeric_limits<goInt16>::max();
            }
            break;
        case GO_INT32:
            {
                myPrivate->size        = sizeof (goInt32);
                myPrivate->signedness  = true;
                myPrivate->string      = "signed integer 32 bit";
                myPrivate->lowerThan   = lowerThanFunction<goInt32>;
                myPrivate->greaterThan = greaterThanFunction<goInt32>;
                myPrivate->equal       = equalFunction<goInt32>;
                myPrivate->minimum     = std::numeric_limits<goInt32>::min();
                myPrivate->maximum     = std::numeric_limits<goInt32>::max();
            }
            break;
        case GO_INT64:
            {
                myPrivate->size        = sizeof (goInt64);
                myPrivate->signedness  = true;
                myPrivate->string      = "signed integer 64 bit";
                myPrivate->lowerThan   = lowerThanFunction<goInt64>;
                myPrivate->greaterThan = greaterThanFunction<goInt64>;
                myPrivate->equal       = equalFunction<goInt64>;
                myPrivate->minimum     = std::numeric_limits<goInt64>::min();
                myPrivate->maximum     = std::numeric_limits<goInt64>::max();
            }
            break;
        case GO_UINT8:
            {
                myPrivate->size        = sizeof (goUInt8);
                myPrivate->signedness  = false;
                myPrivate->string      = "unsigned integer 8 bit";
                myPrivate->lowerThan   = lowerThanFunction<goUInt8>;
                myPrivate->greaterThan = greaterThanFunction<goUInt8>;
                myPrivate->equal       = equalFunction<goUInt8>;
                myPrivate->minimum     = std::numeric_limits<goUInt8>::min();
                myPrivate->maximum     = std::numeric_limits<goUInt8>::max();
            }
            break;
        case GO_UINT16:
            {
                myPrivate->size        = sizeof (goUInt16);
                myPrivate->signedness  = false;
                myPrivate->string      = "unsigned integer 16 bit";
                myPrivate->lowerThan   = lowerThanFunction<goUInt16>;
                myPrivate->greaterThan = greaterThanFunction<goUInt16>;
                myPrivate->equal       = equalFunction<goUInt16>;
                myPrivate->minimum     = std::numeric_limits<goUInt16>::min();
                myPrivate->maximum     = std::numeric_limits<goUInt16>::max();
            }
            break;
        case GO_UINT32:
            {
                myPrivate->size        = sizeof (goUInt32);
                myPrivate->signedness  = false;
                myPrivate->string      = "unsigned integer 32 bit";
                myPrivate->lowerThan   = lowerThanFunction<goUInt32>;
                myPrivate->greaterThan = greaterThanFunction<goUInt32>;
                myPrivate->equal       = equalFunction<goUInt32>;
                myPrivate->minimum     = std::numeric_limits<goUInt32>::min();
                myPrivate->maximum     = std::numeric_limits<goUInt32>::max();
            }
            break;
        case GO_FLOAT:
            {
                myPrivate->size        = sizeof (goFloat);
                myPrivate->signedness  = true;
                myPrivate->string      = "float";
                myPrivate->lowerThan   = lowerThanFunction<goFloat>;
                myPrivate->greaterThan = greaterThanFunction<goFloat>;
                myPrivate->equal       = equalFunction<goFloat>;
                myPrivate->minimum     = std::numeric_limits<goFloat>::min();
                myPrivate->maximum     = std::numeric_limits<goFloat>::max();
            }
            break;
        case GO_DOUBLE:
            {
                myPrivate->size        = sizeof (goDouble);
                myPrivate->signedness  = true;
                myPrivate->string      = "double";
                myPrivate->lowerThan   = lowerThanFunction<goDouble>;
                myPrivate->greaterThan = greaterThanFunction<goDouble>;
                myPrivate->equal       = equalFunction<goDouble>;
                myPrivate->minimum     = std::numeric_limits<goDouble>::min();
                myPrivate->maximum     = std::numeric_limits<goDouble>::max();
            }
            break;
        case GO_VOID_POINTER:
            {
                myPrivate->size        = sizeof (void*);
                myPrivate->signedness  = false;
                myPrivate->string      = "void pointer";
                myPrivate->lowerThan   = lowerThanFunction<void*>;
                myPrivate->greaterThan = greaterThanFunction<void*>;
                myPrivate->equal       = equalFunction<void*>;
                myPrivate->minimum     = std::numeric_limits<unsigned int>::min();
                myPrivate->maximum     = std::numeric_limits<unsigned int>::max();
            }
            break;
        default:
            {
                assert ("UNKNOWN TYPE ENUM" == NULL);
                return false;
            }
            break;
    }
    myPrivate->typeEnum = t;
    return true;
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

const goType&
goType::operator= (const goType& other)
{
    assert (this->setID (other.getID()));
    return *this;
}

goCompareFunction
goType::getLowerThanFunction () const
{
    return myPrivate->lowerThan;
}

goCompareFunction
goType::getGreaterThanFunction () const
{
    return myPrivate->greaterThan;
}

goCompareFunction
goType::getEqualFunction () const
{
    return myPrivate->equal;
}

goDouble
goType::getMinimum () const
{
    return myPrivate->minimum;
}

goDouble
goType::getMaximum () const
{
    return myPrivate->maximum;
}

template <class T>
static bool
greaterThanFunction (const void* v1, const void* v2)
{
    return *(const T*)v1 > *(const T*)v2;
}

template <class T>
static bool
lowerThanFunction (const void* v1, const void* v2)
{
    return *(const T*)v1 < *(const T*)v2;
}

template <class T>
static bool
equalFunction (const void* v1, const void* v2)
{
    return *(const T*)v1 == *(const T*)v2;
}
