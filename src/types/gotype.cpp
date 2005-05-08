#include <gotype.h>
#include <assert.h>
#include <limits>
#include <goquantizer.h>
#include <goquantizer.hpp>
#include <gouniformquantizer.h>
#include <gouniformquantizer.hpp>
#include <goconfig.h>

template <class T> static bool      lowerThanFunction   (const void* v1, const void* v2);
template <class T> static bool      greaterThanFunction (const void* v1, const void* v2);
template <class T> static bool      equalFunction       (const void* v1, const void* v2);
// template <class T> static goIndex_t indexFunction       (const void*);

static goIndex_t int8IndexFunction (const void*);
static goIndex_t uint8IndexFunction (const void*);
static goIndex_t int16IndexFunction (const void*);
static goIndex_t uint16IndexFunction (const void*);
static goIndex_t int32IndexFunction (const void*);
static goIndex_t uint32IndexFunction (const void*);
static goIndex_t floatIndexFunction (const void*);
static goIndex_t doubleIndexFunction (const void*);


class goQuantizationTables
{
    public:
        goQuantizationTables ();
        ~goQuantizationTables ();

        static const goIndex_t int8MinIndex   = 0;
        static const goIndex_t int8MaxIndex   = 255;
        static const goIndex_t uint8MinIndex  = 0;
        static const goIndex_t uint8MaxIndex  = 255;
        static const goIndex_t int16MinIndex  = 0;
        static const goIndex_t int16MaxIndex  = 0xffff;
        static const goIndex_t uint16MinIndex = 0;
        static const goIndex_t uint16MaxIndex = 0xffff;
        static const goIndex_t int32MinIndex  = 0;
        static const goIndex_t int32MaxIndex  = 0xffff;
        static const goIndex_t uint32MinIndex = 0;
        static const goIndex_t uint32MaxIndex = 0xffff;
        static const goIndex_t floatMinIndex  = 0;
        static const goIndex_t floatMaxIndex  = 0xffff;
        static const goIndex_t doubleMinIndex = 0;
        static const goIndex_t doubleMaxIndex = 0xffff;
        static goFloat*   int8Tablef;
        static goFloat*  uint8Tablef;
        static goFloat*  int16Tablef;
        static goFloat* uint16Tablef;
        static goFloat*  int32Tablef;
        static goFloat* uint32Tablef;
        static goFloat*  floatTablef;
        static goFloat* doubleTablef;
        static goInt8*  fTableInt8;
        static goUInt8*  fTableUInt8;
        static goInt16*  fTableInt16;
        static goUInt16*  fTableUInt16;
        static goInt32*  fTableInt32;
        static goUInt32*  fTableUInt32;
        static goFloat*  fTableFloat;
        static goDouble* fTableDouble;

        static goDouble*   int8Tabled;
        static goDouble*  uint8Tabled;
        static goDouble*  int16Tabled;
        static goDouble* uint16Tabled;
        static goDouble*  int32Tabled;
        static goDouble* uint32Tabled;
        static goDouble*  floatTabled;
        static goDouble* doubleTabled;
};

goFloat*   goQuantizationTables::int8Tablef   = new goFloat   [goQuantizationTables::int8MaxIndex - goQuantizationTables::int8MinIndex];
goFloat*  goQuantizationTables::uint8Tablef  = new goFloat  [goQuantizationTables::uint8MaxIndex - goQuantizationTables::uint8MinIndex];
goFloat*  goQuantizationTables::int16Tablef  = new goFloat  [goQuantizationTables::int16MaxIndex - goQuantizationTables::int16MinIndex];
goFloat* goQuantizationTables::uint16Tablef = new goFloat [goQuantizationTables::uint16MaxIndex - goQuantizationTables::uint16MinIndex];
goFloat*  goQuantizationTables::int32Tablef  = new goFloat  [goQuantizationTables::int32MaxIndex - goQuantizationTables::int32MinIndex];
goFloat* goQuantizationTables::uint32Tablef = new goFloat [goQuantizationTables::uint32MaxIndex - goQuantizationTables::uint32MinIndex];
goFloat*  goQuantizationTables::floatTablef  = new goFloat  [goQuantizationTables::floatMaxIndex - goQuantizationTables::floatMinIndex];
goFloat* goQuantizationTables::doubleTablef = new goFloat [goQuantizationTables::doubleMaxIndex - goQuantizationTables::doubleMinIndex];

goInt8* goQuantizationTables::fTableInt8 = new goInt8 [goQuantizationTables::floatMaxIndex - goQuantizationTables::floatMinIndex];
goUInt8* goQuantizationTables::fTableUInt8 = new goUInt8 [goQuantizationTables::floatMaxIndex - goQuantizationTables::floatMinIndex];
goInt16* goQuantizationTables::fTableInt16 = new goInt16 [goQuantizationTables::floatMaxIndex - goQuantizationTables::floatMinIndex];
goUInt16* goQuantizationTables::fTableUInt16 = new goUInt16 [goQuantizationTables::floatMaxIndex - goQuantizationTables::floatMinIndex];
goInt32* goQuantizationTables::fTableInt32 = new goInt32 [goQuantizationTables::floatMaxIndex - goQuantizationTables::floatMinIndex];
goUInt32* goQuantizationTables::fTableUInt32 = new goUInt32 [goQuantizationTables::floatMaxIndex - goQuantizationTables::floatMinIndex];
goFloat* goQuantizationTables::fTableFloat = new goFloat [goQuantizationTables::floatMaxIndex - goQuantizationTables::floatMinIndex];
goDouble* goQuantizationTables::fTableDouble = new goDouble [goQuantizationTables::floatMaxIndex - goQuantizationTables::floatMinIndex];

template <class sourceT, class targetT>
static inline targetT* _createQuantizationTable (targetT* table, sourceT minSourceValue, sourceT maxSourceValue, targetT minTargetValue, targetT maxTargetValue, goIndex_t minIndex, goIndex_t maxIndex)
{
    goDouble step = ((goDouble)maxSourceValue-(goDouble)minSourceValue+1)/(goDouble)(maxIndex-minIndex+1);
    goUniformQuantizer<sourceT,targetT> Q ((sourceT)step, minSourceValue, maxSourceValue, minTargetValue, maxTargetValue);
    // targetT* table  = new targetT [maxIndex - minIndex];
    targetT* origin = table - minIndex;

    goIndex_t i;
    goDouble sourceValue = (goDouble)minSourceValue;
    for (i = minIndex; i <= maxIndex; ++i)
    {
        origin[i] = Q.quantize ((sourceT)sourceValue);
        sourceValue += step;
    }
    return table;
}

goQuantizationTables::goQuantizationTables ()
{
    {
        goType t (GO_INT8);
        _createQuantizationTable<goInt8,goFloat> (int8Tablef, (goInt8)t.getMinimum(), (goInt8)t.getMaximum(), 0.0f, 1.0f,
                                                 int8MinIndex, int8MaxIndex);
        _createQuantizationTable<goFloat,goInt8> (fTableInt8, 0.0f, 1.0f, (goInt8)t.getMinimum(), (goInt8)t.getMaximum(),
                                                 floatMinIndex, floatMaxIndex);
    }
    {
        goType t (GO_UINT8);
        _createQuantizationTable<goUInt8,goFloat> (uint8Tablef, (goUInt8)t.getMinimum(), (goUInt8)t.getMaximum(), 0.0f, 1.0f,
                                                 uint8MinIndex, uint8MaxIndex);
        _createQuantizationTable<goFloat,goUInt8> (fTableUInt8, 0.0f, 1.0f, (goUInt8)t.getMinimum(), (goUInt8)t.getMaximum(),
                                                  floatMinIndex, floatMaxIndex);
    }
    {
        goType t (GO_INT16);
        _createQuantizationTable<goInt16,goFloat> (int16Tablef, (goInt16)t.getMinimum(), (goInt16)t.getMaximum(), 0.0f, 1.0f,
                                                  int16MinIndex, int16MaxIndex);
        _createQuantizationTable<goFloat,goInt16> (fTableInt16, 0.0f, 1.0f, (goInt16)t.getMinimum(), (goInt16)t.getMaximum(),
                                                  floatMinIndex, floatMaxIndex);
    }
    {
        goType t (GO_UINT16);
        _createQuantizationTable<goUInt16,goFloat> (uint16Tablef, (goUInt16)t.getMinimum(), (goUInt16)t.getMaximum(), 0.0f, 1.0f,
                                                   uint16MinIndex, uint16MaxIndex);
        _createQuantizationTable<goFloat,goUInt16> (fTableUInt16, 0.0f, 1.0f, (goUInt16)t.getMinimum(), (goUInt16)t.getMaximum(),
                                                  floatMinIndex, floatMaxIndex);
    }
    {
        goType t (GO_INT32); 
        _createQuantizationTable<goInt32,goFloat> (int32Tablef, (goInt32)t.getMinimum(), (goInt32)t.getMaximum(), 0.0f, 1.0f,
                                                  int32MinIndex, int32MaxIndex);
        _createQuantizationTable<goFloat,goInt32> (fTableInt32, 0.0f, 1.0f, (goInt32)t.getMinimum(), (goInt32)t.getMaximum(),
                                                  floatMinIndex, floatMaxIndex);
    }
    {
        goType t (GO_UINT32); 
        _createQuantizationTable<goUInt32,goFloat> (uint32Tablef, (goUInt32)t.getMinimum(), (goUInt32)t.getMaximum(), 0.0f, 1.0f,
                                                   uint32MinIndex, uint32MaxIndex);
        _createQuantizationTable<goFloat,goUInt32> (fTableUInt32, 0.0f, 1.0f, (goUInt32)t.getMinimum(), (goUInt32)t.getMaximum(),
                                                   floatMinIndex, floatMaxIndex);
    }
    {
        goType t (GO_FLOAT); 
        _createQuantizationTable<goFloat,goFloat> (floatTablef, (goFloat)t.getMinimum(), (goFloat)t.getMaximum(), 0.0f, 1.0f,
                                                   floatMinIndex, floatMaxIndex);
        _createQuantizationTable<goFloat,goFloat> (fTableFloat, 0.0f, 1.0f, (goFloat)t.getMinimum(), (goFloat)t.getMaximum(),
                                                  floatMinIndex, floatMaxIndex);
    }
    {
        goType t (GO_DOUBLE); 
        _createQuantizationTable<goDouble,goFloat> (doubleTablef, t.getMinimum(), t.getMaximum(), 0.0f, 1.0f,
                                                   doubleMinIndex, doubleMaxIndex);
        _createQuantizationTable<goFloat,goDouble> (fTableDouble, 0.0f, 1.0f, t.getMinimum(), t.getMaximum(),
                                                   floatMinIndex, floatMaxIndex);
    }
    std::cout << "Initialized goQuantizationTables\n";
}

goQuantizationTables::~goQuantizationTables ()
{
}

// Just for initialization of the static members
// goQuantizationTables _goGlobalQuantizationTables;

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
        goIndexFunction   indexFunction;

        goDouble minimum;
        goDouble maximum;
};

goTypePrivate::goTypePrivate (goTypeEnum t)
    : typeEnum      (t),
      size          (goType::getSize (t)),
      signedness    (goType::isSigned (t)),
      string        (""),
      lowerThan     (NULL),
      equal         (NULL),
      greaterThan   (NULL),
      indexFunction (NULL),
      minimum       (0.0),
      maximum       (1.0)
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

goType::goType (const goType& other)
    : goObjectBase (),
      myPrivate (NULL)
{
    setClassName ("goType");
    myPrivate = new goTypePrivate (other.getID());
    assert (myPrivate);
    setID (other.getID());
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
                myPrivate->size          = sizeof (goInt8);
                myPrivate->signedness    = true;
                myPrivate->string        = "signed integer 8 bit";
                myPrivate->lowerThan     = lowerThanFunction<goInt8>;
                myPrivate->greaterThan   = greaterThanFunction<goInt8>;
                myPrivate->equal         = equalFunction<goInt8>;
                myPrivate->minimum       = std::numeric_limits<goInt8>::min();
                myPrivate->maximum       = std::numeric_limits<goInt8>::max();
                myPrivate->indexFunction = int8IndexFunction;
            }
            break;
        case GO_INT16:
            {
                myPrivate->size          = sizeof (goInt16);
                myPrivate->signedness    = true;
                myPrivate->string        = "signed integer 16 bit";
                myPrivate->lowerThan     = lowerThanFunction<goInt16>;
                myPrivate->greaterThan   = greaterThanFunction<goInt16>;
                myPrivate->equal         = equalFunction<goInt16>;
                myPrivate->minimum       = std::numeric_limits<goInt16>::min();
                myPrivate->maximum       = std::numeric_limits<goInt16>::max();
                myPrivate->indexFunction = int16IndexFunction;
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
                myPrivate->indexFunction = int32IndexFunction;
            }
            break;
#ifdef HAVE_INT64
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
                myPrivate->indexFunction = int32IndexFunction;
            }
            break;
#endif
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
                myPrivate->indexFunction = uint8IndexFunction;
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
                myPrivate->indexFunction = uint16IndexFunction;
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
                myPrivate->indexFunction = uint32IndexFunction;
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
                myPrivate->indexFunction = floatIndexFunction;
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
                myPrivate->indexFunction = doubleIndexFunction;
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
#ifdef HAVE_INT64
        case GO_INT64:
            {
                return sizeof (goInt64);
            }
            break;
#endif
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


/**
 * @brief Get a compare function for "lower than" relation.
 *
 * @return Compare function for "lower than" relation.
 **/
goCompareFunction
goType::getLowerThanFunction () const
{
    return myPrivate->lowerThan;
}

/**
 * @brief Get a compare function for "greater than" relation.
 *
 * @return Compare function for "greater than" relation.
 **/
goCompareFunction
goType::getGreaterThanFunction () const
{
    return myPrivate->greaterThan;
}

/**
 * @brief Get a compare function for "equal" relation.
 *
 * @return Compare function for "equal" relation.
 **/
goCompareFunction
goType::getEqualFunction () const
{
    return myPrivate->equal;
}

/**
 * @brief Get an index function.
 *
 * @return The standard index generating function for this data type.
 **/
goIndexFunction 
goType::getIndexFunction () const
{
    return myPrivate->indexFunction;
}

/**
 * @brief Get the minimum index.
 *
 * @return The minimum index generated by the standard index 
 *         function for this data type.
 **/
goIndex_t
goType::getMinIndex () const
{
    return 0;
}

/**
 * @brief Get the maximum index.
 *
 * @return The maximum index generated by the standard index 
 *         function for this data type.
 **/
goIndex_t
goType::getMaxIndex () const
{
    switch (this->getID())
    {
        case GO_INT8:
        case GO_UINT8:
        {
            return 255;
        }
        break;
        case GO_INT16:
        case GO_UINT16:
        case GO_INT32:
        case GO_UINT32:
        case GO_FLOAT:
        case GO_DOUBLE:
        {
            return 0xffff;
        }
        break;
    }
    return 0;
}

#if 0
const void*
goType::getQuantizationTable (goTypeEnum targetType) const
{
    if (targetType == GO_FLOAT)
    {
        switch (this->getID())
        {
            case GO_INT8:
            {
                return goQuantizationTables::int8Tablef;
            }
            break;
            case GO_UINT8:
            {
                return goQuantizationTables::uint8Tablef;
            }
            break;
            case GO_INT16:
            {
                return goQuantizationTables::int16Tablef;
            }
            break;
            case GO_UINT16:
            {
                return goQuantizationTables::uint16Tablef;
            }
            break;
            case GO_INT32:
            {
                return goQuantizationTables::int32Tablef;
            }
            break;
            case GO_UINT32:
            {
                return goQuantizationTables::uint32Tablef;
            }
            break;
            case GO_FLOAT:
            {
                return goQuantizationTables::floatTablef;
            }
            break;
            case GO_DOUBLE:
            {
                return goQuantizationTables::doubleTablef;
            }
            break;
            default:
                return NULL;
                break;
        }
    }
    if (this->getID() == GO_FLOAT)
    {
        switch (targetType)
        {
            case GO_INT8:
                {
                    return goQuantizationTables::fTableInt8;
                }
                break;
            case GO_UINT8:
                {
                    return goQuantizationTables::fTableUInt8;
                }
                break;
            case GO_INT16:
                {
                    return goQuantizationTables::fTableInt16;
                }
                break;
            case GO_UINT16:
                {
                    return goQuantizationTables::fTableUInt16;
                }
                break;
            case GO_INT32:
                {
                    return goQuantizationTables::fTableInt32;
                }
                break;
            case GO_UINT32:
                {
                    return goQuantizationTables::fTableUInt32;
                }
                break;
            case GO_FLOAT:
                {
                    return goQuantizationTables::fTableFloat;
                }
                break;
            case GO_DOUBLE:
                {
                    return goQuantizationTables::fTableDouble;
                }
                break;
            default:
                return NULL;
                break;
        }
    }
    return NULL;
}
#endif

/**
 * @brief Get the minimum value represented by this data type.
 *
 * @return The minimum value cast to goDouble.
 **/
goDouble
goType::getMinimum () const
{
    return myPrivate->minimum;
}

/**
 * @brief Get the maximum value represented by this data type.
 *
 * @return The maximum value cast to goDouble.
 **/
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

static goIndex_t 
int8IndexFunction (const void* valueP)
{
    static goType t (GO_INT8);
    static goDouble delta = (goQuantizationTables::int8MaxIndex - goQuantizationTables::int8MinIndex) / ((t.getMaximum() - t.getMinimum()) / 1.0);
    static goDouble delta_rec = 1.0 / delta;
    static goDouble minimum = t.getMinimum();
    goInt8 input = *(goInt8*)valueP;
    goSize_t d = (goSize_t)((input - minimum) * delta_rec);
    return (goIndex_t)(goQuantizationTables::int8MinIndex + delta * d);
}
static goIndex_t 
uint8IndexFunction (const void* valueP)
{
    static goType t (GO_UINT8);
    static goDouble delta = (goQuantizationTables::uint8MaxIndex - goQuantizationTables::uint8MinIndex) / ((t.getMaximum() - t.getMinimum()) / 1.0);
    static goDouble delta_rec = 1.0 / delta;
    static goDouble minimum = t.getMinimum();
    goUInt8 input = *(goUInt8*)valueP;
    goSize_t d = (goSize_t)((input - minimum) * delta_rec);
    return (goIndex_t)(goQuantizationTables::uint8MinIndex + delta * d);
}
static goIndex_t 
int16IndexFunction (const void* valueP)
{
    static goType t (GO_INT16);
    static goDouble delta = (goQuantizationTables::int16MaxIndex - goQuantizationTables::int16MinIndex) / ((t.getMaximum() - t.getMinimum()) / 1.0);
    static goDouble delta_rec = 1.0 / delta;
    static goDouble minimum = t.getMinimum();
    goInt16 input = *(goInt16*)valueP;
    goSize_t d = (goSize_t)((input - minimum) * delta_rec);
    return (goIndex_t)(goQuantizationTables::int16MinIndex + delta * d);
}
static goIndex_t 
uint16IndexFunction (const void* valueP)
{
    static goType t (GO_UINT16);
    static goDouble delta = (goQuantizationTables::uint16MaxIndex - goQuantizationTables::uint16MinIndex) / ((t.getMaximum() - t.getMinimum()) / 1.0);
    static goDouble delta_rec = 1.0 / delta;
    static goDouble minimum = t.getMinimum();
    goUInt16 input = *(goUInt16*)valueP;
    goSize_t d = (goSize_t)((input - minimum) * delta_rec);
    return (goIndex_t)(goQuantizationTables::uint16MinIndex + delta * d);
}
static goIndex_t 
int32IndexFunction (const void* valueP)
{
    static goType t (GO_INT32);
    static goDouble delta = (goQuantizationTables::int32MaxIndex - goQuantizationTables::int32MinIndex) / ((t.getMaximum() - t.getMinimum()) / 1.0);
    static goDouble delta_rec = 1.0 / delta;
    static goDouble minimum = t.getMinimum();
    goInt32 input = *(goInt32*)valueP;
    goSize_t d = (goSize_t)((input - minimum) * delta_rec);
    return (goIndex_t)(goQuantizationTables::int32MinIndex + delta * d);
}
static goIndex_t 
uint32IndexFunction (const void* valueP)
{
    static goType t (GO_UINT32);
    static goDouble delta = (goQuantizationTables::uint32MaxIndex - goQuantizationTables::uint32MinIndex) / ((t.getMaximum() - t.getMinimum()) / 1.0);
    static goDouble delta_rec = 1.0 / delta;
    static goDouble minimum = t.getMinimum();
    goUInt32 input = *(goUInt32*)valueP;
    goSize_t d = (goSize_t)((input - minimum) * delta_rec);
    return (goIndex_t)(goQuantizationTables::uint32MinIndex + delta * d);
}
static goIndex_t 
floatIndexFunction (const void* valueP)
{
    static goType t (GO_FLOAT);
    static goDouble delta = goQuantizationTables::floatMaxIndex - goQuantizationTables::floatMinIndex;
    return (goIndex_t)(*(goFloat*)valueP * delta);
}
static goIndex_t 
doubleIndexFunction (const void* valueP)
{
    static goType t (GO_DOUBLE);
    static goDouble delta = goQuantizationTables::doubleMaxIndex - goQuantizationTables::doubleMinIndex;
    return (goIndex_t)(*(goDouble*)valueP * delta);
}
