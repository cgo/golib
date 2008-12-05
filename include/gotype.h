#ifndef GOTYPE_H
#define GOTYPE_H

#include <gotypes.h>
#ifndef GOSTRING_H
# include <gostring.h>
#endif
#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif

/**
 * @brief Comparison function (see goType).
 **/
typedef bool (*goCompareFunction)(const void*, const void*);
/**
 * @brief Index generating function (see goType).
 **/
typedef goIndex_t (*goIndexFunction)(const void*);

// We need a function that:
// - converts from a void* to a float or double
// - converts from a float or double back to the type (also void* to a table)
// --> write a class that creates those tables and functions that return float/double
//     from a void*

class goTypePrivate;
/*!
 * \addtogroup types
 * @{
 */
/*! 
 * \brief Provides type information.
 *
 * This class contains information about a data type.
 * It provides an ID enumerator (see goTypeEnum) such as
 * GO_UINT8, GO_INT8, GO_UINT16, GO_INT16, GO_UINT32, GO_INT32, GO_FLOAT, GO_DOUBLE, ...,
 * a character string describing the type, the data size in bytes,
 * and a means to generate lookup tables for data.
 * In order to create a lookup table, you can use
 * getIndexFunction() to
 * get a function which generates indices of type goIndex_t from a void* to
 * a data value. You may also specify your own index generating functions for
 * a specific type (see goIndexFunction).
 * Get the minimum and maximum indices created by the function returned
 * by getIndexFunction() using the methods getMinIndex() and getMaxIndex()
 * and use these values to simply create an array of values you want to map
 * each index to, like this:
 *
 * <pre>
 *  ...
 *  goSignal3D<void>* sig = ...;
    goArray<goUInt16> LUT;
    goUInt16*         LUTOrigin = 0;
    {
        goType targetType (GO_UINT16);
        goIndex_t minIndex = sig->getDataType().getMinIndex();
        goIndex_t maxIndex = sig->getDataType().getMaxIndex();
        goDouble delta = (targetType.getMaximum() - targetType.getMinimum())/ (goDouble)(maxIndex - minIndex);
        LUT.resize (maxIndex - minIndex + 1);
        LUTOrigin = LUT.getPtr() - minIndex;
        goIndex_t i;
        goDouble value = (goDouble)targetType.getMinimum();
        for (i = minIndex; i <= maxIndex; ++i)
        {
            LUTOrigin[i] = (goUInt16)value;
            value += delta;
        }
    }
    goIndexFunction indexFunction = sig->getDataType().getIndexFunction();
    // Get the uint16 value for the data value at (5,1,2):
    goUInt16 value = LUTOrigin[indexFunction(sig->getPtr(5,1,2)];
    ...
    </pre>
    You can also use the function goCreateQuantizationTable() for this:
    <pre>
    ...
    goSignal3D<void>* sig = ...;
    goArray<goUInt16> LUT;
    goUInt16*         LUTOrigin = 0;
    lutOrigin = goCreateQuantizationTable (dataType, goUInt16(0), goUInt16(65535), 
                                           minIndex, maxIndex, lut);
    goIndexFunction indexFunction = sig->getDataType().getIndexFunction();
    // Get the uint16 value for the data value at (5,1,2):
    goUInt16 value = LUTOrigin[indexFunction(sig->getPtr(5,1,2)];
    ...
    </pre>
 * 
 */
class goType : public goObjectBase
{
    public:
        goType (goTypeEnum t);
        goType (const goType& other);
        virtual ~goType ();

        static goSize_t  getSize   (goTypeEnum t);
        static bool      isSigned  (goTypeEnum t);
        static void      getString (goTypeEnum t, goString& stringRet);

        bool              setID     (goTypeEnum t);
        goSize_t          getSize   () const;
        bool              isSigned  () const;
        const goString&   getString () const;
        goTypeEnum        getID     () const;
        goCompareFunction getLowerThanFunction   () const;
        goCompareFunction getGreaterThanFunction () const;
        goCompareFunction getEqualFunction       () const;
       
        goIndexFunction   getIndexFunction       () const;
        goIndex_t         getMinIndex            () const;
        goIndex_t         getMaxIndex            () const;
        
        goDouble          getMinimum () const;
        goDouble          getMaximum () const;
        
        const goType& operator= (const goType& other);

    private:
        goTypePrivate* myPrivate;
};

template <class T>
goTypeEnum goTypeID ();

template <class targetT>
targetT* goCreateQuantizationTable (const goType&     sourceType,
                                    targetT           minTargetValue,
                                    targetT           maxTargetValue,
                                    goIndex_t         minIndex,
                                    goIndex_t         maxIndex,
                                    goArray<targetT>& tableRet,
                                    goDouble          sourceMin = 0.0,
                                    goDouble          sourceMax = -1.0);
/*! @} */
#endif
