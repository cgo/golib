#ifndef GOTYPE_HPP
#define GOTYPE_HPP

#ifndef GOUNIFORMQUANTIZER_H
# include <gouniformquantizer.h>
#endif
#ifndef GOUNIFORMQUANTIZER_HPP
# include <gouniformquantizer.hpp>
#endif
#ifndef GOQUANTIZER_HPP
# include <goquantizer.hpp>
#endif

/*
 * NOTE: This is done so that we are able to create quantization tables with other source data ranges
 * than the standard ones.
 */
template <class targetT, class sourceT>
targetT* _goCreateQuantizationTable (targetT           minTargetValue,
                                     targetT           maxTargetValue,
                                     sourceT           minSourceValue,
                                     sourceT           maxSourceValue,
                                     goIndex_t         minIndex,
                                     goIndex_t         maxIndex,
                                     goArray<targetT>& tableRet)
{
    goDouble step = ((goDouble)maxSourceValue-(goDouble)minSourceValue)/(goDouble)(maxIndex-minIndex);
    goUniformQuantizer<sourceT,targetT> Q (step, minSourceValue, maxSourceValue, minTargetValue, maxTargetValue);
    tableRet.resize (maxIndex - minIndex + 1);
    targetT* origin = tableRet.getPtr() - minIndex;

    goIndex_t i;
    goDouble sourceValue = (goDouble)minSourceValue;
    for (i = minIndex; i <= maxIndex; ++i)
    {
        origin[i] = Q.quantize ((sourceT)sourceValue);
        sourceValue += step;
    }
    return origin;
}

/**
 * @brief Create a quantization table (lookup table)
 *        for a given source type.
 *
 * @param sourceType     goType of the source data type.
 * @param minTargetValue Minimum target value, associated with minIndex.
 * @param maxTargetValue Maximum target value, associated with maxIndex.
 * @param minIndex       Minimum index into the table.
 * @param maxIndex       Maximum index into the table.
 * @oaran tableRet       Reference to a goArray of type targetT
 *                       which contains the quantization table after 
 *                       the function returns.
 * @param sourceMinimum  See note! If sourceMinimum > sourceMaximum, the default values
 *                       for the source data type are used.
 *                       If not, these values are used as min and max
 *                       data values for the input data type of the
 *                       quantization table.
 *                       Default is sourceMinimum = 0.0 and sourceMaximum = -1.0.
 * @param sourceMaximum  See note! See above.
 * 
 * @note WARNING: Do not hand over sourceMinimum and sourceMaximum. This does not work.
 *
 * @todo sourceMinimum and sourceMaximum does not work because index functions
 *       assume certain value ranges (esp. float/double!!)
 *
 * @return A pointer to the quantization table entry
 *         associated with index 0 which is simply &tableRet[-minIndex].
 *
 * @note This works currently only for scalar types.
 **/
template <class targetT>
targetT* goCreateQuantizationTable (const goType&               sourceType,
                                    targetT                     minTargetValue,
                                    targetT                     maxTargetValue,
                                    goIndex_t                   minIndex,
                                    goIndex_t                   maxIndex,
                                    goArray<targetT>&           tableRet,
                                    goDouble                    sourceMinimum,
                                    goDouble                    sourceMaximum)
{
    goDouble sourceMin = 0.0; 
    goDouble sourceMax = 1.0; 
    if (sourceMinimum <= sourceMaximum)
    {
        sourceMin = sourceMinimum;
        sourceMax = sourceMaximum;
    }
    else
    {
        sourceMin = sourceType.getMinimum();
        sourceMax = sourceType.getMaximum();
    }
    switch (sourceType.getID())
    {
        case GO_INT8:
            {
                return _goCreateQuantizationTable (minTargetValue, maxTargetValue,
                                                   (goInt8)sourceMin,
                                                   (goInt8)sourceMax,
                                                   minIndex, maxIndex, tableRet);
            }
            break;
        case GO_UINT8:
            {
                return _goCreateQuantizationTable (minTargetValue, maxTargetValue,
                                                   (goUInt8)sourceMin,
                                                   (goUInt8)sourceMax,
                                                   minIndex, maxIndex, tableRet);
            }
            break;
        case GO_INT16:
            {
                return _goCreateQuantizationTable (minTargetValue, maxTargetValue,
                                                   (goInt16)sourceMin,
                                                   (goInt16)sourceMax,
                                                   minIndex, maxIndex, tableRet);
            }
            break;
        case GO_UINT16:
            {
                return _goCreateQuantizationTable (minTargetValue, maxTargetValue,
                                                   (goUInt16)sourceMin,
                                                   (goUInt16)sourceMax,
                                                   minIndex, maxIndex, tableRet);
            }
            break;
        case GO_INT32:
            {
                return _goCreateQuantizationTable (minTargetValue, maxTargetValue,
                                                   (goInt32)sourceMin,
                                                   (goInt32)sourceMax,
                                                   minIndex, maxIndex, tableRet);
            }
            break;
        case GO_UINT32:
            {
                return _goCreateQuantizationTable (minTargetValue, maxTargetValue,
                                                   (goUInt32)sourceMin,
                                                   (goUInt32)sourceMax,
                                                   minIndex, maxIndex, tableRet);
            }
            break;
        case GO_FLOAT:
            {
                return _goCreateQuantizationTable (minTargetValue, maxTargetValue,
                                                   (goFloat)sourceMin,
                                                   (goFloat)sourceMax,
                                                   minIndex, maxIndex, tableRet);
            }
            break;
        case GO_DOUBLE:
            {
                return _goCreateQuantizationTable (minTargetValue, maxTargetValue,
                                                   (goDouble)sourceMin,
                                                   (goDouble)sourceMax,
                                                   minIndex, maxIndex, tableRet);
            }
            break;
        default:
            return NULL;
    }
    return NULL;
}

#endif
