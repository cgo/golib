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
 * 
 * @return A pointer to the quantization table entry
 *         associated with index 0 which is simply &tableRet[-minIndex].
 *
 * @note This works currently only for scalar types.
 **/
template <class targetT>
targetT* goCreateQuantizationTable (const goType&     sourceType,
                                    targetT           minTargetValue,
                                    targetT           maxTargetValue,
                                    goIndex_t         minIndex,
                                    goIndex_t         maxIndex,
                                    goArray<targetT>& tableRet)
{
    switch (sourceType.getID())
    {
        case GO_INT8:
            {
                return _goCreateQuantizationTable (minTargetValue, maxTargetValue,
                                                   (goInt8)sourceType.getMinimum(),
                                                   (goInt8)sourceType.getMaximum(),
                                                   minIndex, maxIndex, tableRet);
            }
            break;
        case GO_UINT8:
            {
                return _goCreateQuantizationTable (minTargetValue, maxTargetValue,
                                                   (goUInt8)sourceType.getMinimum(),
                                                   (goUInt8)sourceType.getMaximum(),
                                                   minIndex, maxIndex, tableRet);
            }
            break;
        case GO_INT16:
            {
                return _goCreateQuantizationTable (minTargetValue, maxTargetValue,
                                                   (goInt16)sourceType.getMinimum(),
                                                   (goInt16)sourceType.getMaximum(),
                                                   minIndex, maxIndex, tableRet);
            }
            break;
        case GO_UINT16:
            {
                return _goCreateQuantizationTable (minTargetValue, maxTargetValue,
                                                   (goUInt16)sourceType.getMinimum(),
                                                   (goUInt16)sourceType.getMaximum(),
                                                   minIndex, maxIndex, tableRet);
            }
            break;
        case GO_INT32:
            {
                return _goCreateQuantizationTable (minTargetValue, maxTargetValue,
                                                   (goInt32)sourceType.getMinimum(),
                                                   (goInt32)sourceType.getMaximum(),
                                                   minIndex, maxIndex, tableRet);
            }
            break;
        case GO_UINT32:
            {
                return _goCreateQuantizationTable (minTargetValue, maxTargetValue,
                                                   (goUInt32)sourceType.getMinimum(),
                                                   (goUInt32)sourceType.getMaximum(),
                                                   minIndex, maxIndex, tableRet);
            }
            break;
        case GO_FLOAT:
            {
                return _goCreateQuantizationTable (minTargetValue, maxTargetValue,
                                                   (goFloat)0.0f,
                                                   (goFloat)1.0f,
                                                   minIndex, maxIndex, tableRet);
            }
            break;
        case GO_DOUBLE:
            {
                return _goCreateQuantizationTable (minTargetValue, maxTargetValue,
                                                   (goDouble)0.0,
                                                   (goDouble)1.0,
                                                   minIndex, maxIndex, tableRet);
            }
            break;
        default:
            return NULL;
    }
    return NULL;
}

#endif
