#include <gotype.h>
#include <gotype.hpp>
#include <gotypes.h>
#include <iostream>

int main ()
{
    goType t (GO_FLOAT);

    // const goInt8* table = (const goInt8*)t.getQuantizationTable (GO_INT8);
    //if (!table)
    //{
    //    std::cout << "ERROR\n";
    //    exit (0);
   // }

    goIndex_t minIndex = t.getMinIndex();
    goIndex_t maxIndex = t.getMaxIndex();

    goIndex_t i;
//    for (i = minIndex; i <= maxIndex; ++i)
//    {
//        std::cout << (int)table [i] << "\n";
//    }

    {
        goInt8 value = 0;
        goType int8Type (GO_INT8);
        goIndexFunction indexFunction = int8Type.getIndexFunction();
        for (value = (goInt8)int8Type.getMinimum(); value < (goInt8)int8Type.getMaximum(); ++value)
        {
            std::cout << "Index for " << (int)value << " is " << indexFunction(&value) << "\n";
        }
    }
    {
        goFloat value = 0.0f;
        goType type (GO_FLOAT);
        goIndexFunction indexFunction = type.getIndexFunction();
        std::cout << "Minimum: " << type.getMinimum() << "   Maximum: " << type.getMaximum() << "\n";
        for (value = 0.0f; value <= 1.00001f; value += 0.01f)
        {
            std::cout << "Index for " << value << " is " << indexFunction(&value) << "\n";
        }
    }
    
    goType myType (GO_INT8);
    goIndexFunction idx = myType.getIndexFunction();
    goArray<goUInt16> myLUT;
    goType t2 (GO_UINT16);
    goUInt16* lut = goCreateQuantizationTable (myType, (goUInt16)t2.getMinimum(), (goUInt16)t2.getMaximum(), myType.getMinIndex(), myType.getMaxIndex(), myLUT);
    if (!lut)
    {
        std::cout << "Could not create LUT.\n";
        exit(0);
    }
    
    goInt8 value;
    for (value = -128; value < 127; ++value)
    {
        std::cout << value << " --> " << lut[idx(&value)] << "\n";
    }
    std::cout << value << " --> " << lut[idx(&value)] << "\n";
    
    exit (1);
}
