#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gosignalmacros.h>
#include <gofilter3d.h>
#include <gotypes.h>
#include <iostream>

template<class T>
void printSignal (goSignal3DBase<T>& signal)
{
    goIndex_t x, y;

#if 0
    for (x = -((int)signal.getBorderX()); x < (goIndex_t)signal.getSizeX() + signal.getBorderX(); ++x)
    {
        cout << (int)signal.getXDiff()[x] << " ";
    }
    cout << "\n";
    
    for (x = -((int)signal.getBorderY()); x < (goIndex_t)signal.getSizeY() + signal.getBorderY(); ++x)
    {
        cout << (int)signal.getYDiff()[x] << " ";
    }
    cout << "\n";
    
//    for (x = -((int)signal.getBorderY()); x < (goIndex_t)signal.getSizeY() + signal.getBorderY(); ++x)
    {
        cout << (int)signal.getYJump()[x] << " ";
    }
    cout << "\n";
#endif 
//    for (y = -((int)signal.getBorderY()); y < signal.getSizeY() + signal.getBorderY(); ++y)
    for (y = 0; y < signal.getSizeY(); ++y)
    {
//        for (x = -((int)signal.getBorderX()); x < (goIndex_t)signal.getSizeX() + signal.getBorderX(); ++x)
        for (x = 0; x < signal.getSizeX(); ++x)
        {
            cout << *signal.getPtr (x, y, 0) << " ";
        }
        cout << "\n";
    }
    cout << "\n";
}

template<class T>
void printDirect (goSignal3DBase<T>& signal)
{
    goIndex_t x, y;
   
    T* p = signal.getPtr(0, 0, 0);
    
    for (y = 0; y < signal.getSizeY(); ++y)
    {
        for (x = 0; x < signal.getSizeX(); ++x)
        {
            cout << *p << " ";
            ++p;
        }
        cout << "\n";
    }
    cout << "\n";
}

template<class T>
void printWithPointers (goSignal3D<T>& signal)
{
    T* pz = signal.getPtr(-((goIndex_t)signal.getBorderX()), 0, 0);
    T* p  = pz;
    T* py = pz;
    goPtrdiff_t* dx = &signal.getXDiff()[-((int)signal.getBorderX())];
    goPtrdiff_t* dxSave = dx;
    goPtrdiff_t* dy = signal.getYDiff();
    goPtrdiff_t* dySave = dy;

    goIndex_t x,y,z;

    for (y = 0; y < signal.getSizeY(); ++y)
    {
        dx = dxSave;
        p = py;
        for (x = -((int)signal.getBorderX()); x < (signal.getSizeX() + signal.getBorderX()); ++x)
        {
            std::cout << *p << " ";
            p += *dx;
            ++dx;
        }
        py += *dy;
        // std::cout << "\n" << *dy << "\n";
        std::cout << "\n";
        ++dy;
    }
    std::cout << "\n";
}

int main (void)
{
    goSignal3D<goFloat> signal (16, 16, 16, 4, 4, 4, 4, 4, 4);

    signal.fillByte (0);
    printSignal (signal);
    
    goIndex_t x, y;

    goInt32 counter = 0;
    
    for (y = 0; y < signal.getSizeY(); ++y)
    {
        for (x = 0; x < signal.getSizeX(); ++x)
        {
            *signal.getPtr (x, y, 0) = counter;
            *signal.getPtr (x, y, 1) = 200 + counter++;
        }
    }
   
    std::cout << "printSignal: \n";
    printSignal (signal);
   
    goFilter3D<goFloat, goFloat> filter;
    goSignal3D<goFilter3D<goFloat,goFloat>::mask_t> mask (3, 1, 1);
    *mask.getPtr (0, 0, 0) = 1.0f;
    *mask.getPtr (1, 0, 0) = 1.0f;
    *mask.getPtr (2, 0, 0) = 1.0f;
    goSignal3D<goFilter3D<goFloat,goFloat>::mask_t> mask_v (3, 3, 1);
    *mask_v.getPtr (0, 0, 0) = 1.0f;
    *mask_v.getPtr (0, 1, 0) = 1.0f;
    *mask_v.getPtr (0, 2, 0) = 1.0f;
    *mask_v.getPtr (1, 0, 0) = 1.0f;
    *mask_v.getPtr (1, 1, 0) = 1.0f;
    *mask_v.getPtr (1, 2, 0) = 1.0f;
    *mask_v.getPtr (2, 0, 0) = 1.0f;
    *mask_v.getPtr (2, 1, 0) = 1.0f;
    *mask_v.getPtr (2, 2, 0) = 1.0f;

    goSignal3D<goFloat> filteredSignal (signal.getSizeX(),
                                        signal.getSizeY(),
                                        signal.getSizeZ(),
                                        signal.getBlockSizeX(),
                                        signal.getBlockSizeY(),
                                        signal.getBlockSizeZ(),
                                        signal.getBorderX(),
                                        signal.getBorderY(),
                                        signal.getBorderZ());
   
    filter.setMask (mask);
    filter.setMaskCenter (1, 0, 0);

    
    filter.filter (signal, filteredSignal);

    std::cout << "Filtered signal:\n";
    printSignal (filteredSignal);
    
    filter.setMask       (mask_v);
    filter.setMaskCenter (1, 1, 0);
    filter.filter        (signal, filteredSignal);

    printSignal (filteredSignal);
    
    // printWithPointers (filteredSignal);

    std::cout << signal.getXDiff()[-1] << " " << signal.getXJump()[-1] << "\n";
    std::cout << signal.getXDiff()[-2] << " " << signal.getXJump()[-2] << "\n";
    std::cout << signal.getXDiff()[-3] << " " << signal.getXJump()[-3] << "\n";
    std::cout << signal.getXDiff()[-4] << " " << signal.getXJump()[-4] << "\n";
    std::cout << signal.getXDiff()[16] << " " << signal.getXJump()[16] << "\n";
    std::cout << signal.getXDiff()[17] << " " << signal.getXJump()[17] << "\n";
    std::cout << signal.getXDiff()[18] << " " << signal.getXJump()[18] << "\n";
    std::cout << signal.getXDiff()[19] << " " << signal.getXJump()[19] << "\n";
    std::cout << *signal.getPtr (-1, 0, 0) << "\n";
    std::cout << *signal.getPtr (-2, 0, 0) << "\n";
    std::cout << *signal.getPtr (-3, 0, 0) << "\n";
    std::cout << *signal.getPtr (-4, 0, 0) << "\n";
    std::cout << *signal.getPtr (16, 0, 0) << "\n";
    std::cout << *signal.getPtr (17, 0, 0) << "\n";
    std::cout << *signal.getPtr (18, 0, 0) << "\n";
    std::cout << *signal.getPtr (19, 0, 0) << "\n";
    std::cout << *signal.getPtr (-1, -1, 0) << "\n";
    std::cout << *signal.getPtr (-2, -2, 0) << "\n";
    std::cout << *signal.getPtr (-3, -3, 0) << "\n";
    std::cout << *signal.getPtr (-4, -4, 0) << "\n";
    std::cout << -((int)signal.getBorderX()) << "\n";
    return 1;
}
