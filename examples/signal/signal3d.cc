#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gotypes.h>
#include <iostream>

template<class T>
void printSignal (goSignal3DBase<T>& signal)
{
    goIndex_t x, y;
    
    for (y = 0; y < signal.getSizeY(); ++y)
    {
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
    T* pz = signal.getPtr(0, 0, 0);
    T* p  = pz;
    T* py = pz;
    goPtrdiff_t* dx = signal.getXDiff();
    goPtrdiff_t* dxSave = dx;
    goPtrdiff_t* dy = signal.getYDiff();
    goPtrdiff_t* dySave = dy;

    goIndex_t x,y,z;

    for (y = 0; y < signal.getSizeY(); ++y)
    {
        dx = dxSave;
        p = py;
        for (x = 0; x < signal.getSizeX(); ++x)
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
    goSignal3D<goInt32> signal (16, 16, 16, 4, 4, 4, 0, 0, 0);

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
    std::cout << "printDirect: \n";
    printDirect (signal);
    std::cout << "printWithPointers: \n";
    printWithPointers (signal);

    goSignal3D<goInt32> signal2 (1, 1, 1);
    
    // signal2 = signal;
    
    std::cout << "signal2 ";
    if (signal2 == signal)
    {
        std::cout << "==";
    }
    else
    {
        std::cout << "!=";
    }
    std::cout << " signal\n";
   
    goSignal3D<goInt32> signal3 (signal);
    std::cout << "signal3 ";
    if (signal3 == signal)
    {
        std::cout << "==";
    }
    else
    {
        std::cout << "!=";
    }
    std::cout << " signal\n";
  
    /********************************************************************************************/
    
    std::cout << "goSubSignal3D\n";
    std::cout << "-------------\n";
    
    goSubSignal3D<goInt32> subSignal (&signal, 4, 4, 4);
    subSignal.setPosition (1, 1, 1);
    printSignal (subSignal);
    
    
    return 1;
}
