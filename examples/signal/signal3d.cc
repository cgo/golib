#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gotypes.h>
#include <iostream>
#include <gosignalmacros.h>

template<class T>
void printSignal (goSignal3DBase<T>& signal)
{
    goIndex_t x, y;
   
//    GO_SIGNAL3D_EACHELEMENT (std::cout << *__ptr << " ", signal, T);
    
    for (y = 0; y < signal.getSizeY(); ++y)
    {
        for (x = 0; x < signal.getSizeX(); ++x)
        {
            std::cout << *signal.getPtr (x, y, 0) << " ";
        }
        std::cout << "\n";
    }
    std::cout << "\n";
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
            std::cout << *p << " ";
            ++p;
        }
        std::cout << "\n";
    }
    std::cout << "\n";
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
    {
        goSignal3D<goInt32> object1;
        goSignal3D<goInt32> object2;

        object1.connectObject (&object2);
        object2.connectObject (&object1);
        object1.sendObjectMessage (GO_OBJECTMESSAGE_NONE);
        object1.sendObjectMessage (GO_OBJECTMESSAGE_DESTRUCTING);
        // object1.disconnectObject (&object2);
        object1.sendObjectMessage (GO_OBJECTMESSAGE_DESTRUCTING);
    }
    
    goSignal3D<goInt32> signal (16, 16, 16, 4, 4, 4, 16, 16, 16);

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
    
    goSubSignal3D<goInt32> subSignal (&signal, 1, 2, 1);
    subSignal.setPosition (-2, -2, 0);
    printSignal (subSignal);
    subSignal.setPosition (-1, 0, 0);
    printSignal (subSignal);
    
    
    /************************************************************************************/
    
    std::cout << "goSubSignal3D with skip 2\n";
    std::cout << "-----------------------\n";
    subSignal.setSize (signal.getSizeX() / 3, signal.getSizeY() / 3, 1);
    subSignal.setPosition (0, 0, 0);
    subSignal.setSkip (2, 2, 0);
    printSignal (subSignal);
    
    std::cout << "goSubSignal3D with 2-shifted size\n";
    std::cout << "-------------------------------\n";
    subSignal.setSize (16, 16, 16);
    subSignal.setPosition (0, 0, 0);
    subSignal.setSkip (0, 0, 0);
    
    subSignal.shiftLeftDiff (2);
    subSignal.shiftRightSize (2);
    printSignal (subSignal);
    return 1;
}
