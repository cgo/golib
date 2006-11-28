#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gofilter1d.h>
#include <gotypes.h>
#include <iostream>
#include <gosignalmacros.h>
#include <gosignal3dgenericiterator.h>
#include <gofilter3d.h>
#include <gotimerobject.h>
#include <gomatrix.h>

void printSignal (goSignal3DBase<void>& signal)
{
    goIndex_t x, y;
   
//    GO_SIGNAL3D_EACHELEMENT (std::cout << *__ptr << " ", signal, T);
    
    for (y = 0; y < signal.getSizeY(); ++y)
    {
        for (x = 0; x < signal.getSizeX(); ++x)
        {
            std::cout << *(goInt16*)signal.getPtr (x, y, 0) << " ";
        }
        std::cout << "\n";
    }
    std::cout << "\n";
}

template <class T>
void printGenericSignal (goSignal3DBase<void>& signal)
{
    goIndex_t x, y;
   
//    GO_SIGNAL3D_EACHELEMENT (std::cout << *__ptr << " ", signal, T);
    
    for (y = 0; y < signal.getSizeY(); ++y)
    {
        for (x = 0; x < signal.getSizeX(); ++x)
        {
            printf("%.3f ",(goFloat)*(T*)signal.getPtr (x, y, 0));
        }
        printf ("\n");
    }
    printf ("\n");
}

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

void printWithPointers (goSignal3D<void>& signal)
{
    goUInt8* pz = (goUInt8*)signal.getPtr(0, 0, 0);
    goUInt8* p  = pz;
    goUInt8* py = pz;
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
            std::cout << *(goUInt16*)p << " ";
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
    //= Performance test
    {
        goSignal3D<void> sig;
        sig.setDataType (GO_FLOAT);
        goSize_t w = 2*1470;
        goSize_t h = 2*1600;
        sig.make (w, h, 1, 16, 16, 1, 3, 3, 0, 1);
        goTimerObject timer;
        goMatrixf M (h, w);
        timer.startTimer();
        for (goSize_t i1 = 0; i1 < 10; ++i1)
        {
            goFloat* p = M.getPtr ();
            for (goSize_t y = 0; y < h; ++y)
            {
                for (goSize_t x = 0; x < w; ++x)
                {
                    *p *= 5.0f;
                    ++p;
                }
            }
        }
        timer.stopTimer();
        printf ("Matrix time: %f\n", timer.getTimerSeconds());
        timer.startTimer();
        for (goSize_t i1 = 0; i1 < 10; ++i1)
        {
            //goSignal3DGenericIterator2<goFloat> it (&sig);
            goSignal3DGenericIterator it (&sig);
            while (!it.endZ())
            {
                it.resetY();
                while (!it.endY())
                {
                    it.resetX();
                    while (!it.endX())
                    {
                        *(goFloat*)*it *= 5.0;
                        it.incrementX();
                    }
                    it.incrementY();
                }
                it.incrementZ();
            }
        }
        timer.stopTimer();
        printf ("sig time: %f\n", timer.getTimerSeconds());

        exit (1);
    }

    {
        goSignal3D<void> s1;
        goSignal3D<void> s2;

        s1.setBorderFlags (GO_X|GO_Y, GO_CONSTANT_BORDER);
        s1.setDataType (GO_FLOAT);
        s1.make (goSize3D(16,16,1), goSize3D(8,8,1), goSize3D(8,8,0), 1);
        goSignal3DGenericIterator it (&s1);
        goFloat v = 10.0f;
        while (!it.endY())
        {
            it.resetX();
            while (!it.endX())
            {
                *(goFloat*)*it = v;
                it.incrementX();
            }
            it.incrementY();
            v += 1.0f;
        }

        const goFloat mask[] = {1.0f, 1.0f, 1.0f};
        goFilter1D f (mask, 3, 1, false);
        // f.filter (s1);
        s1.swapXY();
        f.filter (s1);
        s1.swapXY();
        printGenericSignal<goFloat> (s1);
        exit (1);
    }

    {
        goSignal3D<goInt32> object1;
        goSignal3D<goInt32> object2;

//        object1.connectObject (&object2);
//        object2.connectObject (&object1);
//        object1.sendObjectMessage (GO_OBJECTMESSAGE_NONE);
//        object1.sendObjectMessage (GO_OBJECTMESSAGE_DESTRUCTING);
        // object1.disconnectObject (&object2);
//        object1.sendObjectMessage (GO_OBJECTMESSAGE_DESTRUCTING);
        printf ("Name: %s\n",object1.getClassName());
        exit(1);
    }

    {
        goSignal3D<void> signal;
        signal.setDataType (GO_INT32);
        signal.setBorderFlags (GO_X|GO_Y, GO_CONSTANT_BORDER);
        signal.make (16, 16, 16, 4, 4, 4, 16, 16, 16);
        signal.fillByte (0);
        printf ("Just the signal: \n");
        
        goIndex_t x, y;

        goInt32 counter = 0;
        
        for (y = 0; y < signal.getSizeY(); ++y)
        {
            for (x = 0; x < signal.getSizeX(); ++x)
            {
                *(goInt32*)signal.getPtr (x, y, 0) = counter;
                *(goInt32*)signal.getPtr (x, y, 1) = 200 + counter++;
                printf ("%d ",*(goInt32*)signal.getPtr(x,y,0));
            }
            printf ("\n");
        }

        printf ("\nWith border 3: \n");
        for (y = -3; y < (goIndex_t)signal.getSizeY()+3; ++y)
        {
            for (x = -3; x < (goIndex_t)signal.getSizeX()+3; ++x)
            {
                printf ("%d ", *(goInt32*)signal.getPtr (x, y, 0));
            }
            printf ("\n");
        }

        goSignal3DGenericIterator it (&signal);
        it.setPosition (0,4,0);
        printf ("Position 0,4: \n");
        printf ("leftUp == %d\n",*(goInt32*)it.leftUp());
        printf ("leftDown == %d\n",*(goInt32*)it.leftDown());
        printf ("rightUp == %d\n",*(goInt32*)it.rightUp());
        printf ("rightDown == %d\n",*(goInt32*)it.rightDown());
       
        goFilter3D<void,void> filter;
        const goFloat mask [] = {1.0f, 1.0f, 1.0f,\
                            1.0f, 1.0f, 1.0f,\
                            1.0f, 1.0f, 1.0f};
        filter.setMask (mask, 3, 3, 1, true);
        filter.setMaskCenter (1,1,0);
        goSignal3D<void> signal2;
        signal2.setDataType (GO_FLOAT);
        signal2.make (&signal);
        filter.filter (signal, signal2);

        //= Filtered:
        printf ("Filtered: \n");
        for (y = 0; y < signal2.getSizeY(); ++y)
        {
            for (x = 0; x < signal2.getSizeX(); ++x)
            {
                printf ("%d ",(goInt32)*(goFloat*)signal2.getPtr(x,y,0));
            }
            printf ("\n");
        }
        
        return 1;
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


    /************************************************************************************/
   
    {
        goSignal3D<void> generic;

        if (!generic.setDataType (GO_UINT16))
        {
            std::cout << "setDataType() failed\n";
        }
        generic.make (4,4,1,4,4,1,4,4,4);
        counter = 0;

        goSignal3D<void>::iterator it (&generic);
        while (!it.endZ())
        {
            it.resetY();
            while (!it.endY())
            {
                it.resetX();
                while (!it.endX())
                {
                    printf ("%d ", counter);
                    *(goInt16*)*it = counter++;
                    it.incrementX();
                }
                printf ("\n");
                it.incrementY();
            }
            it.incrementZ();
        }
        
//        for (y = 0; y < generic.getSizeY(); ++y)
//        {
//            for (x = 0; x < generic.getSizeX(); ++x)
//            {
//                *(goInt16*)generic.getPtr (x, y, 0) = counter++;
//                // *(goInt16*)generic.getPtr (x, y, 1) = 200 + counter++;
//            }
//        }
        printSignal (generic);
        printWithPointers (generic);
        // GO_SIGNAL3D_EACHELEMENT_GENERIC (std::cout << *(goUInt16*)__ptr, generic);

        goSubSignal3D<void> sub (&generic, 5, 5, 1);
        sub.setPosition (5, 0, 0);
        // printSignal (sub);
    }
    
    return 1;
}
