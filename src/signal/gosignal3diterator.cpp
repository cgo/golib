/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gosignal3dbase.h>
#include <gosignal3diterator.h>

template <class T>
goSignal3DIterator<T>::goSignal3DIterator (goSignal3DBase<T>* s)
    : sig (s),
      dx (NULL), dy (NULL), dz (NULL),
      px (NULL), py (NULL), pz (NULL),
      posX (0) , posY (0) , posZ (0),
      maxX (s->getSizeX()-1), maxY (s->getSizeY()-1), maxZ(s->getSizeZ()-1)
{
    this->setPosition (0,0,0);
    dx = sig->getXDiff();
    dy = sig->getYDiff();
    dz = sig->getZDiff();
}

template <class T>
goSignal3DIterator<T>::goSignal3DIterator (const goSignal3DIterator<T>& other)
    : sig (other.sig),
      dx (NULL), dy (NULL), dz (NULL),
      px (NULL), py (NULL), pz (NULL),
      posX (0) , posY (0) , posZ (0)
{
    // Standard operator=()
    *this = other;
}

template <class T>
goSignal3DIterator<T>::~goSignal3DIterator ()
{
}

template <class T>
void goSignal3DIterator<T>::setPosition (goIndex_t x, goIndex_t y, goIndex_t z)
{
    posX = x; posY = y; posZ = z;
    dx = sig->getXDiff() + x;
    dy = sig->getYDiff() + y;
    dz = sig->getZDiff() + z;
    px = sig->getPtr(x,y,z);
    py = sig->getPtr(x,y,z);
    pz = sig->getPtr(x,y,z);
}

template <class T>
bool goSignal3DIterator<T>::endX ()
{
    return posX > maxX;
}
template <class T>
bool goSignal3DIterator<T>::endY ()
{
    return posY > maxY;
}
template <class T>
bool goSignal3DIterator<T>::endZ ()
{
    return posZ > maxZ;
}
template<> void goSignal3DIterator<void>::incrementX ()
{
    (goByte*&)px += *dx;
    ++posX;
    ++dx;
}
template <class T>
void goSignal3DIterator<T>::incrementX ()
{
    px += *dx;
    ++posX;
    ++dx;
}
template<> void goSignal3DIterator<void>::incrementY ()
{
    (goByte*&)py += *dy;
    ++posY;
    ++dy;
}
template <class T>
void goSignal3DIterator<T>::incrementY ()
{
    py += *dy;
    ++posY;
    ++dy;
}
template<> void goSignal3DIterator<void>::incrementZ ()
{
    (goByte*&)pz += *dz;
    ++posZ;
    ++dz;
}
template <class T>
void goSignal3DIterator<T>::incrementZ ()
{
    pz += *dz;
    ++posZ;
    ++dz;
}
template<> void goSignal3DIterator<void>::decrementX ()
{
    (goByte*&)px -= *(dx-1);
    --posX;
    --dx;
}
template <class T>
void goSignal3DIterator<T>::decrementX ()
{
    px -= *(dx-1);
    --posX;
    --dx;
}
template<> void goSignal3DIterator<void>::decrementY ()
{
    (goByte*&)py -= *(dy-1);
    --posY;
    --dy;
}
template <class T>
void goSignal3DIterator<T>::decrementY ()
{
    py -= *(dy-1);
    --posY;
    --dy;
}
template<> void goSignal3DIterator<void>::decrementZ ()
{
    (goByte*&)pz -= *(dz-1);
    --posZ;
    --dz;
}
template <class T>
void goSignal3DIterator<T>::decrementZ ()
{
    pz -= *(dz-1);
    --posZ;
    --dz;
}
template <class T>
void goSignal3DIterator<T>::resetX ()
{
    dx   = sig->getXDiff();
    posX = 0;
    px   = py;
}
template <class T>
void goSignal3DIterator<T>::resetY ()
{
    dy   = sig->getYDiff();
    posY = 0;
    py   = pz;
    resetX ();
}
template <class T>
void goSignal3DIterator<T>::resetZ ()
{
    dz   = sig->getZDiff();
    posZ = 0;
    pz   = sig->getPtr(0,0,0);
    resetY ();
    resetX ();
}
template <class T>
T* goSignal3DIterator<T>::operator* ()
{
    return px;
}
template <class T>
const T* goSignal3DIterator<T>::operator* () const
{
    return px;
}

