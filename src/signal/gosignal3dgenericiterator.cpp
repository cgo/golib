/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id: gosignal3dgenericiterator.cpp,v 1.1.1.1 2006/04/19 15:26:30 gosch Exp $
 */

#include <gosignal3dbase.h>
#include <gosignal3dgenericiterator.h>

#if 0
goSignal3DGenericIterator::goSignal3DGenericIterator (goSignal3DBase<void>* s)
    : sig (s),
      dx (NULL), dy (NULL), dz (NULL),
      dxStart (NULL), dyStart (NULL), dzStart (NULL),
      px (NULL), py (NULL), pz (NULL),
      posX (0) , posY (0) , posZ (0),
      maxX (s->getSizeX()-1), maxY (s->getSizeY()-1), maxZ(s->getSizeZ()-1)
{
    this->setPosition (0,0,0);
    dxStart = dx;
    dyStart = dy;
    dzStart = dz;
}

goSignal3DGenericIterator::goSignal3DGenericIterator (const goSignal3DGenericIterator& other)
    : sig (other.sig),
      dx (NULL), dy (NULL), dz (NULL),
      px (NULL), py (NULL), pz (NULL),
      posX (0) , posY (0) , posZ (0)
{
    // Standard operator=()
    *this = other;
}

goSignal3DGenericIterator::~goSignal3DGenericIterator ()
{
}

/**
 * @brief Sets the iterator position to x,y,z in the signal.
 *
 * @param x  X position
 * @param y  Y position
 * @param z  Z position
 **/
//void goSignal3DGenericIterator::setPosition (goIndex_t x, goIndex_t y, goIndex_t z)
//{
//    posX = x; posY = y; posZ = z;
//    dx = sig->getXDiff() + x;
//    dy = sig->getYDiff() + y;
//    dz = sig->getZDiff() + z;
//    px = (goByte*)sig->getPtr(x,y,z);
//    py = (goByte*)sig->getPtr(x,y,z);
//    pz = (goByte*)sig->getPtr(x,y,z);
//}

/**
 * @brief Resets the Z pointer and internals concerning Z to initial values.
 *
 * Calls resetY() and resetX().
 **/
void goSignal3DGenericIterator::resetZ ()
{
    dz   = sig->getZDiff();
    posZ = 0;
    pz   = (goByte*)sig->getPtr(0,0,0);
    resetY ();
    resetX ();
}
#endif

/**
 * @brief Sets the iterator position to x,y,z in the signal.
 *
 * @param x  X position
 * @param y  Y position
 * @param z  Z position
 **/
goSignal3DGenericConstIterator::goSignal3DGenericConstIterator (const goSignal3DBase<void>* s)
    : sig (s),
      dx (NULL), dy (NULL), dz (NULL),
      dxStart (NULL), dyStart (NULL), dzStart (NULL),
      px (NULL), py (NULL), pz (NULL),
      posX (0) , posY (0) , posZ (0),
      maxX (s->getSizeX()-1), maxY (s->getSizeY()-1), maxZ(s->getSizeZ()-1)
{
    this->setPosition (0,0,0);
    dxStart = dx;
    dyStart = dy;
    dzStart = dz;
}

goSignal3DGenericConstIterator::goSignal3DGenericConstIterator (const goSignal3DGenericConstIterator& other)
    : sig (other.sig),
      dx (NULL), dy (NULL), dz (NULL),
      px (NULL), py (NULL), pz (NULL),
      posX (0) , posY (0) , posZ (0)
{
    // Standard operator=()
    *this = other;
}

goSignal3DGenericConstIterator::~goSignal3DGenericConstIterator ()
{
}

/**
 * @brief Sets the iterator position to x,y,z in the signal.
 *
 * @param x  X position
 * @param y  Y position
 * @param z  Z position
 **/
//void goSignal3DGenericConstIterator::setPosition (goIndex_t x, goIndex_t y, goIndex_t z)
//{
//    posX = x; posY = y; posZ = z;
//    dx = sig->getXDiff() + x;
//    dy = sig->getYDiff() + y;
//    dz = sig->getZDiff() + z;
//    px = (goByte*)sig->getPtr(x,y,z);
//    py = (goByte*)sig->getPtr(x,y,z);
//    pz = (goByte*)sig->getPtr(x,y,z);
//}

/**
 * @brief Resets the Z pointer and internals concerning Z to initial values.
 *
 * Calls resetY() and resetX().
 **/
void goSignal3DGenericConstIterator::resetZ ()
{
    dz   = sig->getZDiff();
    posZ = 0;
    pz   = (goByte*)sig->getPtr(0,0,0);
    resetY ();
    resetX ();
}

