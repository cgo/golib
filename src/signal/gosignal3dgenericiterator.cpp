/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 */

#include <gosignal3dbase.h>
#include <gosignal3dgenericiterator.h>

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
void goSignal3DGenericIterator::setPosition (goIndex_t x, goIndex_t y, goIndex_t z)
{
    posX = x; posY = y; posZ = z;
    dx = sig->getXDiff() + x;
    dy = sig->getYDiff() + y;
    dz = sig->getZDiff() + z;
    px = (goByte*)sig->getPtr(x,y,z);
    py = (goByte*)sig->getPtr(x,y,z);
    pz = (goByte*)sig->getPtr(x,y,z);
}

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

#if 0
bool goSignal3DGenericIterator::endX ()
{
    return posX > maxX;
}

bool goSignal3DGenericIterator::endY ()
{
    return posY > maxY;
}

bool goSignal3DGenericIterator::endZ ()
{
    return posZ > maxZ;
}

void goSignal3DGenericIterator::incrementX ()
{
    px += *dx;
    ++posX;
    ++dx;
}

void goSignal3DGenericIterator::incrementY ()
{
    py += *dy;
    ++posY;
    ++dy;
}

void goSignal3DGenericIterator::incrementZ ()
{
    pz += *dz;
    ++posZ;
    ++dz;
}

void goSignal3DGenericIterator::decrementX ()
{
    px -= *(dx-1);
    --posX;
    --dx;
}

void goSignal3DGenericIterator::decrementY ()
{
    py -= *(dy-1);
    --posY;
    --dy;
}

void goSignal3DGenericIterator::decrementZ ()
{
    pz -= *(dz-1);
    --posZ;
    --dz;
}

void goSignal3DGenericIterator::resetX ()
{
    dx   = sig->getXDiff();
    posX = 0;
    px   = py;
}

void goSignal3DGenericIterator::resetY ()
{
    dy   = sig->getYDiff();
    posY = 0;
    py   = pz;
    resetX ();
}

void goSignal3DGenericIterator::resetZ ()
{
    dz   = sig->getZDiff();
    posZ = 0;
    pz   = (goByte*)sig->getPtr(0,0,0);
    resetY ();
    resetX ();
}

goByte* goSignal3DGenericIterator::operator* ()
{
    return px;
}

const goByte* goSignal3DGenericIterator::operator* () const
{
    return px;
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
void goSignal3DGenericConstIterator::setPosition (goIndex_t x, goIndex_t y, goIndex_t z)
{
    posX = x; posY = y; posZ = z;
    dx = sig->getXDiff() + x;
    dy = sig->getYDiff() + y;
    dz = sig->getZDiff() + z;
    px = (goByte*)sig->getPtr(x,y,z);
    py = (goByte*)sig->getPtr(x,y,z);
    pz = (goByte*)sig->getPtr(x,y,z);
}

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

