/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


//#include <gosignal3d.h>
//#include <gosubsignal3d.h>
#include <gosignal3dbase.h>
#include <gosignalmacros.h>
#include <go3vector.h>
#include <golog.h>
#include <goconfig.h>
#include <godefs.h>
#include <gocomplex.h>
#include <gomath.h>
#include <string.h> // bzero()
#include <iostream>


template< class T >
goSignal3DBase<T>::goSignal3DBase () 
    : 
    goObjectBase  (),
    ptr           (NULL),
    real_ptr      (NULL),
    xDiff         (NULL),
    yDiff         (NULL),
    zDiff         (NULL),
    real_xDiff    (NULL),
    real_yDiff    (NULL),
    real_zDiff    (NULL),
    myXJump       (NULL),
    myYJump       (NULL),
    myZJump       (NULL),
    real_myXJump  (NULL),
    real_myYJump  (NULL),
    real_myZJump  (NULL),
    myChannelOffset (NULL),
    mySize        (0, 0, 0),
    myBorderSize  (0, 0, 0),
    myBlockSize   (1, 1, 1),
    myDataType    (GO_UINT8),
    myChannelCount (1),
    myChannel     (0),
    myBorderFlags (3),
    myChangedCaller ()
{
    myBorderFlags.fill (GO_PERIODIC_BORDER);
    this->initializeDataType ();
    // this->setClassName ("goSignal3DBase");
    this->setClassID(GO_SIGNAL3DBASE);
}

template< class T >
goSignal3DBase<T>::~goSignal3DBase () 
{
    this->destroy();
}

template<class T>
goSignal3DBase<T>::goSignal3DBase (goSignal3DBase<T>& other)
  : 
  goObjectBase (),
  ptr        (NULL),
    real_ptr     (NULL),
    xDiff        (NULL),
    yDiff        (NULL),
    zDiff        (NULL),
    real_xDiff   (NULL),
    real_yDiff   (NULL),
    real_zDiff   (NULL),
    myXJump      (NULL),
    myYJump      (NULL),
    myZJump      (NULL),
    real_myXJump (NULL),
    real_myYJump (NULL),
    real_myZJump (NULL),
    myChannelOffset (NULL),
    mySize       (0, 0, 0),
    myBorderSize (0, 0, 0),
    myBlockSize  (1, 1, 1),
    myDataType   (GO_UINT8),
    myChannelCount (1),
    myChannel     (0),
    myBorderFlags (3),
    myChangedCaller ()
{
    myBorderFlags.fill (GO_PERIODIC_BORDER);
    this->initializeDataType ();
    // this->setClassName ("goSignal3DBase");
    this->setClassID(GO_SIGNAL3DBASE);
    this->reference(other);
}

#define INITIALIZE_DATATYPE_METHOD(TYPEENUM) {\
    { \
        return myDataType.setID (TYPEENUM); \
    } \
}

template<> bool
goSignal3DBase<goInt8>::initializeDataType ()
INITIALIZE_DATATYPE_METHOD(GO_INT8);
template<> bool
goSignal3DBase<goInt16>::initializeDataType ()
INITIALIZE_DATATYPE_METHOD(GO_INT16);
template<> bool
goSignal3DBase<goInt32>::initializeDataType ()
INITIALIZE_DATATYPE_METHOD(GO_INT32);
#ifdef HAVE_INT64
template<> bool
goSignal3DBase<goInt64>::initializeDataType ()
INITIALIZE_DATATYPE_METHOD(GO_INT64);
#endif
template<> bool
goSignal3DBase<goUInt8>::initializeDataType ()
INITIALIZE_DATATYPE_METHOD(GO_UINT8);
template<> bool
goSignal3DBase<goUInt16>::initializeDataType ()
INITIALIZE_DATATYPE_METHOD(GO_UINT16);
template<> bool
goSignal3DBase<goUInt32>::initializeDataType ()
INITIALIZE_DATATYPE_METHOD(GO_UINT32);
template<> bool
goSignal3DBase<goFloat>::initializeDataType ()
INITIALIZE_DATATYPE_METHOD(GO_FLOAT);
template<> bool
goSignal3DBase<goDouble>::initializeDataType ()
INITIALIZE_DATATYPE_METHOD(GO_DOUBLE);
template<> bool
goSignal3DBase<void*>::initializeDataType ()
INITIALIZE_DATATYPE_METHOD(GO_VOID_POINTER);
// Initialize the generic container with uint8 -- 
// the type of the generic container can be changed with setDataType()
template<> bool
goSignal3DBase<void>::initializeDataType ()
INITIALIZE_DATATYPE_METHOD(GO_UINT8);

template<> bool
goSignal3DBase<goComplexf>::initializeDataType ()
INITIALIZE_DATATYPE_METHOD(GO_COMPLEX_SINGLE);

#undef INITIALIZE_DATATYPE_METHOD

template <class T>
bool
goSignal3DBase<T>::initializeDataType ()
{
    goLog::error ("Unknown data type in initialization.",this);
    assert ("Unknown data type" == NULL);
    return false;
}

#define GET_PTR(type) (*(const type*) ((goUInt8*)ptr + myZJump[z] + myYJump[y] + myXJump[x] + myChannelOffset[channel]))
template <>
goDouble
goSignal3DBase<void>::getValue (goIndex_t x, goIndex_t y, goIndex_t z, goSize_t channel) const
{
    switch (this->getDataType().getID())
    {
        case GO_UINT8:  return static_cast<goDouble> (GET_PTR(goUInt8)); break;
        case GO_INT8:   return static_cast<goDouble> (GET_PTR(goInt8)); break;
        case GO_UINT16: return static_cast<goDouble> (GET_PTR(goUInt16)); break;
        case GO_INT16:  return static_cast<goDouble> (GET_PTR(goInt16)); break;
        case GO_UINT32: return static_cast<goDouble> (GET_PTR(goUInt32)); break;
        case GO_INT32:  return static_cast<goDouble> (GET_PTR(goInt32)); break;
        case GO_FLOAT:  return static_cast<goDouble> (GET_PTR(goFloat)); break;
        case GO_DOUBLE: return static_cast<goDouble> (GET_PTR(goDouble)); break;
        default: goLog::error ("goSignal3DBase::getValue(): unsupported data type."); 
                 return 0.0; 
                 break;
    }
}
#undef GET_PTR

template <>
goDouble
goSignal3DBase<void*>::getValue (goIndex_t x, goIndex_t y, goIndex_t z, goSize_t channel) const
{
    goLog::error ("goSignal3DBase::getValue() does not work for void* type.");
    return 0.0;
}

/** 
 * @brief Convenience function to get a value.
 *
 * @note This is slow.
 *
 * Get a value at a specific position conveniently and type independent.
 * While this is ok for a single value, it is slow for use in a loop.
 * Use the goSignal3DGenericIterator family of classes for fast iteration over signals.
 * 
 * @param x X coordinate 
 * @param y Y coordinate
 * @param z Z coordinate
 * @param channel Channel number
 *
 * @return The value at the given position.
 */
template <class T>
goDouble
goSignal3DBase<T>::getValue (goIndex_t x, goIndex_t y, goIndex_t z, goSize_t channel) const
{
    return static_cast<goDouble> (*(const T*) ((goUInt8*)ptr + myZJump[z] + myYJump[y] + myXJump[x] + myChannelOffset[channel]));
}

#define GET_PTR(type) (*(type*) ((goUInt8*)ptr + myZJump[z] + myYJump[y] + myXJump[x] + myChannelOffset[channel]))
template <>
void
goSignal3DBase<void>::setValue (goDouble value, goIndex_t x, goIndex_t y, goIndex_t z, goSize_t channel)
{
    switch (this->getDataType().getID())
    {
        case GO_UINT8:  GET_PTR(goUInt8)  = static_cast<goUInt8> (value);   break;
        case GO_INT8:   GET_PTR(goInt8)   = static_cast<goInt8> (value);    break;
        case GO_UINT16: GET_PTR(goUInt16) = static_cast<goUInt16> (value);  break;
        case GO_INT16:  GET_PTR(goInt16)  = static_cast<goInt16> (value);   break;
        case GO_UINT32: GET_PTR(goUInt32) = static_cast<goUInt32> (value);  break;
        case GO_INT32:  GET_PTR(goInt32)  = static_cast<goInt32> (value);   break;
        case GO_FLOAT:  GET_PTR(goFloat)  = static_cast<goFloat> (value);   break;
        case GO_DOUBLE: GET_PTR(goDouble) = static_cast<goDouble> (value);  break;
        default: goLog::error ("goSignal3DBase::setValue(): unsupported data type."); 
                 break;
    }
}
#undef GET_PTR

template <>
void
goSignal3DBase<void*>::setValue (goDouble value, goIndex_t x, goIndex_t y, goIndex_t z, goSize_t channel)
{
    goLog::error ("goSignal3DBase::setValue() does not work for void* type.");
}

/** 
 * @brief Convenience function to set a value.
 *
 * @note This is slow.
 *
 * Set a value at a specific position conveniently and type independent.
 * While this is ok for a single value, it is slow for use in a loop.
 * Use the goSignal3DGenericIterator family of classes for fast iteration over signals.
 * 
 * @param value Value. Will be cast to the appropriate data type.
 * @param x X coordinate 
 * @param y Y coordinate
 * @param z Z coordinate
 * @param channel Channel number
 */
template <class T>
void
goSignal3DBase<T>::setValue (goDouble value, goIndex_t x, goIndex_t y, goIndex_t z, goSize_t channel)
{
    *(T*) ((goUInt8*)ptr + myZJump[z] + myYJump[y] + myXJump[x] + myChannelOffset[channel]) = static_cast<T> (value);
}

template<> void*
goSignal3DBase<void>::getPtr (goIndex_t x, goIndex_t y, goIndex_t z)
{
    return (goUInt8*)ptr + myZJump[z] + myYJump[y] + myXJump[x] + myChannelOffset[myChannel];
}

template<> const void*
goSignal3DBase<void>::getClosest (go3Vector<goFloat>& point) const
{
    return ((goUInt8*)getPtr ((int)point.x, (int)point.y, (int)point.z));
}

template<> goFloat
goSignal3DBase<void*>::sample(go3Vector<goFloat>& point)
{
	return 0.0f;
}

template<> goFloat
goSignal3DBase<void>::sample(go3Vector<goFloat>& point)
{
    goLog::warning ("sample() not implemented for <void>. Contact the author.",this);
	return 0.0f;
}

template <class T>
void goSignal3DBase<T>::cleanup ()
{
    if (real_xDiff)
    {
            delete[] real_xDiff;
            xDiff = NULL;
            real_xDiff = NULL;
    }
    if (real_yDiff)
    {
            delete[] real_yDiff;
            yDiff = NULL;
            real_yDiff = NULL;
    }
    if (real_zDiff)
    {
            delete[] real_zDiff;
            zDiff = NULL;
            real_zDiff = NULL;
    }
    if (real_myXJump)
    {
            delete[] real_myXJump;
            myXJump = NULL;
            real_myXJump = NULL;
    }
    if (real_myYJump)
    {
            delete[] real_myYJump;
            myYJump = NULL;
            real_myYJump = NULL;
    }
    if (real_myZJump)
    {
            delete[] real_myZJump;
            myZJump = NULL;
            real_myZJump = NULL;
    }
    if (myChannelOffset)
    {
            delete[] myChannelOffset;
            myChannelOffset = NULL;
    }

    this->mySize       = goSize3D (0,0,0);
    this->myBorderSize = goSize3D (0,0,0);
    this->myBlockSize  = goSize3D (0,0,0);
    this->myBlocks     = goSize3D (0,0,0);
}

template <class T>
bool
goSignal3DBase<T>::initialize (T*       dataptr,
                               goSize_t x, goSize_t y, goSize_t z,
                               goSize_t blockSizeX, 
                               goSize_t blockSizeY, 
                               goSize_t blockSizeZ,
                               goSize_t border_x, 
                               goSize_t border_y, 
                               goSize_t border_z,
                               goSize_t channelCount)
{
    this->cleanup();

    myBorderSize.x = border_x;
    myBorderSize.y = border_y;
    myBorderSize.z = border_z;
    myBlockSize.x  = blockSizeX;
    myBlockSize.y  = blockSizeY;
    myBlockSize.z  = blockSizeZ;
    
    mySize.x = x;
    mySize.y = y;
    mySize.z = z;
    myChannelCount = channelCount;
    
    myBlocks.x = (mySize.x + myBlockSize.x - 1) / myBlockSize.x;
    myBlocks.y = (mySize.y + myBlockSize.y - 1) / myBlockSize.y;
    myBlocks.z = (mySize.z + myBlockSize.z - 1) / myBlockSize.z;
    
    real_xDiff   = new goPtrdiff_t[mySize.x + 2 * myBorderSize.x];
    real_yDiff   = new goPtrdiff_t[mySize.y + 2 * myBorderSize.y];
    real_zDiff   = new goPtrdiff_t[mySize.z + 2 * myBorderSize.z];
    
    real_myXJump = new goPtrdiff_t[mySize.x + 2 * myBorderSize.x];
    real_myYJump = new goPtrdiff_t[mySize.y + 2 * myBorderSize.y];
    real_myZJump = new goPtrdiff_t[mySize.z + 2 * myBorderSize.z];
   
    myChannelOffset = new goPtrdiff_t[myChannelCount];

    if (!real_xDiff || !real_yDiff || !real_zDiff || !real_myXJump || !real_myYJump || !real_myZJump || !myChannelOffset)
    {
        return false;
    }
    
    xDiff = real_xDiff + myBorderSize.x;
    yDiff = real_yDiff + myBorderSize.y;
    zDiff = real_zDiff + myBorderSize.z;
    
    myXJump = real_myXJump + myBorderSize.x;
    myYJump = real_myYJump + myBorderSize.y;
    myZJump = real_myZJump + myBorderSize.z;
    
    goIndex_t i;
    // Channel data are stored one element after another for now.
    for (i = 0; i < (goIndex_t)myChannelCount; ++i)
    {
        myChannelOffset[i] = i;
    }
    
    real_ptr = dataptr;
    ptr      = real_ptr;

    goPtrdiff_t blockJump  = myBlockSize.x * 
                             myBlockSize.y * myBlockSize.z * myChannelCount;
    goPtrdiff_t blockJumpY = blockJump * myBlocks.x; 
    goPtrdiff_t blockJumpZ = blockJumpY * myBlocks.y;
   
    for (i = 0; i < (goIndex_t)mySize.x; ++i)
    {
        xDiff[i]   = 1 * myChannelCount;
    }

    for (i = (goIndex_t)myBlockSize.x - 1; i < (goIndex_t)mySize.x; i += myBlockSize.x)
    {
        xDiff[i] = blockJump - (myBlockSize.x + 1)*myChannelCount;
    }
    
    for (i = 0; i < (goIndex_t)mySize.y; ++i)
    {
        yDiff[i] = myBlockSize.x * myChannelCount;
    }

    for (i = (goIndex_t)myBlockSize.y - 1; i < (goIndex_t)mySize.y; i += myBlockSize.y)
    {
        yDiff[i] = blockJumpY - (myBlockSize.x * (myBlockSize.y - 1)) * myChannelCount;
    }

    for (i = 0; i < (goIndex_t)mySize.z; ++i)
    {
        //= FIXME: Why is this myChannelCount**2 ?
        //= Wed Jan  2 14:28:28 CET 2008
        zDiff[i] = myBlockSize.x * myBlockSize.y * myChannelCount * myChannelCount;
    }

    for (i = (goIndex_t)myBlockSize.z - 1; i < (goIndex_t)mySize.z; i += myBlockSize.z)
    {
        zDiff[i] = blockJumpZ - (myBlockSize.x * myBlockSize.y * (myBlockSize.z - 1)) * myChannelCount;
    }


    goPtrdiff_t currentJump = 0;
    goSize_t j;
    for (j = 0; j < mySize.x; ++j)
    {
        if ((j % myBlockSize.x) == 0)
        {
            currentJump = blockJump * j / myBlockSize.x;
        }
        myXJump[j] = currentJump;
        currentJump += myChannelCount;
    }

    currentJump = 0;
    for (j = 0; j < mySize.y; ++j)
    {
        if ((j % myBlockSize.y) == 0)
        {
            currentJump = blockJumpY * j / myBlockSize.y;
        }
        myYJump[j] = currentJump;
        currentJump += myBlockSize.x * myChannelCount;
    }
    
    currentJump = 0;
    for (j = 0; j < mySize.z; ++j)
    {
        if ((j % myBlockSize.z) == 0)
        {
            currentJump = blockJumpZ * j / myBlockSize.z;
        }
        myZJump[j] = currentJump;
        currentJump += myBlockSize.x * myBlockSize.y * myChannelCount;
    }

    // Periodize signal over the border
    // this->periodicBorders();
    this->applyBorderFlags();
    return true;
}

template <class T>
void
goSignal3DBase<T>::constantBorders (int axes)
{
    goIndex_t j;
    goIndex_t i;
   
    if (axes & GO_X)
    {
        //goLog::message ("constantBorders() to X",this);
        goIndex_t sz = static_cast<goIndex_t>(this->getSizeX());
        goIndex_t border = this->getBorderX();
        this->xDiff[sz - 1] = 0;
        for (i = -border; i < 0; ++i)
        {
            xDiff[i]   = 0;
            myXJump[i] = myXJump[0];
        }

        j = 0;
        goIndex_t sz_border = sz + border;
        for (i = sz; i < sz_border; ++i)
        {
            xDiff[i] = 0;
            myXJump[i] = myXJump[sz-1];
        }
//        if (border > 0)
//        {
//            xDiff[-1] = myXJump[0] - myXJump[-1];
//        }
    }
    if (axes & GO_Y)
    {
        //goLog::message ("constantBorders() to Y",this);
        goIndex_t sz = static_cast<goIndex_t>(this->getSizeY());
        goIndex_t border = this->getBorderY();
        this->yDiff[sz - 1] = 0;
        for (i = -border; i < 0; ++i)
        {
            yDiff[i]   = 0;
            myYJump[i] = myYJump[0];
        }

        j = 0;
        goIndex_t sz_border = sz + border;
        for (i = sz; i < sz_border; ++i)
        {
            yDiff[i] = 0;
            myYJump[i] = myYJump[sz-1];
        }
    }
    if (axes & GO_Z)
    {
        //goLog::message ("constantBorders() to Z",this);
        goIndex_t sz = static_cast<goIndex_t>(this->getSizeZ());
        goIndex_t border = this->getBorderZ();
        this->zDiff[sz - 1] = 0;
        for (i = -border; i < 0; ++i)
        {
            zDiff[i]   = 0;
            myZJump[i] = myZJump[0];
        }

        j = 0;
        goIndex_t sz_border = sz + border;
        for (i = sz; i < sz_border; ++i)
        {
            zDiff[i] = 0;
            myZJump[i] = myZJump[sz-1];
        }
    }
}

template <class T>
void
goSignal3DBase<T>::periodicBorders (int axes)
{
    // Periodize signal over the border

    goIndex_t j;
    goIndex_t i;
   
    if (axes & GO_X)
    {
        this->xDiff[getSizeX() - 1] = this->myXJump[0] - this->myXJump[getSizeX() - 1] ;
        j = getSizeX() - getBorderX();
        for (i = -((goIndex_t)getBorderX()); i < 0; ++i, ++j)
        {
            xDiff[i]   = xDiff[j];
            myXJump[i] = myXJump[j];
        }

        j = 0;
        for (i = getSizeX(); i < (goIndex_t)getSizeX() + getBorderX(); ++i, ++j)
        {
            xDiff[i] = xDiff[j];
            myXJump[i] = myXJump[j];
        }
        if (getBorderX() > 0)
        {
            xDiff[-1] = myXJump[0] - myXJump[-1];
        }
    }

    if (axes & GO_Y)
    {
        this->yDiff[getSizeY() - 1] = this->myYJump[0] - this->myYJump[getSizeY() - 1] ;
        j = getSizeY() - getBorderY();
        for (i = -((goIndex_t)getBorderY()); i < 0; ++i, ++j)
        {
            yDiff[i]   = yDiff[j];
            myYJump[i] = myYJump[j];
        }

        j = 0;
        for (i = getSizeY(); i < (goIndex_t)getSizeY() + getBorderY(); ++i, ++j)
        {
            yDiff[i] = yDiff[j];
            myYJump[i] = myYJump[j];
        }
        if (getBorderY() > 0)
        {
            yDiff[-1] = myYJump[0] - myYJump[-1];
        }
    }

    if (axes & GO_Z)
    {
        this->zDiff[getSizeZ() - 1] = this->myZJump[0] - this->myZJump[getSizeZ() - 1] ;
        j = getSizeZ() - getBorderZ();
        for (i = -((goIndex_t)getBorderZ()); i < 0; ++i, ++j)
        {
            zDiff[i]   = zDiff[j];
            myZJump[i] = myZJump[j];
        }

        j = 0;
        for (i = getSizeZ(); i < (goIndex_t)getSizeZ() + getBorderZ(); ++i, ++j)
        {
            zDiff[i] = zDiff[j];
            myZJump[i] = myZJump[j];
        }
        if (getBorderZ() > 0)
        {
            zDiff[-1] = myZJump[0] - myZJump[-1];
        }
    }
}

template<> bool
goSignal3DBase<void>::initialize (void*    dataptr,
                                  goSize_t x, goSize_t y, goSize_t z,
                                  goSize_t blockSizeX, 
                                  goSize_t blockSizeY, 
                                  goSize_t blockSizeZ,
                                  goSize_t border_x, 
                                  goSize_t border_y, 
                                  goSize_t border_z,
                                  goSize_t channelCount)
{
    this->cleanup();
    
    myBorderSize.x = border_x;
    myBorderSize.y = border_y;
    myBorderSize.z = border_z;
    myBlockSize.x  = blockSizeX;
    myBlockSize.y  = blockSizeY;
    myBlockSize.z  = blockSizeZ;
    
    mySize.x = x;
    mySize.y = y;
    mySize.z = z;
    myChannelCount = channelCount;
  
    myBlocks.x = (mySize.x + myBlockSize.x - 1) / myBlockSize.x;
    myBlocks.y = (mySize.y + myBlockSize.y - 1) / myBlockSize.y;
    myBlocks.z = (mySize.z + myBlockSize.z - 1) / myBlockSize.z;
    
    real_xDiff   = new goPtrdiff_t[mySize.x + 2 * myBorderSize.x];
    real_yDiff   = new goPtrdiff_t[mySize.y + 2 * myBorderSize.y];
    real_zDiff   = new goPtrdiff_t[mySize.z + 2 * myBorderSize.z];
    
    real_myXJump = new goPtrdiff_t[mySize.x + 2 * myBorderSize.x];
    real_myYJump = new goPtrdiff_t[mySize.y + 2 * myBorderSize.y];
    real_myZJump = new goPtrdiff_t[mySize.z + 2 * myBorderSize.z];
   
    myChannelOffset = new goPtrdiff_t[myChannelCount];

    if (!real_xDiff || !real_yDiff || !real_zDiff || !real_myXJump || !real_myYJump || !real_myZJump || !myChannelOffset)
    {
        return false;
    }
    
    xDiff = real_xDiff + myBorderSize.x;
    yDiff = real_yDiff + myBorderSize.y;
    zDiff = real_zDiff + myBorderSize.z;
    
    myXJump = real_myXJump + myBorderSize.x;
    myYJump = real_myYJump + myBorderSize.y;
    myZJump = real_myZJump + myBorderSize.z;
    
    goIndex_t i;
    goPtrdiff_t elementSize = myDataType.getSize();
    // Channel data are stored one element after another for now.
    for (i = 0; i < (goIndex_t)myChannelCount; ++i)
    {
        myChannelOffset[i] = i * elementSize;
    }

    real_ptr = dataptr;
    ptr      = real_ptr;

    goPtrdiff_t blockJump  = myBlockSize.x * 
                             myBlockSize.y * myBlockSize.z * elementSize * myChannelCount;
    goPtrdiff_t blockJumpY = blockJump * myBlocks.x; 
    goPtrdiff_t blockJumpZ = blockJumpY * myBlocks.y;
  
    for (i = 0; i < (goIndex_t)mySize.x; ++i)
    {
        xDiff[i]   = 1 * elementSize * myChannelCount;
    }

    for (i = (goIndex_t)myBlockSize.x - 1; i < (goIndex_t)mySize.x; i += myBlockSize.x)
    {
        xDiff[i] = blockJump - ((myBlockSize.x - 1) * elementSize * myChannelCount);
    }
    
    for (i = 0; i < (goIndex_t)mySize.y; ++i)
    {
        yDiff[i] = myBlockSize.x * elementSize * myChannelCount;
    }

    for (i = (goIndex_t)myBlockSize.y - 1; i < (goIndex_t)mySize.y; i += myBlockSize.y)
    {
        yDiff[i] = blockJumpY - (myBlockSize.x * (myBlockSize.y - 1) * elementSize * myChannelCount);
    }

    for (i = 0; i < (goIndex_t)mySize.z; ++i)
    {
        zDiff[i] = myBlockSize.x * myBlockSize.y * elementSize * myChannelCount;
    }

    for (i = (goIndex_t)myBlockSize.z - 1; i < (goIndex_t)mySize.z; i += myBlockSize.z)
    {
        zDiff[i] = blockJumpZ - (myBlockSize.x * myBlockSize.y * (myBlockSize.z - 1) * elementSize * myChannelCount);
    }

    // Du bloeder, kleiner Scheiss-Idiot.  FIXME
    // I don't think there is anything left here to fix. 
    // The comment will remain for historic reasons.
//    blockJump   = myBlockSize.x * myBlockSize.y * myBlockSize.z;
//    blockJumpY  = blockJump * myBlocks.x; 
//    blockJumpZ  = blockJumpY * myBlocks.y;
    goPtrdiff_t currentJump = 0;
    goSize_t j;
    for (j = 0; j < mySize.x; ++j)
    {
        if ((j % myBlockSize.x) == 0)
        {
            currentJump = blockJump * j / myBlockSize.x;
        }
        myXJump[j] = currentJump; // * elementSize * myChannelCount;
        currentJump += elementSize * myChannelCount;
    }

    currentJump = 0;
    for (j = 0; j < mySize.y; ++j)
    {
        if ((j % myBlockSize.y) == 0)
        {
            currentJump = blockJumpY * j / myBlockSize.y;
        }
        myYJump[j] = currentJump; //  * elementSize * myChannelCount;
        currentJump += myBlockSize.x * elementSize * myChannelCount;
    }
    
    currentJump = 0;
    for (j = 0; j < mySize.z; ++j)
    {
        if ((j % myBlockSize.z) == 0)
        {
            currentJump = blockJumpZ * j / myBlockSize.z;
        }
        myZJump[j] = currentJump; // * elementSize * myChannelCount;
        currentJump += myBlockSize.x * myBlockSize.y * elementSize * myChannelCount;
    }


    // Periodize signal over the border
    // this->periodicBorders();
    this->applyBorderFlags();
    return true;

#if 0
    xDiff[getSizeX() - 1] = myXJump[0] - myXJump[getSizeX() - 1] ;
    yDiff[getSizeY() - 1] = myYJump[0] - myYJump[getSizeY() - 1] ;
    zDiff[getSizeZ() - 1] = myZJump[0] - myZJump[getSizeZ() - 1] ;
    
    j = getSizeX() - getBorderX();

    for (i = -((goIndex_t)getBorderX()); i < 0; ++i, ++j)
    {
        xDiff[i]   = xDiff[j];
        myXJump[i] = myXJump[j];
    }

    j = 0;
    for (i = getSizeX(); i < getSizeX() + getBorderX(); ++i, ++j)
    {
        xDiff[i] = xDiff[j];
        myXJump[i] = myXJump[j];
    }

    j = getSizeY() - getBorderY();
    for (i = -((goIndex_t)getBorderY()); i < 0; ++i, ++j)
    {
        yDiff[i]   = yDiff[j];
        myYJump[i] = myYJump[j];
    }

    j = 0;
    for (i = getSizeY(); i < getSizeY() + getBorderY(); ++i, ++j)
    {
        yDiff[i] = yDiff[j];
        myYJump[i] = myYJump[j];
    }

    j = getSizeZ() - getBorderZ();
    for (i = -((goIndex_t)getBorderZ()); i < 0; ++i, ++j)
    {
        zDiff[i]   = zDiff[j];
        myZJump[i] = myZJump[j];
    }

    j = 0;
    for (i = getSizeZ(); i < getSizeZ() + getBorderZ(); ++i, ++j)
    {
        zDiff[i] = zDiff[j];
        myZJump[i] = myZJump[j];
    }

    
    if (getBorderX() > 0)
    {
        xDiff[-1] = myXJump[0] - myXJump[-1];
    }
    if (getBorderY() > 0)
    {
        yDiff[-1] = myYJump[0] - myYJump[-1];
    }
    if (getBorderZ() > 0)
    {
        zDiff[-1] = myZJump[0] - myZJump[-1];
    }
    
    return true;
#endif
}


template<class T>
void
goSignal3DBase<T>::destroy ()
{
    this->cleanup();
}

template<class T>
goSize_t
goSignal3DBase<T>::memoryUsage() const
{
    if (real_ptr) 
    {
        return (goSize_t)(sizeof(T) * getSizeX() * getSizeY() * getSizeZ() * this->getChannelCount());
    }

    return 0;
}

template<class T>
void
goSignal3DBase<T>::setChanged ()
{
    this->myChangedCaller ();
    this->sendObjectMessage (GO_OBJECTMESSAGE_CHANGED);
}

template <class T>
goCaller0<void>& goSignal3DBase<T>::getChangedCaller ()
{
    return this->myChangedCaller;
}

template<> goSize_t
goSignal3DBase<void>::memoryUsage() const
{
    if (real_ptr) 
    {
        return (goSize_t)(myDataType.getSize() * getSizeX() * getSizeY() * getSizeZ() * this->getChannelCount());
    }

    return 0;
}

template< class T >
void
goSignal3DBase<T>::setPtr(T *p)
{
    ptr = p;
}

template< class T >
const goSignal3DBase<T>&
goSignal3DBase<T>::reference (goSignal3DBase<T> &other) {
    this->destroy();
    goLog::warning("reference(): Referencing another goSignal3DBase. This is NOT a operator=().",this); 
    this->initialize (other.getRealPtr (),
                      other.getSizeX(), other.getSizeY(), other.getSizeZ(),
                      other.getBlockSizeX(), other.getBlockSizeY(), other.getBlockSizeZ(),
                      other.getBorderX(), other.getBorderY(), other.getBorderZ(), other.getChannelCount());
    
    return *this;
}

template< class T >
bool
goSignal3DBase<T>::operator== (goSignal3DBase<T> &other) {
    goSize_t x,y,z;
    goSize_t thisX, thisY, thisZ;
    x = other.getSizeX();
    y = other.getSizeY();
    z = other.getSizeZ();
    thisX = getSizeX();
    thisY = getSizeY();
    thisZ = getSizeZ();

    if ( (thisX != x) || (thisY != y) || (thisZ != z) ) {
        return false;
    }

    goPtrdiff_t* oxd = other.getXDiff();
    goPtrdiff_t* xd  = getXDiff();

    T *op;
    T *p;

    for (z = 0; z < thisZ; ++z)
    {
        for (y = 0; y < thisY; ++y)
        {
            p  = getPtr (0, y, z);
            op = other.getPtr (0, y, z);
            for (x = 0; x < thisX; ++x)
            {
                if (*p != *op)
                {
                    return false;
                }
                p  += xd[x];
                op += oxd[x];
            }
        }
    }
    
    return true;
}

template<> bool
goSignal3DBase<void>::operator== (goSignal3DBase<void> &other) 
{
    goLog::warning("goSignal3DBase<void>: operator== not implemented for void.");
    return false;
}

template<> goDouble
goSignal3DBase<void*>::getMaximum() const
{
    return 0.0;
}

template<> goDouble
goSignal3DBase<goComplex<goFloat> >::getMaximum() const
{
    return 0.0;
}

template<> goDouble
goSignal3DBase<void>::getMaximum() const
{
    const goUInt8* p = (goUInt8*)this->getPtr();
    goSize_t x,y,z;
    goSize_t xSize, ySize, zSize;
    xSize = getSizeX();
    ySize = getSizeY();
    zSize = getSizeZ();

    const void* maxVal = p;
    goCompareFunction greaterThan = getDataType().getGreaterThanFunction();
    for (z = 0; z < zSize; z++)
    {
        for (y = 0; y < ySize; y++)
        {
            p = (goUInt8*)getPtr (0, y, z);
            for (x = 0; x < xSize; x++)
            {
                if (greaterThan(p,maxVal))
                {
                    maxVal = p;
                }
                p += xDiff[x];
            }
        }
    }

    switch (getDataType().getID())
    {
        case GO_INT8:         return (goDouble)*(const goInt8*)maxVal; break;
        case GO_INT16:        return (goDouble)*(const goInt16*)maxVal; break;
        case GO_INT32:        return (goDouble)*(const goInt32*)maxVal; break;
#ifdef HAVE_INT64                              
        case GO_INT64:        return (goDouble)*(const goInt64*)maxVal; break;
#endif                              
        case GO_UINT8:        return (goDouble)*(const goUInt8*)maxVal; break;
        case GO_UINT16:       return (goDouble)*(const goUInt16*)maxVal; break;
        case GO_UINT32:       return (goDouble)*(const goUInt32*)maxVal; break;
        case GO_FLOAT:        return (goDouble)*(const goFloat*)maxVal; break;
        case GO_DOUBLE:       return (goDouble)*(const goDouble*)maxVal; break;
        default: goLog::warning ("getMaximum() not implemented for this type.",this); return 0.0; break;
    }
    return 0.0;
}

template< class T >
goDouble
goSignal3DBase<T>::getMaximum() const
{
    const T *p = this->getPtr();
    goSize_t x,y,z;
    goSize_t xSize, ySize, zSize;
    xSize = getSizeX();
    ySize = getSizeY();
    zSize = getSizeZ();

    T maxVal = *p;
    for (z = 0; z < zSize; z++)
    {
        for (y = 0; y < ySize; y++)
        {
            p = getPtr (0, y, z);
            for (x = 0; x < xSize; x++)
            {
                if (*p > maxVal)
                {
                    maxVal = *p;
                }
                p += xDiff[x];
            }
        }
    }
    return (goDouble)maxVal;
}

template<> goDouble
goSignal3DBase<void*>::getMinimum() const
{
    return 0.0;
}

template<> goDouble
goSignal3DBase<goComplex<goFloat> >::getMinimum() const
{
    return 0.0;
}


template<> goDouble
goSignal3DBase<void>::getMinimum() const
{
    const goUInt8 *p = (goUInt8*)this->getPtr();
    goSize_t x,y,z;
    goSize_t xSize, ySize, zSize;
    xSize = getSizeX();
    ySize = getSizeY();
    zSize = getSizeZ();

    const goUInt8* minVal = p;
    goCompareFunction lowerThan = getDataType().getLowerThanFunction();
    for (z = 0; z < zSize; z++)
    {
        for (y = 0; y < ySize; y++)
        {
            p = (goUInt8*)getPtr (0, y, z);
            for (x = 0; x < xSize; x++)
            {
                if (lowerThan(p,minVal))
                {
                    minVal = p;
                }
                p += xDiff[x];
            }
        }
    }

    switch (getDataType().getID())
    {
        case GO_INT8:         return (goDouble)*(const goInt8*)minVal; break;
        case GO_INT16:        return (goDouble)*(const goInt16*)minVal; break;
        case GO_INT32:        return (goDouble)*(const goInt32*)minVal; break;
#ifdef HAVE_INT64
        case GO_INT64:        return (goDouble)*(const goInt64*)minVal; break;
#endif
        case GO_UINT8:        return (goDouble)*(const goUInt8*)minVal; break;
        case GO_UINT16:       return (goDouble)*(const goUInt16*)minVal; break;
        case GO_UINT32:       return (goDouble)*(const goUInt32*)minVal; break;
        case GO_FLOAT:        return (goDouble)*(const goFloat*)minVal; break;
        case GO_DOUBLE:       return (goDouble)*(const goDouble*)minVal; break;
        default: goLog::warning ("getMinimum() not implemented for this type.",this); return 0.0; break;
    }
    return 0.0;
}

template<> void
goSignal3DBase<void>::getMinMax(goDouble& minRet, goDouble& maxRet) const
{
    const goUInt8 *p = (goUInt8*)this->getPtr();
    goSize_t x,y,z;
    goSize_t xSize, ySize, zSize;
    xSize = getSizeX();
    ySize = getSizeY();
    zSize = getSizeZ();

    const goUInt8* minVal = p;
    const goUInt8* maxVal = p;
    goCompareFunction lowerThan = getDataType().getLowerThanFunction();
    goCompareFunction greaterThan = getDataType().getGreaterThanFunction();
    for (z = 0; z < zSize; z++)
    {
        for (y = 0; y < ySize; y++)
        {
            p = (goUInt8*)getPtr (0, y, z);
            for (x = 0; x < xSize; x++)
            {
                if (lowerThan(p,minVal))
                {
                    minVal = p;
                }
                if (greaterThan(p,maxVal))
                {
                    maxVal = p;
                }
                p += xDiff[x];
            }
        }
    }

    switch (getDataType().getID())
    {
        case GO_INT8:         
            minRet = (goDouble)*(const goInt8*)minVal; 
            maxRet = (goDouble)*(const goInt8*)maxVal; 
            break;
        case GO_INT16:        
            minRet = (goDouble)*(const goInt16*)minVal; 
            maxRet = (goDouble)*(const goInt16*)maxVal; 
            break;
        case GO_INT32:        
            minRet = (goDouble)*(const goInt32*)minVal; 
            maxRet = (goDouble)*(const goInt32*)maxVal; 
            break;
#ifdef HAVE_INT64
        case GO_INT64:        
            minRet = (goDouble)*(const goInt64*)minVal; 
            maxRet = (goDouble)*(const goInt64*)maxVal; 
            break;
#endif
        case GO_UINT8:        
            minRet = (goDouble)*(const goUInt8*)minVal; 
            maxRet = (goDouble)*(const goUInt8*)maxVal; 
            break;
        case GO_UINT16:       
            minRet = (goDouble)*(const goUInt16*)minVal; 
            maxRet = (goDouble)*(const goUInt16*)maxVal; 
            break;
        case GO_UINT32:       
            minRet = (goDouble)*(const goUInt32*)minVal; 
            maxRet = (goDouble)*(const goUInt32*)maxVal; 
            break;
        case GO_FLOAT:        
            minRet = (goDouble)*(const goFloat*)minVal; 
            maxRet = (goDouble)*(const goFloat*)maxVal; 
            break;
        case GO_DOUBLE:       
            minRet = (goDouble)*(const goDouble*)minVal; 
            maxRet = (goDouble)*(const goDouble*)maxVal; 
            break;
        default: goLog::warning ("getMinimum() not implemented for this type.",this); break;
    }
}

template<class T> void
goSignal3DBase<T>::getMinMax(goDouble& minRet, goDouble& maxRet) const
{
    goLog::warning ("getMinMax() not implemented for this template instantiation.", this);
}
    
template< class T >
goDouble
goSignal3DBase<T>::getMinimum() const
{
    const T *p = this->getPtr();
    goSize_t x,y,z;
    goSize_t xSize, ySize, zSize;
    xSize = getSizeX();
    ySize = getSizeY();
    zSize = getSizeZ();

    T minVal = *p;
    for (z = 0; z < zSize; z++)
    {
        for (y = 0; y < ySize; y++)
        {
            p = getPtr (0, y, z);
            for (x = 0; x < xSize; x++)
            {
                if (*p < minVal)
                {
                    minVal = *p;
                }
                p += xDiff[x];
            }
        }
    }

    return (goDouble)minVal;
}


/** 
 * @brief Fills the current channel.
 * 
 * @param value pointer to a value.
 */
template< class T >
    void
goSignal3DBase<T>::fill (const T* value)
{
    GO_SIGNAL3D_EACHELEMENT(*__ptr = *value, (*this), T);
}

/** 
 * @brief Fill the currently selected channel with a given value.
 * 
 * @param value Pointer to the value (must be of correct type).
 */
template<> void
goSignal3DBase<void>::fill (const void* value)
{
    switch (this->getDataType().getID())
    {
        case GO_INT8:
            GO_SIGNAL3D_EACHELEMENT_GENERIC(*(goInt8*)__ptr = *(goInt8*)value, (*this));
            break;
        case GO_UINT8:
            GO_SIGNAL3D_EACHELEMENT_GENERIC(*(goUInt8*)__ptr = *(goUInt8*)value, (*this));
            break;
        case GO_INT16:
            GO_SIGNAL3D_EACHELEMENT_GENERIC(*(goInt16*)__ptr = *(goInt16*)value, (*this));
            break;
        case GO_UINT16:
            GO_SIGNAL3D_EACHELEMENT_GENERIC(*(goUInt16*)__ptr = *(goUInt16*)value, (*this));
            break;
        case GO_INT32:
            GO_SIGNAL3D_EACHELEMENT_GENERIC(*(goInt32*)__ptr = *(goInt32*)value, (*this));
            break;
        case GO_UINT32:
            GO_SIGNAL3D_EACHELEMENT_GENERIC(*(goUInt32*)__ptr = *(goUInt32*)value, (*this));
            break;
        case GO_FLOAT:
            GO_SIGNAL3D_EACHELEMENT_GENERIC(*(goFloat*)__ptr = *(goFloat*)value, (*this));
            break;
        case GO_DOUBLE:
            GO_SIGNAL3D_EACHELEMENT_GENERIC(*(goDouble*)__ptr = *(goDouble*)value, (*this));
            break;
        default:
            goLog::warning ("goSignal3DBase<void>: fill not yet implemented for this type.");
            break;
    }
}

template<> void
goSignal3DBase<void>::fill (goDouble v)
{
    switch (this->getDataType().getID())
    {
        case GO_INT8:
            {
                goInt8 value = (goInt8)v;
                GO_SIGNAL3D_EACHELEMENT_GENERIC(*(goInt8*)__ptr = value, (*this));
            }
            break;
        case GO_UINT8:
            {
                goUInt8 value = (goUInt8)v;
                GO_SIGNAL3D_EACHELEMENT_GENERIC(*(goUInt8*)__ptr = value, (*this));
            }
            break;
        case GO_INT16:
            {
                goInt16 value = (goInt16)v;
                GO_SIGNAL3D_EACHELEMENT_GENERIC(*(goInt16*)__ptr = value, (*this));
            }
            break;
        case GO_UINT16:
            {
                goUInt16 value = (goUInt16)v;
                GO_SIGNAL3D_EACHELEMENT_GENERIC(*(goUInt16*)__ptr = value, (*this));
            }
            break;
        case GO_INT32:
            {
                goInt32 value = (goInt32)v;
                GO_SIGNAL3D_EACHELEMENT_GENERIC(*(goInt32*)__ptr = value, (*this));
            }
            break;
        case GO_UINT32:
            {
                goUInt32 value = (goUInt32)v;
                GO_SIGNAL3D_EACHELEMENT_GENERIC(*(goUInt32*)__ptr = value, (*this));
            }
            break;
        case GO_FLOAT:
            {
                goFloat value = (goFloat)v;
                GO_SIGNAL3D_EACHELEMENT_GENERIC(*(goFloat*)__ptr = value, (*this));
            }
            break;
        case GO_DOUBLE:
            {
                goDouble value = (goDouble)v;
                GO_SIGNAL3D_EACHELEMENT_GENERIC(*(goDouble*)__ptr = value, (*this));
            }
            break;
        default:
            goLog::warning ("goSignal3DBase<void>: fill not yet implemented for this type.");
            break;
    }
}

template<class T> 
void goSignal3DBase<T>::fill (goDouble v) 
{ 
    T value = (T)v;
    this->fill (&value); 
}
template<>
void goSignal3DBase<void*>::fill (goDouble v) 
{ 
    goLog::warning ("goSignal3DBase::fill(double) not supported for void*.");
}

//template<> void goSignal3DBase<goInt8>::fill (goDouble v) { this->fill ((goInt8)v); }
//template<> void goSignal3DBase<goUInt8>::fill (goDouble v) { this->fill ((goUInt8)v); }
//template<> void goSignal3DBase<goInt16>::fill (goDouble v) { this->fill ((goInt16)v); }
//template<> void goSignal3DBase<goUInt16>::fill (goDouble v) { this->fill ((goUInt16)v); }
//template<> void goSignal3DBase<goInt32>::fill (goDouble v) { this->fill ((goInt32)v); }
//template<> void goSignal3DBase<goUInt32>::fill (goDouble v) { this->fill ((goUInt32)v); }
//template<> void goSignal3DBase<goFloat>::fill (goDouble v) { this->fill ((goFloat)v); }
//template<> void goSignal3DBase<goComplexf>::fill (goDouble v) { goLog::warning("fill(double) in complex signals not supported."); }

template <class T>
void goSignal3DBase<T>::shiftLeftDiff  (int n, int axes)
{
    goLog::warning ("goSignal3DBase<T>::shift*() methods are not defined.");
}
template <class T>
void goSignal3DBase<T>::shiftRightDiff (int n, int axes)
{
    goLog::warning ("goSignal3DBase<T>::shift*() methods are not defined.");
}
template <class T>
void goSignal3DBase<T>::shiftLeftSize  (int n, int axes)
{
    goLog::warning ("goSignal3DBase<T>::shift*() methods are not defined.");
}
template <class T>
void goSignal3DBase<T>::shiftRightSize (int n, int axes)
{
    goLog::warning ("goSignal3DBase<T>::shift*() methods are not defined.");
}

/** 
* @brief Sets the border behaviour.
*
* @note Changes only become valid when the data is newly allocated using make() after a call to setBorderFlags(),
* or when applyBorderFlags() is called after setBorderFlags().
*
* Not all axes need to have the same behaviour. If you mix behaviours, be aware
* that the values in the "corners" of the data set may not be what you expect.
* Currently they are just the values imposed by the last application of 
* internal periodisation/constantisation functions. I.e. the last behaviour,
* probably of the z-axis, is mimicked.
* 
* @todo If border strategies are mixed, make the corners do something defined.
* 
* @param axes Axes to apply borderFlag to. One or more of {GO_X,GO_Y,GO_Z} or'ed together.
*    Default is all axes.
* @param borderFlag Flag to apply to the borders. One of {GO_PERIODIC_BORDER,GO_CONSTANT_BORDER}.
*    Default is periodic for all axes.
*/
template <class T>
void goSignal3DBase<T>::setBorderFlags (int axes, int borderFlag)
{
    if ((borderFlag != GO_PERIODIC_BORDER &&
         borderFlag != GO_CONSTANT_BORDER) ||
        (borderFlag == GO_PARENT_BORDER && this->getClassID() != GO_SUBSIGNAL3D))
    {
        goLog::warning("setBorders(): unknown borderFlag value.",this);
        return;
    }
    if (axes & GO_X)
    {
        this->myBorderFlags[0] = borderFlag;
    }
    if (axes & GO_Y)
    {
        this->myBorderFlags[1] = borderFlag;
    }
    if (axes & GO_Z)
    {
        this->myBorderFlags[2] = borderFlag;
    }
}

template <class T>
void goSignal3DBase<T>::setBorderFlags (const goFixedArray<int>& flags)
{
    if (flags.getSize() != this->myBorderFlags.getSize())
    {
        return;
    }
    goSize_t sz = flags.getSize();
    goSize_t i;
    //= Check for legal values.
    for (i = 0; i < sz; ++i)
    {
        if ((flags[i] != GO_PERIODIC_BORDER && flags[i] != GO_CONSTANT_BORDER) ||
                (flags[i] == GO_PARENT_BORDER && this->getClassID() != GO_SUBSIGNAL3D))
        {
            return;
        }
    }
    
    this->myBorderFlags = flags;
}

template <class T>
const goFixedArray<int>& goSignal3DBase<T>::getBorderFlags () const
{
    return this->myBorderFlags;
}

template <class T>
void goSignal3DBase<T>::applyBorderFlags (int axis)
{
    goIndex_t i;
    switch (axis)
    {
        case GO_X: i = 0; break;
        case GO_Y: i = 1; break;
        case GO_Z: i = 2; break;
        default: return; break;
    }
    switch (this->myBorderFlags[i])
    {
        case GO_PERIODIC_BORDER: this->periodicBorders(axis); break;
        case GO_CONSTANT_BORDER: this->constantBorders(axis); break;
        case GO_PARENT_BORDER: goLog::error ("GO_PARENT_BORDER not yet implemented.", this); break; // No need for action.
        default: goLog::warning("applyBorderFlags(): unknown borderFlag value.",this); break;
    }
}

template <class T>
void goSignal3DBase<T>::applyBorderFlags ()
{
    static const int axesEnums[3] = {GO_X,GO_Y,GO_Z};
    goIndex_t i;
    for (i = 0; i < 3; ++i)
    {
        switch (this->myBorderFlags[i])
        {
            case GO_PERIODIC_BORDER: this->periodicBorders(axesEnums[i]); break;
            case GO_CONSTANT_BORDER: this->constantBorders(axesEnums[i]); break;
            case GO_PARENT_BORDER: goLog::error ("GO_PARENT_BORDER not yet implemented.", this); break; // No need for action.
            default: goLog::warning("applyBorderFlags(): unknown borderFlag value.",this); break;
        }
    }
}

/*!
 * \brief Sets data type for <void> type signals.
 *
 * This method works only for signals of type void.
 * The destroy() method is called in the course of this method,
 * so all data in the signal, if any, will be lost afterwards.
 * 
 * \param t  Type enumerator
 *
 * \return  True if successful, false otherwise.
 */
template<> bool
goSignal3DBase<void>::setDataType (goTypeEnum t)
{
    if (t == myDataType.getID())
    {
        return true;
    }
    this->destroy ();
    return myDataType.setID (t);
}

template<class T>
inline const goPtrdiff_t* 
goSignal3DBase<T>::getXDiff () const
{
    return xDiff;
}

template<class T>
inline const goPtrdiff_t* 
goSignal3DBase<T>::getYDiff () const
{
    return yDiff;
}

template<class T>
inline const goPtrdiff_t* 
goSignal3DBase<T>::getZDiff () const
{
    return zDiff;
}

template<class T>
inline goPtrdiff_t* 
goSignal3DBase<T>::getXDiff () 
{
    return xDiff;
}

template<class T>
inline goPtrdiff_t* 
goSignal3DBase<T>::getYDiff () 
{
    return yDiff;
}

template<class T>
inline goPtrdiff_t* 
goSignal3DBase<T>::getZDiff () 
{
    return zDiff;
}

template<class T>
inline const goPtrdiff_t*
goSignal3DBase<T>::getXJump () const
{
    return myXJump;
}

template<class T>
inline const goPtrdiff_t*
goSignal3DBase<T>::getYJump () const
{
    return myYJump;
}

template<class T>
inline const goPtrdiff_t*
goSignal3DBase<T>::getZJump () const
{
    return myZJump;
}

template<class T>
inline goPtrdiff_t*
goSignal3DBase<T>::getXJump () 
{
    return myXJump;
}

template<class T>
inline goPtrdiff_t*
goSignal3DBase<T>::getYJump () 
{
    return myYJump;
}

template<class T>
inline goPtrdiff_t*
goSignal3DBase<T>::getZJump () 
{
    return myZJump;
}

#if 0
template<class T>
inline
void
goSignal3DBase<T>::shiftLeftDiffX (int n)
{
   if (n <= 0)
       return;

   if (getSizeX() > 1)
   {
       
   }
   shiftLeftDiffX (n-1);
}
#endif

template< class T >
inline
void
goSignal3DBase<T>::rotateAxes ()
{
#if 0
    goString msg;
    msg = "Rotate axes ---\n";
    msg += "\tdiffs == "; 
    msg += (int)xDiff; msg += " ";
    msg += (int)yDiff; msg += " ";
    msg += (int)zDiff;
    msg += "\n\tjumps == ";
    msg += (int)myXJump; msg += " ";
    msg += (int)myYJump; msg += " ";
    msg += (int)myZJump;
    msg += "\n\trealdiffs == ";
    msg += (int)real_xDiff; msg += " ";
    msg += (int)real_yDiff; msg += " ";
    msg += (int)real_zDiff;
    msg += "\n\trealjumps == ";
    msg += (int)real_myXJump; msg += " ";
    msg += (int)real_myYJump; msg += " ";
    msg += (int)real_myZJump;
    msg += "\n\tsize == ";
    msg += (int)mySize.x; msg += " ";
    msg += (int)mySize.y; msg += " ";
    msg += (int)mySize.z; 
    msg += "\n\tbordersize == ";
    msg += (int)myBorderSize.x; msg += " ";
    msg += (int)myBorderSize.y; msg += " ";
    msg += (int)myBorderSize.z; 
    msg += "\n\tblocksize == ";
    msg += (int)myBlockSize.x; msg += " ";
    msg += (int)myBlockSize.y; msg += " ";
    msg += (int)myBlockSize.z; 
    msg += "\n\tblocks == ";
    msg += (int)myBlocks.x; msg += " ";
    msg += (int)myBlocks.y; msg += " ";
    msg += (int)myBlocks.z; 
    
    goLog::message(msg,this);
   #endif
 
  goPtrdiff_t* tempDiff = zDiff;
  goSize_t tempSize = mySize.z;
  goPtrdiff_t* tempJump = myZJump;
  
  mySize.z = mySize.y;
  mySize.y = mySize.x;
  mySize.x = tempSize;
  zDiff = yDiff;
  yDiff = xDiff;
  xDiff = tempDiff;
  myZJump = myYJump;
  myYJump = myXJump;
  myXJump = tempJump;

  tempDiff = real_zDiff;
  real_zDiff = real_yDiff;
  real_yDiff = real_xDiff;
  real_xDiff = tempDiff;
  
  tempJump = real_myZJump;
  real_myZJump = real_myYJump;
  real_myYJump = real_myXJump;
  real_myXJump = tempJump;
  
  tempSize = myBorderSize.z;
  myBorderSize.z = myBorderSize.y;
  myBorderSize.y = myBorderSize.x;
  myBorderSize.x = tempSize;
  
  tempSize = myBlockSize.z;
  myBlockSize.z = myBlockSize.y;
  myBlockSize.y = myBlockSize.x;
  myBlockSize.x = tempSize;

  tempSize = myBlocks.z;
  myBlocks.z = myBlocks.y;
  myBlocks.y = myBlocks.x;
  myBlocks.x = tempSize;
}

template <class T>
void
goSignal3DBase<T>::swapXY()
{
    goPtrdiff_t* tempDiff = xDiff;
    goPtrdiff_t* tempJump = myXJump;
    goSize_t     tempSize = mySize.x;

    xDiff         = yDiff;
    yDiff         = tempDiff;
    tempDiff      = real_xDiff;
    real_xDiff    = real_yDiff;
    real_yDiff    = tempDiff;
    myXJump       = myYJump;
    myYJump       = tempJump;
    tempJump      = real_myXJump;
    real_myXJump  = real_myYJump;
    real_myYJump  = tempJump;
    mySize.x      = mySize.y;
    mySize.y      = tempSize;
    tempSize      = myBlockSize.x;
    myBlockSize.x = myBlockSize.y;
    myBlockSize.y = tempSize;
    tempSize      = myBlocks.x;
    myBlocks.x    = myBlocks.y;
    myBlocks.y    = tempSize;

    tempSize = myBorderSize.x;
    myBorderSize.x = myBorderSize.y;
    myBorderSize.y = tempSize;
}

template <class T>
static void flip_array (T* array, goSize_t sz)
{
    goSize_t i = 0;
    goSize_t j = sz - 1;
    T* a = array;
    T* e = &array[sz-1];
    while (i < j)
    {
        T temp = *a;
        *a = *e;
        *e = temp;
        ++a; ++i;
        --e; --j;
    }
}

template <class T>
static void flip_diff_array (T* array, goSize_t sz)
{
    goSize_t i = 0;
    goSize_t j = sz - 1;
    T* a = array;
    T* e = &array[sz-1];
    T temp = array[j];
    while (j > 0)
    {
        array[j] = -array[j-1];
        --j;
    }
    array[0] = -temp;
}

/** 
 * @brief BROKEN. Fix when there's time.
 *
 * @todo BROKEN. Fix when there's time.
 * @bug BROKEN. Fix when there's time.
 *
 * @param axis Axis to flip.
 */
template <class T>
void
goSignal3DBase<T>::flip (int axis)
{
    switch (axis)
    {
        case GO_X: 
            {
                goSize_t sz = this->getSizeX() + 2 * this->getBorderX();
                flip_array (this->real_myXJump, sz);
                flip_array (this->real_xDiff, sz);
                goPtrdiff_t* a = this->real_xDiff;
                for (goSize_t i = 0; i < sz; ++i, ++a)
                {
                    *a *= static_cast<goPtrdiff_t>(-1);
                }
                this->ptr = this->getPtr(0,0,0);
                this->real_ptr = this->ptr;
            }
            break;
        case GO_Y: 
            {
                goSize_t sz = this->getSizeY();
                flip_array (this->myYJump, sz);
                flip_diff_array (this->yDiff, sz);
                this->applyBorderFlags (GO_Y);
                //goPtrdiff_t* a = this->real_myYJump;
                //for (goSize_t i = 0; i < sz; ++i, ++a)
                //{
                //    *a = *a * static_cast<goPtrdiff_t>(-1) ;
                //}
                // this->ptr = this->getPtr(0,0,0);
                // this->real_ptr = this->ptr;
            }
            break;
        case GO_Z: 
            {
                goSize_t sz = this->getSizeZ() + 2 * this->getBorderZ();
                flip_array (this->real_myZJump, sz);
                flip_array (this->real_zDiff, sz);
                goPtrdiff_t* a = this->real_zDiff;
                for (goSize_t i = 0; i < sz; ++i, ++a)
                {
                    *a *= static_cast<goPtrdiff_t>(-1);
                }
                this->ptr = this->getPtr(0,0,0);
                this->real_ptr = this->ptr;
            }
            break;
        default: return; break;
    }
}

template <class T>
const goType& 
goSignal3DBase<T>::getDataType () const
{
    return myDataType;
}
        
template<class T>
inline
T*
goSignal3DBase<T>::getPtr (goIndex_t x, goIndex_t y, goIndex_t z)
{
//    goString msg = "myChannel: ";
//    msg += (int)myChannel;
//    msg += ", offset: "; 
//    msg += (int)myChannelOffset[myChannel];
//    goLog::message(msg,this);
//    printf ("%s\n",msg.toCharPtr());
    return ptr + myZJump[z] + myYJump[y] + myXJump[x] + myChannelOffset[myChannel];
}

template<class T>
inline
const T*
goSignal3DBase<T>::getPtr (goIndex_t x, goIndex_t y, goIndex_t z) const
{
    return (const T*) ((goUInt8*)ptr + myZJump[z] + myYJump[y] + myXJump[x] + myChannelOffset[myChannel]);
}

template<class T>
inline
const T*
goSignal3DBase<T>::getClosest (go3Vector<goFloat>& point) const
{
    return (getPtr ((int)point.x, (int)point.y, (int)point.z));
}


template<> inline
goFloat
goSignal3DBase<goComplex<goFloat> >::sample (go3Vector<goFloat>& point)
{
    return 0.0f;
}

template<class T>
inline
goFloat
goSignal3DBase<T>::sample (go3Vector<goFloat>& point)
{
    int left = (int)point.x;
    goFloat px = point.x - left;
    int top  = (int)point.y;
    goFloat py = point.y - top;
    int front = (int)point.z;
    goFloat pz = point.z - front;

#if 0
	if ( (left < -1) || (left > getSizeX() - 1) || 
		 (top < -1) || (top > getSizeY() - 1) ||
		 (front < -1) || (front > getSizeZ() - 1) )
	{
		cout << "################### \n";
		cout << "\tleft = " << left << ", top = " << top << ", front = " << front << endl;
		return 0;
	}
#endif

    T* p = getPtr (left,top,front);
    T A = *p;
    T B = *(p + xDiff[left]);
    T C = *(p + yDiff[top]); // *getPtr (left,top + 1,front));
    T D = *(p + xDiff[left] + yDiff[top]); // *getPtr (left + 1,top + 1,front));

    p += zDiff[front];
    T E = *p;
    T F = *(p + xDiff[left]);
    T G = *(p + yDiff[top]);
    T H = *(p + xDiff[left] + yDiff[top]);

    goFloat I1;
    SIGNAL3D_bilinear (A,B,C,D,px,py,I1);
    
    goFloat I2;
    SIGNAL3D_bilinear (E,F,G,H,px,py,I2);
    
    return (I1 + (I2 - I1) * pz);
}

template <class T>
T*
goSignal3DBase<T>::getPtr ()
{
    return getPtr (0, 0, 0);
}

template <class T>
const T*
goSignal3DBase<T>::getPtr () const
{
    return getPtr (0, 0, 0);
}

template <class T>
T*
goSignal3DBase<T>::getRealPtr ()
{
    return real_ptr;
}

template <class T>
const T*
goSignal3DBase<T>::getRealPtr () const
{
    return real_ptr;
}

/**
 * @brief INTERNAL -- DO NOT USE
 *
 * @return 
 **/
template <class T>
void
goSignal3DBase<T>::setSize (goSize_t x, goSize_t y, goSize_t z, goSize_t channelCount)
{
    mySize.x = x;
    mySize.y = y;
    mySize.z = z;
    myChannelCount = channelCount;
}

/**
 * @brief INTERNAL -- DO NOT USE
 *
 * @return 
 **/
template <class T>
void
goSignal3DBase<T>::setSize (const goSize3D& sz)
{
    mySize = sz;
}

/**
 * @brief INTERNAL -- DO NOT USE
 *
 * @return 
 **/
template <class T>
void
goSignal3DBase<T>::setSizeX (goSize_t s)
{
    mySize.x = s;
}

/**
 * @brief INTERNAL -- DO NOT USE
 *
 * @return 
 **/
template <class T>
void
goSignal3DBase<T>::setSizeY (goSize_t s)
{
    mySize.y = s;
}

/**
 * @brief INTERNAL -- DO NOT USE
 *
 * @return 
 **/
template <class T>
void
goSignal3DBase<T>::setSizeZ (goSize_t s)
{
    mySize.z = s;
}

/** 
 * @brief Resize the border and re-initialise internal data structures
 * accordingly.
 * 
 * Newly set border flags (setBorderFlags()) will be taken into account automatically.
 *
 * @param size New border size.
 */
template <class T>
void goSignal3DBase<T>::resizeBorder (const goSize3D& size)
{
    if (this->real_ptr)
    {
        goSize3D newBorder = size;
        newBorder.x = goMath::min<goSize_t> (newBorder.x, this->getSizeX());
        newBorder.y = goMath::min<goSize_t> (newBorder.y, this->getSizeY());
        newBorder.z = goMath::min<goSize_t> (newBorder.z, this->getSizeZ());
        this->initialize (this->real_ptr, this->getSizeX(), this->getSizeY(), this->getSizeZ(),
                this->getBlockSizeX(), this->getBlockSizeY(), this->getBlockSizeZ(), 
                newBorder.x, newBorder.y, newBorder.z, this->getChannelCount());
    }
}

/**
 * @brief INTERNAL -- DO NOT USE
 *
 * @return 
 **/
template <class T>
void
goSignal3DBase<T>::setBorder (goSize_t x, goSize_t y, goSize_t z)
{
    myBorderSize.x = x;
    myBorderSize.y = y;
    myBorderSize.z = z;
}

template <class T>
goSize_t
goSignal3DBase<T>::getSizeX () const
{
    return mySize.x;
}

template <class T>
goSize_t
goSignal3DBase<T>::getSizeY () const
{
    return mySize.y;
}

template <class T>
goSize_t
goSignal3DBase<T>::getSizeZ () const
{
    return mySize.z;
}

template <class T>
goIndex_t
goSignal3DBase<T>::getBorderX () const
{
    return (goIndex_t)myBorderSize.x;
}

template <class T>
goIndex_t
goSignal3DBase<T>::getBorderY () const
{
    return (goIndex_t)myBorderSize.y;
}

template <class T>
goIndex_t
goSignal3DBase<T>::getBorderZ () const
{
    return (goIndex_t)myBorderSize.z;
}

template <class T>
goSize_t
goSignal3DBase<T>::getBlockSizeX () const 
{
    return myBlockSize.x;
}

template <class T>
goSize_t
goSignal3DBase<T>::getBlockSizeY () const 
{
    return myBlockSize.y;
}

template <class T>
goSize_t
goSignal3DBase<T>::getBlockSizeZ () const 
{
    return myBlockSize.z;
}

template <class T>
goSize_t
goSignal3DBase<T>::getChannelCount () const
{
    return myChannelCount;
}

template<>
goDouble goSignal3DBase<void>::sum () const
{
    goDouble ret = 0.0;
    switch (this->getDataType().getID())
    {
        case GO_INT8: GO_SIGNAL3D_EACHELEMENT_GENERIC_CONST (ret += *(goInt8*)__ptr, (*this)); break;
        case GO_INT16: GO_SIGNAL3D_EACHELEMENT_GENERIC_CONST (ret += *(goInt16*)__ptr, (*this)); break;
        case GO_INT32: GO_SIGNAL3D_EACHELEMENT_GENERIC_CONST (ret += *(goInt32*)__ptr, (*this)); break;
        case GO_UINT8: GO_SIGNAL3D_EACHELEMENT_GENERIC_CONST (ret += *(goUInt8*)__ptr, (*this)); break;
        case GO_UINT16: GO_SIGNAL3D_EACHELEMENT_GENERIC_CONST (ret += *(goUInt16*)__ptr, (*this)); break;
        case GO_UINT32: GO_SIGNAL3D_EACHELEMENT_GENERIC_CONST (ret += *(goUInt32*)__ptr, (*this)); break;
        case GO_FLOAT: GO_SIGNAL3D_EACHELEMENT_GENERIC_CONST (ret += *(goFloat*)__ptr, (*this)); break;
        case GO_DOUBLE: GO_SIGNAL3D_EACHELEMENT_GENERIC_CONST (ret += *(goDouble*)__ptr, (*this)); break;
        default: return 0.0;
    }
    return ret;
}

template <class T>
goDouble goSignal3DBase<T>::sum () const
{
    goLog::warning ("sum() not implemented for this instantiation.", this);
    return 0.0;
}

/// \todo This will be a problem with const objects. Find out how that can be solved without const_cast<>
template <class T>
void
goSignal3DBase<T>::setChannel (goSize_t c) 
{
    myChannel = c;
}

#ifndef GOSIGNAL3DGENERICITERATOR_H
# include <gosignal3dgenericiterator.h>
#endif

// === Please excuse the macros. They make the operator+=-like operators for void signals.

#define MAKE_SIGNAL_SIGNAL_OPERATOR(OPERATOR,OPERATORNAME) \
template <class T, class T2> \
static inline void _signalOperator##OPERATORNAME##__ (goSignal3DBase<void>& sig, const goSignal3DBase<void>& other) \
{ \
    if (sig.getChannelCount() == 1) \
    { \
        goSignal3DGenericIterator      it (&sig); \
        goSignal3DGenericConstIterator ot (&other); \
        \
        while (!it.endZ() && !ot.endZ()) \
        { \
            it.resetY(); \
            ot.resetY(); \
            while (!it.endY() && !ot.endY()) \
            { \
                it.resetX(); \
                ot.resetX(); \
                while (!it.endX() && !ot.endX()) \
                { \
                    *(T*)*it OPERATOR (T)*(T2*)*ot; \
                    it.incrementX(); \
                    ot.incrementX(); \
                } \
                it.incrementY(); \
                ot.incrementY(); \
            } \
            it.incrementZ(); \
            ot.incrementZ(); \
        } \
    } \
    else \
    { \
        goSize_t chan_backup_1 = sig.getChannel(); \
        goSize_t chan_2 = other.getChannel(); \
        goSize_t chan_count = goMath::min (sig.getChannelCount(), other.getChannelCount()); \
        goSize_t i = 0; \
        goSignal3DGenericIterator      it (&sig); \
        goSignal3DGenericConstIterator ot (&other); \
        \
        while (!it.endZ() && !ot.endZ()) \
        { \
            it.resetY(); \
            ot.resetY(); \
            while (!it.endY() && !ot.endY()) \
            { \
                it.resetX(); \
                ot.resetX(); \
                while (!it.endX() && !ot.endX()) \
                { \
                    for (i = 0; i < chan_count; ++i) \
                    { \
                        *((T*)*it + i) OPERATOR (T)*((T2*)*ot + i - chan_2); \
                    } \
                    it.incrementX(); \
                    ot.incrementX(); \
                } \
                it.incrementY(); \
                ot.incrementY(); \
            } \
            it.incrementZ(); \
            ot.incrementZ(); \
        } \
        sig.setChannel (chan_backup_1); \
    } \
} \
template <class T>  \
static inline void _signalOperator##OPERATORNAME##_ (goSignal3DBase<void>& sig, const goSignal3DBase<void>& other) \
{ \
    switch (other.getDataType().getID()) \
    { \
        case   GO_INT8:     _signalOperator##OPERATORNAME##__<T,goInt8>     (sig,   other);   break; \
        case   GO_UINT8:    _signalOperator##OPERATORNAME##__<T,goUInt8>    (sig,   other);   break; \
        case   GO_INT16:    _signalOperator##OPERATORNAME##__<T,goInt16>    (sig,   other);   break; \
        case   GO_UINT16:   _signalOperator##OPERATORNAME##__<T,goUInt16>   (sig,   other);   break; \
        case   GO_INT32:    _signalOperator##OPERATORNAME##__<T,goInt32>    (sig,   other);   break; \
        case   GO_UINT32:   _signalOperator##OPERATORNAME##__<T,goUInt32>   (sig,   other);   break; \
        case   GO_FLOAT:    _signalOperator##OPERATORNAME##__<T,goFloat>    (sig,   other);   break; \
        case   GO_DOUBLE:   _signalOperator##OPERATORNAME##__<T,goDouble>   (sig,   other);   break; \
        default: goLog::warning("goSignal3DBase<void> operator+=: unknown type."); break; \
    } \
} \
template<> goSignal3DBase<void>& \
goSignal3DBase<void>::operator OPERATOR (const goSignal3DBase<void>& other) \
{ \
    switch (this->getDataType().getID()) \
    { \
        case   GO_INT8:     _signalOperator##OPERATORNAME##_<goInt8>     (*this,other);   break; \
        case   GO_UINT8:    _signalOperator##OPERATORNAME##_<goUInt8>    (*this,other);   break; \
        case   GO_INT16:    _signalOperator##OPERATORNAME##_<goInt16>    (*this,other);   break; \
        case   GO_UINT16:   _signalOperator##OPERATORNAME##_<goUInt16>   (*this,other);   break; \
        case   GO_INT32:    _signalOperator##OPERATORNAME##_<goInt32>    (*this,other);   break; \
        case   GO_UINT32:   _signalOperator##OPERATORNAME##_<goUInt32>   (*this,other);   break; \
        case   GO_FLOAT:    _signalOperator##OPERATORNAME##_<goFloat>    (*this,other);   break; \
        case   GO_DOUBLE:   _signalOperator##OPERATORNAME##_<goDouble>   (*this,other);   break; \
        default: goLog::warning("operator #OPERATOR: unknown type."); break; \
    } \
    return *this; \
}

#define MAKE_SIGNAL_SCALAR_OPERATOR(OPERATOR,OPERATORNAME,SCALAR) \
template <class T> \
static inline void _signalScalarOperator##OPERATORNAME##_ (goSignal3DBase<void>& sig, SCALAR scalar) \
{ \
    if (sig.getChannelCount() == 1)\
    {\
        goSignal3DGenericIterator it (&sig); \
            \
            while (!it.endZ()) \
            { \
                it.resetY(); \
                    while (!it.endY()) \
                    { \
                        it.resetX(); \
                            while (!it.endX()) \
                            { \
                                *(T*)*it OPERATOR (T)scalar; \
                                    it.incrementX(); \
                            } \
                        it.incrementY(); \
                    } \
                it.incrementZ(); \
            } \
    }\
    else\
    {\
        goSignal3DGenericIterator it (&sig); \
        goIndex_t i = 0;\
        goIndex_t chanCount = sig.getChannelCount();\
            \
            while (!it.endZ()) \
            { \
                it.resetY(); \
                    while (!it.endY()) \
                    { \
                        it.resetX(); \
                            while (!it.endX()) \
                            { \
                                for (i = 0; i < chanCount; ++i)\
                                {\
                                    *(((T*)*it) + i) OPERATOR (T)scalar; \
                                }\
                                it.incrementX(); \
                            } \
                        it.incrementY(); \
                    } \
                it.incrementZ(); \
            } \
    }\
} \
template<> goSignal3DBase<void>& \
goSignal3DBase<void>::operator OPERATOR (SCALAR scalar) \
{ \
    switch (this->getDataType().getID()) \
    { \
        case   GO_INT8:     _signalScalarOperator##OPERATORNAME##_<goInt8>     (*this,scalar);   break; \
        case   GO_UINT8:    _signalScalarOperator##OPERATORNAME##_<goUInt8>    (*this,scalar);   break; \
        case   GO_INT16:    _signalScalarOperator##OPERATORNAME##_<goInt16>    (*this,scalar);   break; \
        case   GO_UINT16:   _signalScalarOperator##OPERATORNAME##_<goUInt16>   (*this,scalar);   break; \
        case   GO_INT32:    _signalScalarOperator##OPERATORNAME##_<goInt32>    (*this,scalar);   break; \
        case   GO_UINT32:   _signalScalarOperator##OPERATORNAME##_<goUInt32>   (*this,scalar);   break; \
        case   GO_FLOAT:    _signalScalarOperator##OPERATORNAME##_<goFloat>    (*this,scalar);   break; \
        case   GO_DOUBLE:   _signalScalarOperator##OPERATORNAME##_<goDouble>   (*this,scalar);   break; \
        default: goLog::warning("operator #OPERATOR: unknown type."); break; \
    } \
    return *this; \
}

MAKE_SIGNAL_SIGNAL_OPERATOR(+=,PlusEqual);
MAKE_SIGNAL_SIGNAL_OPERATOR(-=,MinusEqual);
MAKE_SIGNAL_SIGNAL_OPERATOR(*=,TimesEqual);
MAKE_SIGNAL_SIGNAL_OPERATOR(/=,DivideEqual);
MAKE_SIGNAL_SCALAR_OPERATOR(+=,PlusEqual,goFloat);
MAKE_SIGNAL_SCALAR_OPERATOR(-=,MinusEqual,goFloat);
MAKE_SIGNAL_SCALAR_OPERATOR(*=,TimesEqual,goFloat);
MAKE_SIGNAL_SCALAR_OPERATOR(/=,DivideEqual,goFloat);

#undef MAKE_SIGNAL_SIGNAL_OPERATOR
#undef MAKE_SIGNAL_SCALAR_OPERATOR

template <class T>
goSignal3DBase<T>& goSignal3DBase<T>::operator += (const goSignal3DBase<T>& other)
{
    goLog::warning ("operator+= not implemented for this type. Use goSignal3DBase<void> instead!",this);
    return *this;
}
template <class T>
goSignal3DBase<T>& goSignal3DBase<T>::operator -= (const goSignal3DBase<T>& other)
{
    goLog::warning ("operator-= not implemented for this type. Use goSignal3DBase<void> instead!",this);
    return *this;
}
template <class T>
goSignal3DBase<T>& goSignal3DBase<T>::operator *= (const goSignal3DBase<T>& other)
{
    goLog::warning ("operator*= not implemented for this type. Use goSignal3DBase<void> instead!",this);
    return *this;
}
template <class T>
goSignal3DBase<T>& goSignal3DBase<T>::operator /= (const goSignal3DBase<T>& other)
{
    goLog::warning ("operator/= not implemented for this type. Use goSignal3DBase<void> instead!",this);
    return *this;
}
template<> goSignal3DBase<void*>& goSignal3DBase<void*>::operator += (goFloat scalar)
{
    goLog::warning ("operator [+-*/]= not implemented for void*.",this);
    return *this;
}
template<> goSignal3DBase<void*>& goSignal3DBase<void*>::operator -= (goFloat scalar)
{
    goLog::warning ("operator [+-*/]= not implemented for void*.",this);
    return *this;
}
template<> goSignal3DBase<void*>& goSignal3DBase<void*>::operator *= (goFloat scalar)
{
    goLog::warning ("operator [+-*/]= not implemented for void*.",this);
    return *this;
}
template<> goSignal3DBase<void*>& goSignal3DBase<void*>::operator /= (goFloat scalar)
{
    goLog::warning ("operator [+-*/]= not implemented for void*.",this);
    return *this;
}

/** 
 * @brief Add a scalar to each element in the signal.
 * 
 * Adds a scalar to each element in the signal.
 * Implemented for all types, but please use void. The others may be discontinued in the future.
 *
 * @note For void type signals (use them!), the operation is carried out for all channels, 
 * since this seemed more natural to me,
 * especially for *= operations. All ?= operators behave the same in this respect.
 * Operators which accept another goSignal3D[Base] do NOT behave like this.
 * That may change in the future. Also see the helper functions (namely, goMath::vectorMult()) in gomath.h.
 *
 * @param scalar  Scalar.
 * 
 * @return *this
 */
template <class T>
goSignal3DBase<T>& goSignal3DBase<T>::operator += (goFloat scalar)
{
    GO_SIGNAL3D_EACHELEMENT (*__ptr = (T)(*__ptr + scalar), (*this), T);
    return *this;
}
/** 
 * @brief -= Operator.
 *
 * @see operator+=
 * 
 * @param scalar 
 * 
 * @return *this 
 */
template <class T>
goSignal3DBase<T>& goSignal3DBase<T>::operator -= (goFloat scalar)
{
    GO_SIGNAL3D_EACHELEMENT (*__ptr = (T)(*__ptr - scalar), (*this), T);
    return *this;
}
/** 
 * @brief *= Operator.
 *
 * @see operator+=
 * 
 * @param scalar 
 * 
 * @return *this 
 */
template <class T>
goSignal3DBase<T>& goSignal3DBase<T>::operator *= (goFloat scalar)
{
    GO_SIGNAL3D_EACHELEMENT (*__ptr = (T)(*__ptr * scalar), (*this), T);
    return *this;
}
/** 
 * @brief /= Operator.
 *
 * @see operator+=
 * 
 * @param scalar 
 * 
 * @return *this 
 */
template <class T>
goSignal3DBase<T>& goSignal3DBase<T>::operator /= (goFloat scalar)
{
    GO_SIGNAL3D_EACHELEMENT (*__ptr = (T)(*__ptr / scalar), (*this), T);
    return *this;
}

#if 0
template<class T>
    void
goSignal3DBase<T>::interpolateBorders()
{
    goSubSignal3D<T> source;
    goSubSignal3D<T> target;
    source.setParent (this);
    source.setDiff (getXDiff(), getYDiff(), getZDiff());
    target.setParent (this);
    target.setDiff (getXDiff(), getYDiff(), getZDiff());

    source.setSize (getSizeX(), getSizeY(), 1);
    target.setSize (getSizeX(), getSizeY(), 1);
    source.setPosition (0, 0, 0);
    target.setPosition (0, 0, -1);
    GO_SIGNAL3D_EACHELEMENT_2((*__ptr_target = *__ptr), source, target, T, T);
    source.setPosition (0, 0, getSizeZ() - 1);
    target.setPosition (0, 0, getSizeZ());
    GO_SIGNAL3D_EACHELEMENT_2((*__ptr_target = *__ptr), source, target, T, T);

    source.setSize (getSizeX(), 1, getSizeZ() + 2);
    target.setSize (getSizeX(), 1, getSizeZ() + 2);
    source.setPosition (0, 0, -1);
    target.setPosition (0, -1, -1);
    GO_SIGNAL3D_EACHELEMENT_2((*__ptr_target = *__ptr), source, target, T, T);
    source.setPosition (0, getSizeY() - 1, -1);
    target.setPosition (0, getSizeY(), -1);
    GO_SIGNAL3D_EACHELEMENT_2((*__ptr_target = *__ptr), source, target, T, T);

    source.setSize (1, getSizeY() + 2, getSizeZ() + 2);
    target.setSize (1, getSizeY() + 2, getSizeZ() + 2);
    source.setPosition (0, -1, -1);
    target.setPosition (-1, -1, -1);
    GO_SIGNAL3D_EACHELEMENT_2((*__ptr_target = *__ptr), source, target, T, T);
    source.setPosition (getSizeX() - 1, -1, -1);
    target.setPosition (getSizeX(), -1, -1);
    GO_SIGNAL3D_EACHELEMENT_2((*__ptr_target = *__ptr), source, target, T, T);
}		
#endif

#if 0
template <class T>
    void
goSignal3DBase<T>::interpolateFromSignal (goSignal3DBase<T>& other, Neighbour n)
    /*
     * Currently, only 1 voxel wide areas are copied from the "other" signal. If you
     * want to use this e.g. for cyclic filtering with filter width > 2, you have to
     * extend this method accordingly. It shouldn't be hard, but it has to be done.
     */
{
    bool interpolate = false;
    if ( (other.getSizeX() != getSizeX()) ||
            (other.getSizeY() != getSizeY()) ||
            (other.getSizeZ() != getSizeZ()) )
    {
#if _GODEBUG >= 3
        cout << "signal3D: interpolateFromSignal: uneven sizes of signals!" << endl;
#endif
        interpolate = true;
    }
    goSubSignal3D<T> subSource;
    goSubSignal3D<T> subTarget;
    subSource.setParent (&other);
    subTarget.setParent (this);
    subSource.setDiff (other.getXDiff(), other.getYDiff(), other.getZDiff());
    subTarget.setDiff (this->getXDiff(), this->getYDiff(), this->getZDiff());
    switch (n)
    {
        case LEFT: 	subSource.setSize (1, other.getSizeY(), other.getSizeZ());
                    subSource.setPosition ((int)other.getSizeX() - 1, 0, 0);
                    subTarget.setSize (1, this->getSizeY(), this->getSizeZ());
                    subTarget.setPosition (-1, 0, 0);
                    subSource.rotateAxes(); subSource.rotateAxes();
                    subTarget.rotateAxes(); subTarget.rotateAxes();
                    break;
        case RIGHT:	subSource.setSize (1, other.getSizeY(), other.getSizeZ());
                    subSource.setPosition (0, 0, 0);
                    subTarget.setSize (1, this->getSizeY(), this->getSizeZ());
                    subTarget.setPosition (this->getSizeX(), 0, 0);
                    subSource.rotateAxes(); subSource.rotateAxes();
                    subTarget.rotateAxes(); subTarget.rotateAxes();
                    break;
        case TOP:	subSource.setSize (other.getSizeX(), 1, other.getSizeZ());
                    subSource.setPosition (0, getSizeY() - 1, 0);
                    subTarget.setSize (this->getSizeX(), 1, this->getSizeZ());
                    subTarget.setPosition (0, -1, 0);
                    subSource.rotateAxes();
                    subTarget.rotateAxes();
                    break;
        case BOTTOM:	subSource.setSize (other.getSizeX(), 1, other.getSizeZ());
                        subSource.setPosition (0, 0, 0);
                        subTarget.setSize (this->getSizeX(), 1, this->getSizeZ());
                        subTarget.setPosition (0, this->getSizeY(), 0);
                        subSource.rotateAxes();
                        subTarget.rotateAxes();
                        break;
        case FRONT:	subSource.setSize (other.getSizeX(), other.getSizeY(), 1);
                    subSource.setPosition (0, 0, getSizeZ() - 1);
                    subTarget.setSize (this->getSizeX(), this->getSizeY(), 1);
                    subTarget.setPosition (0, 0, -1);
                    break;
        case BACK:	subSource.setSize (other.getSizeX(), other.getSizeY(), 1);
                    subSource.setPosition (0, 0, 0);
                    subTarget.setSize (this->getSizeX(), this->getSizeY(), 1);
                    subTarget.setPosition (0, 0, this->getSizeZ());
                    break;
        case EDGE1:
                    subSource.setSize (other.getSizeX(), 1, 1);
                    subSource.setPosition (0, other.getSizeY() - 1, other.getSizeZ() - 1);
                    subTarget.setSize (this->getSizeX(), 1, 1);
                    subTarget.setPosition (0, -1, -1);
                    break;
        case EDGE2:
                    subSource.setSize (1, 1, other.getSizeZ());
                    subSource.setPosition (0, other.getSizeY() - 1, 0);
                    subTarget.setSize (1, 1, this->getSizeZ());
                    subTarget.setPosition (this->getSizeX(), -1, 0);
                    subSource.rotateAxes();
                    subTarget.rotateAxes();
                    break;	
        case EDGE3:
                    subSource.setSize (other.getSizeX(), 1, 1);
                    subSource.setPosition (0, other.getSizeY() - 1, 0);
                    subTarget.setSize (this->getSizeX(), 1, 1);
                    subTarget.setPosition (0, -1, this->getSizeZ());
                    break;	
        case EDGE4:
                    subSource.setSize (1, 1, other.getSizeZ());
                    subSource.setPosition (other.getSizeX() - 1, other.getSizeY() - 1, 0);
                    subTarget.setSize (1, 1, this->getSizeZ());
                    subTarget.setPosition (-1, -1, 0);
                    subSource.rotateAxes();
                    subTarget.rotateAxes();
                    break;	

        case EDGE5:
                    subSource.setSize (other.getSizeX(), 1, 1);
                    subSource.setPosition (0, 0, other.getSizeZ() - 1);
                    subTarget.setSize (this->getSizeX(), 1, 1);
                    subTarget.setPosition (0, this->getSizeY(), -1);
                    break;
        case EDGE6:
                    subSource.setSize (1, 1, other.getSizeZ());
                    subSource.setPosition (0, 0, 0);
                    subTarget.setSize (1, 1, this->getSizeZ());
                    subTarget.setPosition (this->getSizeX(), this->getSizeY(), 0);
                    subSource.rotateAxes();
                    subTarget.rotateAxes();
                    break;	
        case EDGE7:
                    subSource.setSize (other.getSizeX(), 1, 1);
                    subSource.setPosition (0, 0, 0);
                    subTarget.setSize (this->getSizeX(), 1, 1);
                    subTarget.setPosition (0, this->getSizeY(), this->getSizeZ());
                    break;	
        case EDGE8:
                    subSource.setSize (1, 1, other.getSizeZ());
                    subSource.setPosition (other.getSizeX() - 1, 0, 0);
                    subTarget.setSize (1, 1, this->getSizeZ());
                    subTarget.setPosition (-1, this->getSizeY(), 0);
                    subSource.rotateAxes();
                    subTarget.rotateAxes();
                    break;	

        case EDGE9:
                    subSource.setSize (1, other.getSizeY(), 1);
                    subSource.setPosition (0, 0, other.getSizeZ() - 1);
                    subTarget.setSize (1, this->getSizeY(), 1);
                    subTarget.setPosition (this->getSizeX(), 0, -1);
                    subSource.rotateAxes(); subSource.rotateAxes();
                    subTarget.rotateAxes(); subTarget.rotateAxes();
                    break;
        case EDGE10:
                    subSource.setSize (1, other.getSizeY(), 1);
                    subSource.setPosition (0, 0, 0);
                    subTarget.setSize (1, this->getSizeY(), 1);
                    subTarget.setPosition (this->getSizeX(), 0, this->getSizeZ());
                    subSource.rotateAxes(); subSource.rotateAxes();
                    subTarget.rotateAxes(); subTarget.rotateAxes();
                    break;	
        case EDGE11:
                    subSource.setSize (1, other.getSizeY(), 1);
                    subSource.setPosition (other.getSizeX() - 1, 0, 0);
                    subTarget.setSize (1, this->getSizeY(), 1);
                    subTarget.setPosition (-1, 0, this->getSizeZ());
                    subSource.rotateAxes(); subSource.rotateAxes();
                    subTarget.rotateAxes(); subTarget.rotateAxes();
                    break;	
        case EDGE12:
                    subSource.setSize (1, other.getSizeY(), 1);
                    subSource.setPosition (other.getSizeX() - 1, 0, other.getSizeZ() - 1);
                    subTarget.setSize (1, this->getSizeY(), 1);
                    subTarget.setPosition (-1, 0, -1);
                    subSource.rotateAxes(); subSource.rotateAxes();
                    subTarget.rotateAxes(); subTarget.rotateAxes();
                    break;	
        case CORNER1:
                    subSource.setSize(1,1,1);
                    subTarget.setSize(1,1,1);
                    subSource.setPosition(other.getSizeX()-1, other.getSizeY()-1, other.getSizeZ()-1);
                    subTarget.setPosition(-1,-1,-1);
                    *subTarget.getPtr(0,0,0) = *subSource.getPtr(0,0,0);
                    return;
        case CORNER2:
                    subSource.setSize(1,1,1);
                    subTarget.setSize(1,1,1);
                    subSource.setPosition(0, other.getSizeY()-1, other.getSizeZ()-1);
                    subTarget.setPosition(this->getSizeX(),-1,-1);
                    *subTarget.getPtr(0,0,0) = *subSource.getPtr(0,0,0);
                    return;
        case CORNER3:
                    subSource.setSize(1,1,1);
                    subTarget.setSize(1,1,1);
                    subSource.setPosition(0, other.getSizeY()-1, 0);
                    subTarget.setPosition(this->getSizeX(), -1, this->getSizeZ());
                    *subTarget.getPtr(0,0,0) = *subSource.getPtr(0,0,0);
                    return;
        case CORNER4:
                    subSource.setSize(1,1,1);
                    subTarget.setSize(1,1,1);
                    subSource.setPosition(other.getSizeX()-1, other.getSizeY()-1, 0);
                    subTarget.setPosition(-1,-1,this->getSizeZ());
                    *subTarget.getPtr(0,0,0) = *subSource.getPtr(0,0,0);
                    return;
        case CORNER5:
                    subSource.setSize(1,1,1);
                    subTarget.setSize(1,1,1);
                    subSource.setPosition(other.getSizeX()-1, 0, other.getSizeZ()-1);
                    subTarget.setPosition(-1,this->getSizeY(),-1);
                    *subTarget.getPtr(0,0,0) = *subSource.getPtr(0,0,0);
                    return;
        case CORNER6:
                    subSource.setSize(1,1,1);
                    subTarget.setSize(1,1,1);
                    subSource.setPosition(0, 0, other.getSizeZ()-1);
                    subTarget.setPosition(this->getSizeX(), this->getSizeY(), -1);
                    *subTarget.getPtr(0,0,0) = *subSource.getPtr(0,0,0);
                    return;
        case CORNER7:
                    subSource.setSize(1,1,1);
                    subTarget.setSize(1,1,1);
                    subSource.setPosition(0, 0, 0);
                    subTarget.setPosition(this->getSizeX(), this->getSizeY(), this->getSizeZ());
                    *subTarget.getPtr(0,0,0) = *subSource.getPtr(0,0,0);
                    return;
        case CORNER8:					
                    subSource.setSize(1,1,1);
                    subTarget.setSize(1,1,1);
                    subSource.setPosition(other.getSizeX()-1, 0, 0);
                    subTarget.setPosition(-1, this->getSizeY(), this->getSizeZ());
                    *subTarget.getPtr(0,0,0) = *subSource.getPtr(0,0,0);
                    return;
    }
    if (!interpolate)
    {
        GO_SIGNAL3D_EACHELEMENT_2((*__ptr_target = *__ptr), subSource, subTarget, T, T);
    }
    else 
    {
        if ( (subSource.getSizeZ() == 1) && (subSource.getSizeY() == 1) )
        {
            //This is no interpolation for source>target, because source doesn't get filtered
            goDouble dx = subSource.getSizeX() / (float)subTarget.getSizeX();
            goDouble px = 0;
            goDouble d;
            T A,B;
            goInt32 x;
            for (x = 0; x < (goInt32)(subTarget.getSizeX() - 1); x++)
            {
                A = *subSource.getPtr ((goInt32)px,0,0);
                B = *subSource.getPtr ((goInt32)px + 1, 0, 0);
                d = px - (goInt32)px;
                *subTarget.getPtr (x,0,0) = (T) (A + (B - A) * d);
                px += dx;
            }
            *subTarget.getPtr (x, 0, 0) = *subSource.getPtr (subSource.getSizeX() - 1, 0, 0);
        }
        goDouble dx = subSource.getSizeX() / (float)subTarget.getSizeX();
        goDouble dy = subSource.getSizeY() / (float)subTarget.getSizeY();
        go3Vector<goDouble> point;
        goSize_t x,y;
        point.z = 0;
        point.y = 0;
        for (y = 0; y < subTarget.getSizeY(); y++)
        {
            point.x = 0;
            for (x = 0; x < subTarget.getSizeX(); x++)
            {
                T a = 0,b = 0,c = 0,d = 0;
                goInt32  ix, iy;
                goDouble rx, ry;
                ix = (goInt32)point.x;
                iy = (goInt32)point.y;
                rx = point.x - ix;
                ry = point.y - iy;
                a = *subSource.getPtr (ix,iy,0);
                if (x < (subTarget.getSizeX() - 1))
                {
                    b = *subSource.getPtr (ix + 1,iy,0);
                }
                if (y < (subTarget.getSizeY() - 1))
                {
                    c = *subSource.getPtr (ix, iy + 1,0);
                    if (x < (subTarget.getSizeX() - 1))
                    {
                        d = *subSource.getPtr (ix + 1, iy + 1,0);
                    }
                }
                goDouble C;
                SIGNAL3D_bilinear (a,b,c,d,rx,ry,C);
                *subTarget.getPtr (x,y,0) = (T)C;
                point.x += dx;
            }
            point.y += dy;
        }
    }
}

#endif

#if 0
template< class T >
bool
goSignal3DBase<T>::read (ifstream &f, bool no_extra_memory) {
    if ( (!ptr) || ( (xSize * ySize * zSize) <= 0 ) ) {
        goError::print("goSignal3DBase::read()","No block memory allocated or zero size indicated. No data read.");
        return false;
    }
    if (!no_extra_memory)		// Load everything, then copy
    {
        T buffer[xSize * ySize * zSize];
        T *b_ptr;
        b_ptr = &buffer[0];
        goSize_t bufSize = xSize * ySize * zSize * sizeof(T);
        bzero((void*)b_ptr, bufSize);
        f.read ((char*)b_ptr, bufSize);
        GO_SIGNAL3D_EACHELEMENT(*__ptr = *(b_ptr++), (*this), T);
    }
    else   // Use readSlice to load every element
    {
        /* Actually useless to use nested loops, but hey, this is just for loading... */
        goSize_t k;
        for (k = 0; k < getSizeZ(); k++) 
        {
            readSlice (f, (goIndex_t)k, no_extra_memory);
        }
    }
    return true;
}

template< class T >
bool
goSignal3DBase<T>::readSlice (ifstream &f, goIndex_t slice, bool no_extra_memory) {
    if ( (!ptr) || ( (xSize * ySize * zSize) <= 0 ) ) {
        goError::print("goSignal3DBase::readSlice()","No block memory allocated or zero size indicated. No data read.");
        return false;
    }
    goSize_t i,j;
    goPtrdiff_t dx = getXDiff();
    goPtrdiff_t dy = getYDiff();
    T *p2 = ptr + (slice * getZDiff());
    T *p;
    /*
     * Use nested loops since this is also used for subsignals and more 
     * exotic signals not having exactly linear storage of the data.
     * First, load everything in the local buffer, then copy the data.
     */
    if (!no_extra_memory)
    {
        T buffer[xSize * ySize];
        goSize_t bufSize = sizeof(T) * xSize * ySize;
        bzero((void*)&buffer[0], bufSize);
        f.read ((char*)&buffer[0], bufSize);
        int ptr = 0;
        for (j = getSizeY(); j > 0; j--) 
        {
            p = p2;
            for (i = getSizeX(); i > 0; i--) {
                *p = buffer[ptr++];
                p += dx;
            }
            p2 += dy;
        }
        if (f.eof()) 
        {
            goError::print ("goSignal3DBase::readSlice()","Early EOF detected. Some data might be corrupted.");
            return false;
        }
    } 
    else  // Don't use extra memory, load everything element-by-element
    {
        for (j = getSizeY(); j > 0; j--) {
            p = p2;
            for (i = getSizeX(); i > 0; i--) {
                f.read ((char*)p, sizeof(T));
                // cout << "Read line " << j << " element " << i << ": " << *p << endl;
                // f >> *p;
                p += dx;
            }
            p2 += dy;
            if (f.eof()) {
                goError::print ("goSignal3DBase::readSlice()","Early EOF detected.");
                return false;
            }
        }
    }
    return true;
}

template< class T >
bool
goSignal3DBase<T>::write (ofstream &f, bool no_extra_memory) {
    if ( (!ptr) || ( (xSize * ySize * zSize) <= 0 ) ) {
        goError::print("goSignal3DBase::write()","No block memory allocated or zero size indicated. No data written.");
        return false;
    }
    if (!no_extra_memory)		// Load everything, then copy
    {
        T buffer[xSize * ySize * zSize];
        T *b_ptr;
        b_ptr = &buffer[0];
        GO_SIGNAL3D_EACHELEMENT(*(b_ptr++) = *__ptr, (*this), T);
        f.write ((const char*)&buffer[0], sizeof(T) * xSize * ySize * zSize);
    }
    else
    {
        goSize_t k;
        for (k = 0; k < getSizeZ(); k++) {
            writeSlice (f, (goIndex_t)k, no_extra_memory);
        }
    }
    return true;
}

template< class T >
bool
goSignal3DBase<T>::writeSlice (ofstream &f, goIndex_t slice, bool no_extra_memory) {
    if ( (!ptr) || ( (xSize * ySize * zSize) <= 0 ) ) {
        goError::print("goSignal3DBase::writeSlice()","No block memory allocated or zero size indicated. No data written.");
        return false;
    }
    goSize_t i,j;
    goPtrdiff_t dx = getXDiff();
    goPtrdiff_t dy = getYDiff();
    T *p = ptr + (slice * getZDiff());
    T *p2 = p;
    if (!no_extra_memory)
    {
        T buffer[xSize * ySize];
        int ptr = 0;
        for (j = getSizeY(); j > 0; j--) 
        {
            p = p2;
            for (i = getSizeX(); i > 0; i--) {
                buffer[ptr++] = *p;
                p += dx;
            }
            p2 += dy;
        }
        f.write ((const char*)&buffer[0], xSize * ySize * sizeof(T));
    } 
    else 
    {
        for (j = getSizeY(); j > 0; j--) {
            p = p2;
            for (i = getSizeX(); i > 0; i--) {
                // cout << "writing line " << j << " element " << i << " = " << *p << endl;
                f.write ((const char*)p, sizeof(T));
                // f << *p << " ";
                p += dx;
            }
            // f << "\n";
            p2 += dy;
        }
    }
    return true;
}

#endif

#if 0
template class goSignal3DBase< goInt8 >;
template class goSignal3DBase< goUInt8 >;
template class goSignal3DBase< goInt16 >;
template class goSignal3DBase< goUInt16 >;
template class goSignal3DBase< goInt32 >;
template class goSignal3DBase< goUInt32 >;
#ifdef HAVE_INT64
template class goSignal3DBase< goInt64 >;
#endif
template class goSignal3DBase< goFloat >;
template class goSignal3DBase< goDouble >;
template class goSignal3DBase< goComplex<goFloat> >;
#endif
template class goSignal3DBase< void* >;
template class goSignal3DBase< void >;

