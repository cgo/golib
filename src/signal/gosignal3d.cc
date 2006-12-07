#include <gosignal3d.h>
#include <gosignal3dbase.h>
//#include <gosubsignal3d.h>
#include <goerror.h>
#include <gosignalmacros.h>
#include <gosignalhelper.h>
#include <go3vector.h>
#include <golog.h>
#include <gocomplex.h>
#include <gomath.h>
#include <string.h> // bzero()

#include <iostream>

/* 
 * @class goSignal3D
 * Class to handle 3-dimensional signals.
 * Assumes a 3D signal is stored linearly in memory.
 * xDiff, yDiff, and zDiff, are the pointer differences in each 
 * direction.
 * ptr points at the start address of the signal.
 */

/*! \brief Constructor
 */
template< class T >
goSignal3D<T>::goSignal3D () 
    : goSignal3DBase<T> ()
{
    this->setClassID(GO_SIGNAL3D);
}

/*! \brief Constructor
 * 
 * @note This does not make sense for VOID type signals.
 * Use setDataType() there first.
 *
 * \param x Size in x direction
 * \param y Size in y direction
 * \param z Size in z direction
 * \param blocksize_x Block size in x direction 
 * \param blocksize_y Block size in y direction 
 * \param blocksize_z Block size in z direction 
 * \param border_x Border size in x direction
 * \param border_y Border size in y direction
 * \param border_z Border size in z direction
 */
template< class T >
goSignal3D<T>::goSignal3D (goSize_t x, 
        goSize_t y, 
        goSize_t z,
        goSize_t blocksize_x, 
        goSize_t blocksize_y, 
        goSize_t blocksize_z,
        goSize_t border_x, 
        goSize_t border_y, 
        goSize_t border_z,
        goSize_t channelCount)
  : goSignal3DBase<T> ()
{
    this->setClassID(GO_SIGNAL3D);
    this->make (x,y,z, 
            blocksize_x, blocksize_y, blocksize_z,
            border_x, border_y, border_z, channelCount);
}

/*! \brief Copy constructor
 *
 * Creates a signal of the same size as <code>other</code> and
 * copies the content.
 */
template <class T>
goSignal3D<T>::goSignal3D (goSignal3D<T>& other)
    : goSignal3DBase<T> ()
{
    this->setClassID(GO_SIGNAL3D);
    this->copy(other);
}

template<> goSignal3D<void>::~goSignal3D () 
{
    if (real_ptr)
    {
        delete[] (goUByte*)real_ptr;
        real_ptr = NULL;
    }
}

template< class T >
goSignal3D<T>::~goSignal3D () 
{
    if (this->real_ptr)
    {
        delete[] (goUByte*)this->real_ptr;
        this->real_ptr = NULL;
    }
}

template<> void
goSignal3D<void>::destroy ()
{
    if (real_ptr)
    {
        delete[] (goUInt8*)real_ptr;
        real_ptr = NULL;
        ptr      = NULL;
        goSignal3DBase<void>::destroy ();
    }
}


template<> bool
goSignal3D<void>::make (goSize_t x, goSize_t y, goSize_t z,
                        goSize_t blockSizeX, 
                        goSize_t blockSizeY, 
                        goSize_t blockSizeZ,
		                goSize_t border_x, 
                        goSize_t border_y, 
                        goSize_t border_z,
                        goSize_t channelCount) 
{
    assert (blockSizeX > 0);
    assert (blockSizeY > 0);
    assert (blockSizeZ > 0);

    this->destroy ();
    
    //= NOTE: In order for the iterator classes not to
    //=       cause illegal reads at the end of a zero-border
    //=       signal, make the border min. 1.
    border_x = goMath::max<goSize_t> (border_x, 1);
    border_y = goMath::max<goSize_t> (border_y, 1);
    border_z = goMath::max<goSize_t> (border_z, 1);

    goSize3D blocks;
  
    blocks.x = (x + blockSizeX - 1) / blockSizeX;
    blocks.y = (y + blockSizeY - 1) / blockSizeY;
    blocks.z = (z + blockSizeZ - 1) / blockSizeZ;
    
    // real_ptr = new T[(x + (border_x << 1)) * (y + (border_y << 1)) * (z + (border_z << 1))];
    // ptr = real_ptr + (goPtrdiff_t)(border_x * xDiff + border_y * yDiff + border_z * zDiff);
    goUInt8* p = new goUInt8 [myDataType.getSize() * blocks.x * blockSizeX * blocks.y * blockSizeY * blocks.z * blockSizeZ * channelCount];

    if (!p)
    {
        return false;
    }
    
    if (!initialize (p, x, y, z,
                     blockSizeX,
                     blockSizeY,
                     blockSizeZ,
                     border_x,
                     border_y,
                     border_z,
                     channelCount))
    {
        delete[] p;
        p = NULL;
        return false;
    }

    return true;
}

template<> const goSignal3D<void>&
goSignal3D<void>::copy (const goSignal3DBase<void>& other)
{
    goLog::message("goSignal3D<void>::operator=()");
    this->destroy();
    this->setDataType (other.getDataType().getID());
    this->make (other.getSizeX(),
                other.getSizeY(),
                other.getSizeZ(),
                other.getBlockSizeX(),
                other.getBlockSizeY(),
                other.getBlockSizeZ(),
                other.getBorderX(),
                other.getBorderY(),
                other.getBorderZ(),
                other.getChannelCount());

    goCopySignal (&other,this);
//    goIndex_t chan = other.getChannel();
//    goSize_t i;
//    for (i = 0; i < this->getChannelCount(); ++i)
//    {
//        this->setChannel (i);
//        const_cast<goSignal3DBase<void>&>(other).setChannel (i);
//        goCopySignalChannel (&other, this);
//    }
//    this->setChannel (0);
//    const_cast<goSignal3DBase<void>&>(other).setChannel (chan);
    return *this;
}


/*! \brief Copy operator 
 *
 * Resizes this signal to the size of <code>other</code> and
 * copies the content.
 */
template <class T>
const goSignal3D<T>&
goSignal3D<T>::copy (const goSignal3DBase<T>& other)
{
    std::cout << "goSignal3D::operator=()\n";
    this->destroy();

    this->make (other.getSizeX(),
                other.getSizeY(),
                other.getSizeZ(),
                other.getBlockSizeX(),
                other.getBlockSizeY(),
                other.getBlockSizeZ(),
                other.getBorderX(),
                other.getBorderY(),
                other.getBorderZ(),
                other.getChannelCount());
   
    goSize_t i;
    goSize_t otherChannel = other.getChannel();
    for (i = 0; i < this->getChannelCount(); ++i)
    {
        this->setChannel(i);
        const_cast<goSignal3DBase<T>&>(other).setChannel(i);
        GO_SIGNAL3D_EACHELEMENT_2 (*__ptr = *__ptr_target, (*this), const_cast<goSignal3DBase<T>&>(other), T, T);
    }
    const_cast<goSignal3DBase<T>&>(other).setChannel(otherChannel);
    return *this;
}



/*!
 * Deletes the memory used by the block data.
 * @todo Take care what happens when axes are rotated. 
 * Nothing bad should happen though.
 */
template<class T> void
goSignal3D<T>::destroy ()
{
    goSignal3DBase<T>::destroy ();
    
    if (this->real_ptr)
    {
        delete[] this->real_ptr;
        this->real_ptr = NULL;
        this->ptr      = NULL;
    }
}

template<class T>
goSize_t
goSignal3D<T>::memoryUsage() const
{
    if (this->real_ptr) 
    {
        return (goSize_t)(this->getDataType().getSize() * this->getSizeX() * this->getSizeY() * this->getSizeZ());
    }

    return 0;
}

/// Copies only the size, NOT THE DATA!
template< class T >
bool
goSignal3D<T>::make (const goSignal3DBase<T> *other) {
    return this->make (other->getSizeX(), 
                       other->getSizeY(), 
                       other->getSizeZ(),
                       other->getBlockSizeX(),
                       other->getBlockSizeY(), 
                       other->getBlockSizeZ(), 
                       other->getBorderX(), 
                       other->getBorderY(), 
                       other->getBorderZ(),
                       other->getChannelCount());
}

/*! 
 * \brief Fills this signal with <code>element</code>.
 */
template <class T>
void
goSignal3D<T>::fill (const T* element)
{
    GO_SIGNAL3D_EACHELEMENT (*__ptr = *element, (*this), T);
}

template <class T>
void
goSignal3D<T>::fill (goDouble v)
{
    goSignal3DBase<T>::fill (v);
}

template<> void
goSignal3D<void>::fill (const void* p)
{
    goSignal3DBase<void>::fill(p);
}

template<> void
goSignal3D<void>::fill (goDouble v)
{
    goSignal3DBase<void>::fill(v);
}

/** 
 * @brief Allocate memory.
 * 
 * @see make()
 * 
 * @param size Signal size 
 * @param blockSize  Block size
 * @param borderSize  Borde size
 * @param channelCount  Number of channels
 * 
 * @return True if successful, false otherwise.
 */
template <class T>
bool goSignal3D<T>:: make (const goSize3D& size,
                           const goSize3D& blockSize,
                           const goSize3D& borderSize,
                           goSize_t        channelCount)
{
    return this->make(size.x,size.y,size.z,
                      blockSize.x,blockSize.y,blockSize.z,
                      borderSize.x,borderSize.y,borderSize.z,
                      channelCount);
}

/*!
 * Allocates memory of appropriate size for the whole signal.
 * Sets diffs and size.
 * The data is uninitialized.
 * If make is called, <CODE>destroy()</CODE> should be called when
 * the data is not referenced anymore.
 * destroy() is NOT called automagically when the destructor is called.
 * To destroy the actual data, destroy() needs to be called BEFORE deleting the object.
 * @param x Size in x direction of the signal
 * @param y Size in y direction of the signal
 * @param z Size in z direction of the signal
 * @param border_x Size of the border in x direction
 * @param border_y Size of the border in y direction
 * @param border_z Size of the border in z direction
 * @param channelCount Number of channels (defaults to 1). If you want to store, say,
 *                     RGBA data, you would set this to 4.
 */
template< class T >
bool
goSignal3D<T>::make (goSize_t x, goSize_t y, goSize_t z,
                     goSize_t blockSizeX, 
                     goSize_t blockSizeY, 
                     goSize_t blockSizeZ,
		             goSize_t border_x, 
                     goSize_t border_y, 
                     goSize_t border_z,
                     goSize_t channelCount) 
{
    assert (blockSizeX > 0);
    assert (blockSizeY > 0);
    assert (blockSizeZ > 0);
    
    this->destroy ();

    //= NOTE: In order for the iterator classes not to
    //=       cause illegal reads at the end of a zero-border
    //=       signal, make the border min. 1.
    border_x = goMath::min(goMath::max<goSize_t>(border_x, 1), x);
    border_y = goMath::min(goMath::max<goSize_t>(border_y, 1), y);
    border_z = goMath::min(goMath::max<goSize_t>(border_z, 1), z);
    
    goSize3D blocks;
  
    blocks.x = (x + blockSizeX - 1) / blockSizeX;
    blocks.y = (y + blockSizeY - 1) / blockSizeY;
    blocks.z = (z + blockSizeZ - 1) / blockSizeZ;
    
    // real_ptr = new T[(x + (border_x << 1)) * (y + (border_y << 1)) * (z + (border_z << 1))];
    // ptr = real_ptr + (goPtrdiff_t)(border_x * xDiff + border_y * yDiff + border_z * zDiff);
    T* p = new T[blocks.x * blockSizeX * blocks.y * blockSizeY * blocks.z * blockSizeZ * channelCount];

    if (!p)
    {
        return false;
    }
    
    if (!initialize (p, x, y, z,
                     blockSizeX,
                     blockSizeY,
                     blockSizeZ,
                     border_x,
                     border_y,
                     border_z,
                     channelCount))
    {
        delete[] p;
        p = NULL;
        return false;
    }

    return true;
}


/// Works only with linear-memory blocks. No sub blocks. Uses memset().
template <class T>
void  
goSignal3D<T>::fillByte (goInt8 b)
{ 
    memset ((void*)this->real_ptr, (int)b, 
            this->getDataType().getSize() * this->mySize.x * this->mySize.y * this->mySize.z * this->getChannelCount()); 
}

template class goSignal3D< goInt8 >;
template class goSignal3D< goUInt8 >;
template class goSignal3D< goInt16 >;
template class goSignal3D< goUInt16 >;
template class goSignal3D< goInt32 >;
template class goSignal3D< goUInt32 >;
template class goSignal3D< goInt64 >;
template class goSignal3D< goFloat >;
template class goSignal3D< goDouble >;
template class goSignal3D< goComplex<goFloat> >;
template class goSignal3D< void* >;
template class goSignal3D< void >;
