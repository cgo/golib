#include <gosignal3d.h>
#include <gosignal3dbase.h>
#include <gosubsignal3d.h>
#include <goerror.h>
#include <gosignalmacros.h>
#include <go3vector.h>
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
    this->setClassName ("goSignal3D");
}

/*! \brief Constructor
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
        goSize_t border_z)
  : goSignal3DBase<T> ()
{
    this->setClassName ("goSignal3D");
    this->make (x,y,z, 
            blocksize_x, blocksize_y, blocksize_z,
            border_x, border_y, border_z);
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
    this->setClassName ("goSignal3D");
    *this = other;
}
    

template< class T >
goSignal3D<T>::~goSignal3D () 
{
    std::cout << "~goSignal3D\n";
    if (real_ptr)
    {
        delete[] real_ptr;
        real_ptr = NULL;
    }
}

/*! \brief Copy operator 
 *
 * Resizes this signal to the size of <code>other</code> and
 * copies the content.
 */
template <class T>
const goSignal3D<T>&
goSignal3D<T>::operator= (goSignal3DBase<T>& other)
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
                other.getBorderZ());
    
    GO_SIGNAL3D_EACHELEMENT_2 (*__ptr = *__ptr_target, (*this), other, T, T);
    
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
    
    if (real_ptr)
    {
        delete[] real_ptr;
        real_ptr = NULL;
        ptr      = NULL;
    }
}

template<class T>
goSize_t
goSignal3D<T>::memoryUsage()
{
    if (real_ptr) 
    {
        return (goSize_t)(sizeof(T) * getSizeX() * getSizeY() * getSizeZ());
    }

    return 0;
}

/// Copies only the size, NOT THE DATA!
template< class T >
bool
goSignal3D<T>::make (goSignal3D *other) {
    return this->make (other->getSizeX(), 
                       other->getSizeY(), 
                       other->getSizeZ(),
                       other->getBlockSizeX(),
                       other->getBlockSizeY(), 
                       other->getBlockSizeZ(), 
                       other->getBorderX(), 
                       other->getBorderY(), 
                       other->getBorderZ());
}

/*! \brief Fills this signal with <code>element</code>.
 */
template <class T>
void
goSignal3D<T>::fill (const T& element)
{
    GO_SIGNAL3D_EACHELEMENT (*__ptr = element, (*this), T);
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
template class goSignal3D< void* >;

