#include <gosignal3d.h>
#include <gosignal3dbase.h>
#include <gosubsignal3d.h>
#include <goerror.h>
#include <gosignalmacros.h>
#include <go3vector.h>
#include <string.h> // bzero()

#include <iostream>

template< class T >
goSignal3D<T>::goSignal3D () 
    : goSignal3DBase<T> ()
{
}

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
    this->make (x,y,z, 
            blocksize_x, blocksize_y, blocksize_z,
            border_x, border_y, border_z);
}

template <class T>
goSignal3D<T>::goSignal3D (goSignal3D<T>& other)
    : goSignal3DBase<T> ()
{
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

template <class T>
const goSignal3D<T>&
goSignal3D<T>::operator= (goSignal3DBase<T>& other)
{
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

    return *this;
}

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

