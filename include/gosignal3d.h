#ifndef __GOSIGNAL3D_H__
#define __GOSIGNAL3D_H__

#include <config.h>
#include <iostream.h>
#include <fstream.h>
#include <gotypes.h>
#include <goobjectinfo.h>
#include <go3vector.h>		// sample()
#include <gosignal3dbase.h>

#include <assert.h>
#include <string.h>

template< class T >
class
goSignal3D : public goSignal3DBase<T>
{
    public:
        goSignal3D ();
        goSignal3D (goSize_t x, goSize_t y, goSize_t z,
                    goSize_t blocksize_x = 32, goSize_t blocksize_y = 32, goSize_t blocksize_z = 32,
                    goSize_t border_x = 0, goSize_t border_y = 0, goSize_t border_z = 0);
        goSignal3D (goSignal3D<T>& other);
        virtual ~goSignal3D ();
        virtual const goSignal3D<T>& operator= (goSignal3DBase<T>& other);

        // From goObjectInfo
        virtual goSize_t memoryUsage();

        inline bool make (goSize_t x, goSize_t y, goSize_t z,
                          goSize_t blocksize_x = 32,
                          goSize_t blocksize_y = 32,
                          goSize_t blocksize_z = 32,
                          goSize_t border_x    = 0,
                          goSize_t border_y    = 0,
                          goSize_t border_z    = 0);

        bool make (goSignal3D *other);

        virtual void destroy (); 

        /// Works only with linear-memory blocks. No sub blocks. Uses memset().
        void  fillByte (goInt8 b) 
        { 
            memset ((void*)real_ptr, (int)b, 
                    sizeof(T) * mySize.x * mySize.y * mySize.z ); 
        }

        void fill (const T& element);
};

/*!
 * Allocates memory of appropriate size for the block.
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
 */
template< class T >
inline
bool
goSignal3D<T>::make (goSize_t x, goSize_t y, goSize_t z,
                     goSize_t blockSizeX, 
                     goSize_t blockSizeY, 
                     goSize_t blockSizeZ,
		             goSize_t border_x, 
                     goSize_t border_y, 
                     goSize_t border_z) 
{
    if (blockSizeX == 0 || blockSizeY == 0 || blockSizeZ == 0)
    {
        return false;
    }
    
    assert (blockSizeX > 0);
    assert (blockSizeY > 0);
    assert (blockSizeZ > 0);
    
    goSize3D blocks;
  
    blocks.x = (x + blockSizeX - 1) / blockSizeX;
    blocks.y = (y + blockSizeY - 1) / blockSizeY;
    blocks.z = (z + blockSizeZ - 1) / blockSizeZ;
    
    // real_ptr = new T[(x + (border_x << 1)) * (y + (border_y << 1)) * (z + (border_z << 1))];
    // ptr = real_ptr + (goPtrdiff_t)(border_x * xDiff + border_y * yDiff + border_z * zDiff);
    T* p = new T[blocks.x * blockSizeX * blocks.y * blockSizeY * blocks.z * blockSizeZ];

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
                     border_z))
    {
        delete[] p;
        p = NULL;
        return false;
    }

    return true;
}

/*!
 * \example filter3d.cc
 * This is an example and test program for the macros used to filter goSignal3D
 * objects. See the source code for details.
 * @author Christian Gosch
 */

#endif



