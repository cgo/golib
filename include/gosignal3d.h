#ifndef __GOSIGNAL3D_H__
#define __GOSIGNAL3D_H__

#include <goconfig.h>
#include <iostream>
#include <fstream>
#include <gotypes.h>
#include <go3vector.h>		// sample()
#include <gosignal3dbase.h>

#include <assert.h>
#include <string.h>

/** \addtogroup signal 
 * @{ */
/*!
 * \brief Up to 3-dimensional (signal) data container.
 *
 * The memory layout is sort of optimized to get some
 * data locality when accessing the data locally in all
 * dimensions.
 * \author Christian Gosch
 */
template< class T >
class
goSignal3D : public goSignal3DBase<T>
{
    public:
        goSignal3D ();
        goSignal3D (goSize_t x, goSize_t y, goSize_t z,
                    goSize_t blocksize_x = 32, 
                    goSize_t blocksize_y = 32, 
                    goSize_t blocksize_z = 32,
                    goSize_t border_x = 0, 
                    goSize_t border_y = 0, 
                    goSize_t border_z = 0,
                    goSize_t channelCount = 1);
        goSignal3D (goSignal3D<T>& other);
        virtual ~goSignal3D ();
        const goSignal3D<T>& copy (const goSignal3DBase<T>& other);

        // From goObjectBase
        virtual goSize_t memoryUsage() const;

        bool make (const goSize3D& size,
                   const goSize3D& blockSize,
                   const goSize3D& borderSize,
                   goSize_t        channelCount);
        bool make (goSize_t x, goSize_t y, goSize_t z,
                          goSize_t blocksize_x = 32,
                          goSize_t blocksize_y = 32,
                          goSize_t blocksize_z = 32,
                          goSize_t border_x    = 0,
                          goSize_t border_y    = 0,
                          goSize_t border_z    = 0,
                          goSize_t channelCount = 1);

        bool make (goSignal3DBase<T> *other);

        virtual void destroy (); 
        void fillByte (goInt8 b);

        virtual void fill (const T* element);
    private:
        goSignal3D<T>& operator= (goSignal3D<T>& other);
};
/** @} */


/*!
 * \example filter3d.cc
 * This is an example and test program for the macros used to filter goSignal3D
 * objects. See the source code for details.
 * @author Christian Gosch
 */
#endif



