#ifndef __GOSIGNAL3D_H__
#define __GOSIGNAL3D_H__

#include <config.h>
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
                    goSize_t blocksize_x = 32, goSize_t blocksize_y = 32, goSize_t blocksize_z = 32,
                    goSize_t border_x = 0, goSize_t border_y = 0, goSize_t border_z = 0);
        goSignal3D (goSignal3D<T>& other);
        virtual ~goSignal3D ();
        virtual const goSignal3D<T>& operator= (goSignal3DBase<T>& other);

        // From goObjectBase
        virtual goSize_t memoryUsage();

        bool make (goSize_t x, goSize_t y, goSize_t z,
                          goSize_t blocksize_x = 32,
                          goSize_t blocksize_y = 32,
                          goSize_t blocksize_z = 32,
                          goSize_t border_x    = 0,
                          goSize_t border_y    = 0,
                          goSize_t border_z    = 0);

        bool make (goSignal3D *other);

        virtual void destroy (); 
        void fillByte (goInt8 b);

        void fill (const T* element);
};
/** @} */


/*!
 * \example filter3d.cc
 * This is an example and test program for the macros used to filter goSignal3D
 * objects. See the source code for details.
 * @author Christian Gosch
 */
#endif



