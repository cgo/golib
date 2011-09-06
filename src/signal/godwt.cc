/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


/* Sat Sep  8 15:54:25 CEST 2007
 * Removed specialisations so that no goSubSignal<T> is 
 * needed. This code is therefore currently
 * not used and not usable. 
 * TODO: Make available for <void> type signal3d,
 * for the integer types. */

#include <godwt.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gosignalmacros.h>
#include <goerror.h>
#include <gotypes.h>

#include <math.h>

template< class T >
goDWT<T>::goDWT()
{
}

template< class T >
goDWT<T>::~goDWT()
{
}

/*! \todo: Add void type support for goDWT */
#if 0
template <class T>
static inline void
_haarFilterX (goSignal3DBase<void>& signal, goSignal3DBase<void>& signal_target, goDouble tp0, goDouble tp1)
{
    signal_t *ptr_z               = signal.getPtr();
    signal_t *ptr                 = ptr_z;
    signal_t *ptr_y               = ptr_z;
    signal_target_t *ptr_z_target = signal_target.getPtr();
    signal_target_t *ptr_target   = ptr_z_target;
    signal_target_t *ptr_y_target = ptr_z_target;
    goPtrdiff_t* dx                 = signal.getXDiff();
    goPtrdiff_t* dy                 = signal.getYDiff();
    goPtrdiff_t* dz                 = signal.getZDiff();
    goPtrdiff_t* dx_target          = signal_target.getXDiff();
    goPtrdiff_t* dy_target          = signal_target.getYDiff();
    goPtrdiff_t* dz_target          = signal_target.getZDiff();
    goSize_t i, j, k;					
    godwt_t  tmp1, tmp2;

    goSignal3DGenericIterator sourceIt (&signal);
    goSignal3DGenericIterator targetIt (&signal_target);

    sourceIt.resetZ();
    targetIt.resetZ();
    goSize_t i;
    while (!sourceIt.endZ())
    {
        sourceIt.resetY();
        targetIt.resetY();
        while (!sourceIt.endY())
        {
            sourceIt.resetX();
            targetIt.resetX();
            for (k = 0; k < (signal.getSizeX() >> 1); ++k)   /* one less than size! */	
            {										
                tmp1 = *(T*)*sourceIt * tp0;			
                tmp2 = *(ptr + *dx) * tp1;		
                *ptr_target = tmp1 + tmp2;		
                ptr_target += *dx_target;
                ++dx_target;
                *ptr_target = tmp1 - tmp2;		
                ptr_target += *dx_target;					
                ptr += *dx + *(dx + 1);
                dx += 2;
                ++dx_target; 
            }
        }
    }
    
    for (i = 0; i < signal.getSizeZ(); i++)						
    {											
        ptr_y_target = ptr_z_target;							
        ptr_y = ptr_z;								
        for (j = 0; j < signal.getSizeY(); j++)					
        {										
            ptr = ptr_y;								
            ptr_target = ptr_y_target;						
            dx = signal.getXDiff(); 
            dx_target = signal_target.getXDiff(); 
            for (k = 0; k < (signal.getSizeX() >> 1); k++)   /* one less than size! */	
            {										
                tmp1 = *ptr * tp0;			
                tmp2 = *(ptr + *dx) * tp1;		
                *ptr_target = tmp1 + tmp2;		
                ptr_target += *dx_target;
                ++dx_target;
                *ptr_target = tmp1 - tmp2;		
                ptr_target += *dx_target;					
                ptr += *dx + *(dx + 1);
                dx += 2;
                ++dx_target; 
            }
            ptr_y_target += dy_target[j];
            ptr_y += dy[j];	
        }
        ptr_z_target += dz_target[i];
        ptr_z += dz[i];	
    }
}

static inline void
haarFilterX (goSignal3DBase<void>& signal, goSignal3DBase<void>& signal_target, goDouble tp0, goDouble tp1)
{
    signal_t *ptr_z               = signal.getPtr();
    signal_t *ptr                 = ptr_z;
    signal_t *ptr_y               = ptr_z;
    signal_target_t *ptr_z_target = signal_target.getPtr();
    signal_target_t *ptr_target   = ptr_z_target;
    signal_target_t *ptr_y_target = ptr_z_target;
    goPtrdiff_t* dx                 = signal.getXDiff();
    goPtrdiff_t* dy                 = signal.getYDiff();
    goPtrdiff_t* dz                 = signal.getZDiff();
    goPtrdiff_t* dx_target          = signal_target.getXDiff();
    goPtrdiff_t* dy_target          = signal_target.getYDiff();
    goPtrdiff_t* dz_target          = signal_target.getZDiff();
    goSize_t i, j, k;					
    godwt_t  tmp1, tmp2;

    goSignal3DGenericIterator sourceIt (&signal);
    goSignal3DGenericIterator targetIt (&signal_target);

    sourceIt.resetZ();
    targetIt.resetZ();
    goSize_t i;
    while (!sourceIt.endZ())
    {
        sourceIt.resetY();
        targetIt.resetY();
        while (!sourceIt.endY())
        {
            sourceIt.resetX();
            targetIt.resetX();
            for (k = 0; k < (signal.getSizeX() >> 1); ++k)   /* one less than size! */	
            {										
                tmp1 = *ptr * tp0;			
                tmp2 = *(ptr + *dx) * tp1;		
                *ptr_target = tmp1 + tmp2;		
                ptr_target += *dx_target;
                ++dx_target;
                *ptr_target = tmp1 - tmp2;		
                ptr_target += *dx_target;					
                ptr += *dx + *(dx + 1);
                dx += 2;
                ++dx_target; 
            }
        }
    }
    
    for (i = 0; i < signal.getSizeZ(); i++)						
    {											
        ptr_y_target = ptr_z_target;							
        ptr_y = ptr_z;								
        for (j = 0; j < signal.getSizeY(); j++)					
        {										
            ptr = ptr_y;								
            ptr_target = ptr_y_target;						
            dx = signal.getXDiff(); 
            dx_target = signal_target.getXDiff(); 
            for (k = 0; k < (signal.getSizeX() >> 1); k++)   /* one less than size! */	
            {										
                tmp1 = *ptr * tp0;			
                tmp2 = *(ptr + *dx) * tp1;		
                *ptr_target = tmp1 + tmp2;		
                ptr_target += *dx_target;
                ++dx_target;
                *ptr_target = tmp1 - tmp2;		
                ptr_target += *dx_target;					
                ptr += *dx + *(dx + 1);
                dx += 2;
                ++dx_target; 
            }
            ptr_y_target += dy_target[j];
            ptr_y += dy[j];	
        }
        ptr_z_target += dz_target[i];
        ptr_z += dz[i];	
    }
}
#endif

template<class signal_t, class signal_target_t>
static inline void
haarFilterX (goSignal3DBase<signal_t>& signal, goSignal3DBase<signal_target_t>& signal_target, goDouble tp0, goDouble tp1)
{
    signal_t *ptr_z               = signal.getPtr();
    signal_t *ptr                 = ptr_z;
    signal_t *ptr_y               = ptr_z;
    signal_target_t *ptr_z_target = signal_target.getPtr();
    signal_target_t *ptr_target   = ptr_z_target;
    signal_target_t *ptr_y_target = ptr_z_target;
    goPtrdiff_t* dx                 = signal.getXDiff();
    goPtrdiff_t* dy                 = signal.getYDiff();
    goPtrdiff_t* dz                 = signal.getZDiff();
    goPtrdiff_t* dx_target          = signal_target.getXDiff();
    goPtrdiff_t* dy_target          = signal_target.getYDiff();
    goPtrdiff_t* dz_target          = signal_target.getZDiff();
    goSize_t i, j, k;					
    godwt_t  tmp1, tmp2;


    for (i = 0; i < signal.getSizeZ(); i++)						
    {											
        ptr_y_target = ptr_z_target;							
        ptr_y = ptr_z;								
        for (j = 0; j < signal.getSizeY(); j++)					
        {										
            ptr = ptr_y;								
            ptr_target = ptr_y_target;						
            dx = signal.getXDiff(); 
            dx_target = signal_target.getXDiff(); 
            for (k = 0; k < (signal.getSizeX() >> 1); k++)   /* one less than size! */	
            {										
                tmp1 = *ptr * tp0;			
                tmp2 = *(ptr + *dx) * tp1;		
                *ptr_target = tmp1 + tmp2;		
                ptr_target += *dx_target;
                ++dx_target;
                *ptr_target = tmp1 - tmp2;		
                ptr_target += *dx_target;					
                ptr += *dx + *(dx + 1);
                dx += 2;
                ++dx_target; 
            }
            ptr_y_target += dy_target[j];
            ptr_y += dy[j];	
        }
        ptr_z_target += dz_target[i];
        ptr_z += dz[i];	
    }
}

template<class signal_t, class signal_target_t>
static inline void
haarFilterY (goSignal3DBase<signal_t>& signal, goSignal3DBase<signal_target_t>& signal_target, goDouble tp0, goDouble tp1)
{
    signal_t *ptr_z               = signal.getPtr();
    signal_t *ptr                 = ptr_z;
    signal_t *ptr_y               = ptr_z;
    signal_target_t *ptr_z_target = signal_target.getPtr();
    signal_target_t *ptr_target   = ptr_z_target;
    signal_target_t *ptr_y_target = ptr_z_target;
    goPtrdiff_t* dx                 = signal.getXDiff();
    goPtrdiff_t* dy                 = signal.getYDiff();
    goPtrdiff_t* dz                 = signal.getZDiff();
    goPtrdiff_t* dx_target          = signal_target.getXDiff();
    goPtrdiff_t* dy_target          = signal_target.getYDiff();
    goPtrdiff_t* dz_target          = signal_target.getZDiff();
    goSize_t i, j, k;					
    godwt_t  tmp1, tmp2;

    for (i = 0; i < signal.getSizeZ(); i++)
    {
        ptr_target = ptr_z_target;
        ptr = ptr_z;
        for (j = 0; j < signal.getSizeX(); j++)	
        {
            ptr_y = ptr;
            ptr_y_target = ptr_target;
            dy = signal.getYDiff();
            dy_target = signal_target.getYDiff();
            for (k = 0; k < (signal.getSizeY() >> 1); k++)   /* one less than size! */
            {
                tmp1 = *ptr_y * tp0;
                tmp2 = *(ptr_y + *dy) * tp1;
                *ptr_y_target = tmp1 + tmp2;
                ptr_y_target += *dy_target;
                ++dy_target;
                *ptr_y_target = tmp1 - tmp2;
                ptr_y_target += *dy_target;
                ptr_y += *dy + *(dy + 1);
                dy += 2;
                ++dy_target;
            }
            ptr_target += dx_target[j];	
            ptr += dx[j];
        }
        ptr_z_target += dz_target[i];
        ptr_z += dz[i];	
    }
}

template<class signal_t, class signal_target_t>
static inline void
haarFilterZ (goSignal3DBase<signal_t>& signal, goSignal3DBase<signal_target_t>& signal_target, goDouble tp0, goDouble tp1)
{
    signal_t *ptr_z               = signal.getPtr();
    signal_t *ptr                 = ptr_z;
    signal_t *ptr_y               = ptr_z;
    signal_target_t *ptr_z_target = signal_target.getPtr();
    signal_target_t *ptr_target   = ptr_z_target;
    signal_target_t *ptr_y_target = ptr_z_target;
    goPtrdiff_t* dx                 = signal.getXDiff();
    goPtrdiff_t* dy                 = signal.getYDiff();
    goPtrdiff_t* dz                 = signal.getZDiff();
    goPtrdiff_t* dx_target          = signal_target.getXDiff();
    goPtrdiff_t* dy_target          = signal_target.getYDiff();
    goPtrdiff_t* dz_target          = signal_target.getZDiff();
    goSize_t i, j, k;					
    godwt_t  tmp1, tmp2;

    for (i = 0; i < signal.getSizeX(); i++)						
    {											
        ptr_y_target = ptr_target;							
        ptr_y = ptr;								
        for (j = 0; j < signal.getSizeY(); j++)					
        {										
            ptr_z = ptr_y;								
            ptr_z_target = ptr_y_target;						
            dz = signal.getZDiff(); 
            dz_target = signal_target.getZDiff(); 
            for (k = 0; k < (signal.getSizeZ() >> 1); k++)   /* one less than size! */
            {
                tmp1 = *ptr_z * tp0;
                tmp2 = *(ptr_z + *dz) * tp1;
                *ptr_z_target = tmp1 + tmp2;
                ptr_z_target += *dz_target;
                ++dz_target;
                *ptr_z_target = tmp1 - tmp2;
                ptr_z_target += *dz_target;
                ptr_z += *dz + *(dz + 1);
                dz += 2;
                ++dz_target;
            }
            ptr_y_target += dy_target[j];
            ptr_y += dy[j];
        }
        ptr_target += dx_target[i];	
        ptr += dx[i];
    }
}

template<class signal_t>
static inline void
haarFilterXInplace (goSignal3DBase<signal_t>& signal, goDouble tp0, goDouble tp1)
{
    signal_t *ptr_z = signal.getPtr();
    signal_t *ptr   = ptr_z;
    signal_t *ptr_y = ptr_z;
    goPtrdiff_t* dx   = signal.getXDiff();
    goPtrdiff_t* dy   = signal.getYDiff();
    goPtrdiff_t* dz   = signal.getZDiff();
    goSize_t i, j, k;
    godwt_t  tmp1, tmp2;

    for (i = 0; i < signal.getSizeZ(); i++)	
    {
        ptr_y = ptr_z;								
        for (j = 0; j < signal.getSizeY(); j++)					
        {										
            ptr = ptr_y;								
            dx = signal.getXDiff(); 
            for (k = 0; k < (signal.getSizeX() >> 1); k++)   /* one less than size! */	
            {										
                tmp1 = *ptr * tp0;			
                tmp2 = *(ptr + *dx) * tp1;		
                *ptr = tmp1 + tmp2;		
                ptr += *dx;					
                ++dx;
                *ptr = tmp1 - tmp2;		
                ptr += *dx;					
                ++dx; // FIXME: This must be +2 at this point (?)
            }										
            ptr_y += dy[j];								
        }										
        ptr_z += dz[i];								
    }
}

template<class signal_t>
static inline void
haarFilterYInplace (goSignal3DBase<signal_t>& signal, goDouble tp0, goDouble tp1)
{
    signal_t *ptr_z = signal.getPtr();
    signal_t *ptr   = ptr_z;
    signal_t *ptr_y = ptr_z;
    goPtrdiff_t* dx   = signal.getXDiff();
    goPtrdiff_t* dy   = signal.getYDiff();
    goPtrdiff_t* dz   = signal.getZDiff();
    goSize_t i, j, k;
    godwt_t  tmp1, tmp2;

    for (i = 0; i < signal.getSizeZ(); i++)						
    {											
        ptr = ptr_z;								
        for (j = 0; j < signal.getSizeX(); j++)					
        {										
            ptr_y = ptr;								
            dy = signal.getYDiff(); 
            for (k = 0; k < (signal.getSizeY() >> 1); k++)   /* one less than size! */	
            {										
                tmp1 = *ptr_y * tp0;
                tmp2 = *(ptr_y + *dy) * tp1;		
                *ptr_y = tmp1 + tmp2;		
                ptr_y += *dy;
                ++dy;
                *ptr_y = tmp1 - tmp2;		
                ptr_y += *dy;					
                ++dy; 
            }										
            ptr += dx[j];								
        }										
        ptr_z += dz[i];									
    }											
}

template<class signal_t>
static inline void
haarFilterZInplace (goSignal3DBase<signal_t>& signal, goDouble tp0, goDouble tp1)
{
    signal_t *ptr_z = signal.getPtr();
    signal_t *ptr   = ptr_z;
    signal_t *ptr_y = ptr_z;
    goPtrdiff_t* dx   = signal.getXDiff();
    goPtrdiff_t* dy   = signal.getYDiff();
    goPtrdiff_t* dz   = signal.getZDiff();
    goSize_t i, j, k;
    godwt_t  tmp1, tmp2;

    for (i = 0; i < signal.getSizeX(); i++)						
    {											
        ptr_y = ptr;								
        for (j = 0; j < signal.getSizeY(); j++)					
        {										
            ptr_z = ptr_y;								
            dz = signal.getZDiff(); 
            for (k = 0; k < (signal.getSizeZ() >> 1); k++)   /* one less than size! */	
            {										
                tmp1 = *ptr_z * tp0;			
                tmp2 = *(ptr_z + *dz) * tp1;		
                *ptr_z = tmp1 + tmp2;		
                ptr_z += *dz;
                ++dz;
                *ptr_z = tmp1 - tmp2;		
                ptr_z += *dz;					
                ++dz; \
            }									
            ptr_y += dy[j];								
        }										
        ptr += dx[i];									
    }											
}


template<class signal_t, class signal_target_t>
static inline void
haarReverseX (goSignal3DBase<signal_t>& signal, goSignal3DBase<signal_target_t>& signal_target, goDouble tp0, goDouble tp1)
{
    godwt_t f0;
    godwt_t f1;
    f0 = 1.0f / (2 * tp0);
    f1 = 1.0f / (2 * tp1);

    signal_t *ptr_z               = signal.getPtr();
    signal_t *ptr                 = ptr_z;
    signal_t *ptr_y               = ptr_z;
    signal_target_t *ptr_z_target = signal_target.getPtr();
    signal_target_t *ptr_target   = ptr_z_target;
    signal_target_t *ptr_y_target = ptr_z_target;
    goPtrdiff_t* dx                 = signal.getXDiff();
    goPtrdiff_t* dy                 = signal.getYDiff();
    goPtrdiff_t* dz                 = signal.getZDiff();
    goPtrdiff_t* dx_target          = signal_target.getXDiff();
    goPtrdiff_t* dy_target          = signal_target.getYDiff();
    goPtrdiff_t* dz_target          = signal_target.getZDiff();
    goSize_t i, j, k;					
    godwt_t  tmp1, tmp2;

    for (i = 0; i < signal.getSizeZ(); i++)
    {							
        ptr_y_target = ptr_z_target;			
        ptr_y = ptr_z;					
        for (j = 0; j < signal.getSizeY(); j++)		
        {					
            ptr = ptr_y;							
            ptr_target = ptr_y_target;						
            dx = signal.getXDiff(); 
            dx_target = signal_target.getXDiff(); 
            for (k = 0; k < (signal.getSizeX() >> 1); k++)   /* one less than size! */	
            {										
                tmp1 = *ptr * f0;			
                tmp2 = *(ptr + *dx) * f0;		
                *ptr_target = (signal_target_t)(tmp1 + tmp2);		
                ptr_target += *dx_target;
                ++dx_target;
                *ptr_target = (signal_target_t)(tmp1 - tmp2);		
                ptr_target += *dx_target;					
                ptr += *dx + *(dx + 1);							
                dx += 2; 
                ++dx_target; 
            }										
            ptr_y_target += dy_target[j];						
            ptr_y += dy[j];								
        }										
        ptr_z_target += dz_target[i];							
        ptr_z += dz[i];									
    }											
}

template<class signal_t, class signal_target_t>
static inline void
haarReverseY (goSignal3DBase<signal_t>& signal, goSignal3DBase<signal_target_t>& signal_target, goDouble tp0, goDouble tp1)
{
    godwt_t f0;
    godwt_t f1;
    f0 = 1.0f / (2 * tp0);
    f1 = 1.0f / (2 * tp1);

    signal_t *ptr_z               = signal.getPtr();
    signal_t *ptr                 = ptr_z;
    signal_t *ptr_y               = ptr_z;
    signal_target_t *ptr_z_target = signal_target.getPtr();
    signal_target_t *ptr_target   = ptr_z_target;
    signal_target_t *ptr_y_target = ptr_z_target;
    goPtrdiff_t* dx                 = signal.getXDiff();
    goPtrdiff_t* dy                 = signal.getYDiff();
    goPtrdiff_t* dz                 = signal.getZDiff();
    goPtrdiff_t* dx_target          = signal_target.getXDiff();
    goPtrdiff_t* dy_target          = signal_target.getYDiff();
    goPtrdiff_t* dz_target          = signal_target.getZDiff();
    goSize_t i, j, k;					
    godwt_t  tmp1, tmp2;

    for (i = 0; i < signal.getSizeZ(); i++)						
    {											
        ptr_target = ptr_z_target;							
        ptr = ptr_z;								
        for (j = 0; j < signal.getSizeX(); j++)					
        {										
            ptr_y = ptr;								
            ptr_y_target = ptr_target;						
            dy = signal.getYDiff(); 
            dy_target = signal_target.getYDiff(); 
            for (k = 0; k < (signal.getSizeY() >> 1); k++)   /* one less than size! */	
            {										
                tmp1 = *ptr_y * f0;			
                tmp2 = *(ptr_y + *dy) * f0;		
                *ptr_y_target = tmp1 + tmp2;		
                ptr_y_target += *dy_target;
                ++dy_target;
                *ptr_y_target = tmp1 - tmp2;		
                ptr_y_target += *dy_target;					
                ptr_y += *dy + *(dy + 1);							
                dy += 2; 
                ++dy_target; 
            }										
            ptr_target += dx_target[j];						
            ptr += dx[j];								
        }										
        ptr_z_target += dz_target[i];							
        ptr_z += dz[i];									
    }											
}

template<class signal_t, class signal_target_t>
static inline void
haarReverseZ (goSignal3DBase<signal_t>& signal, goSignal3DBase<signal_target_t>& signal_target, goDouble tp0, goDouble tp1)
{
    godwt_t f0;
    godwt_t f1;
    f0 = 1.0f / (2 * tp0);
    f1 = 1.0f / (2 * tp1);

    signal_t *ptr_z               = signal.getPtr();
    signal_t *ptr                 = ptr_z;
    signal_t *ptr_y               = ptr_z;
    signal_target_t *ptr_z_target = signal_target.getPtr();
    signal_target_t *ptr_target   = ptr_z_target;
    signal_target_t *ptr_y_target = ptr_z_target;
    goPtrdiff_t* dx                 = signal.getXDiff();
    goPtrdiff_t* dy                 = signal.getYDiff();
    goPtrdiff_t* dz                 = signal.getZDiff();
    goPtrdiff_t* dx_target          = signal_target.getXDiff();
    goPtrdiff_t* dy_target          = signal_target.getYDiff();
    goPtrdiff_t* dz_target          = signal_target.getZDiff();
    goSize_t i, j, k;					
    godwt_t  tmp1, tmp2;

    for (i = 0; i < signal.getSizeX(); i++)						
    {											
        ptr_y_target = ptr_target;							
        ptr_y = ptr;								
        for (j = 0; j < signal.getSizeY(); j++)					
        {										
            ptr_z = ptr_y;							
            ptr_z_target = ptr_y_target;						
            dz = signal.getZDiff(); 
            dz_target = signal_target.getZDiff(); 
            for (k = 0; k < (signal.getSizeZ() >> 1); k++)   /* one less than size! */	
            {										
                tmp1 = *ptr_z * f0;			
                tmp2 = *(ptr_z + *dz) * f0;		
                *ptr_z_target = tmp1 + tmp2;		
                ptr_z_target += *dz_target;
                ++dz_target;
                *ptr_z_target = tmp1 - tmp2;		
                ptr_z_target += *dz_target;					
                ptr_z += *dz + *(dz + 1);							
                dz += 2;
                ++dz_target; 
            }										
            ptr_y_target += dy_target[j];						
            ptr_y += dy[j];								
        }										
        ptr_target += dx_target[i];							
        ptr += dx[i];									
    }											
}

template<class signal_t>
static inline void
haarReverseXInplace (goSignal3DBase<signal_t>& signal, goDouble tp0, goDouble tp1)
{
    signal_t *ptr_z = signal.getPtr();
    signal_t *ptr   = ptr_z;
    signal_t *ptr_y = ptr_z;
    goPtrdiff_t* dx   = signal.getXDiff();
    goPtrdiff_t* dy   = signal.getYDiff();
    goPtrdiff_t* dz   = signal.getZDiff();
    goSize_t i, j, k;
    godwt_t  tmp1, tmp2;

    godwt_t f0;
    godwt_t f1;
    f0 = 1 / (2 * tp0);
    f1 = 1 / (2 * tp1);


    for (i = 0; i < signal.getSizeZ(); i++)	
    {						
        ptr_y = ptr_z;						
        for (j = 0; j < signal.getSizeY(); j++)			
        {								
            ptr = ptr_y;					
            dx = signal.getXDiff(); 
            for (k = 0; k < (signal.getSizeX() >> 1); k++)   /* one less than size! */	
            {										
                tmp1 = *ptr * f0;			
                tmp2 = *(ptr + *dx) * f0;		
                *ptr = tmp1 + tmp2;		
                ptr += *dx;
                ++dx;
                *ptr = tmp1 - tmp2;		
                ptr += *dx;					
                ++dx; 
            }										
            ptr_y += dy[j];								
        }										
        ptr_z += dz[i];									
    }											
}
    
template<class signal_t> 
static inline void
haarReverseYInplace (goSignal3DBase<signal_t>& signal, goDouble tp0, goDouble tp1)
{
    signal_t *ptr_z = signal.getPtr();
    signal_t *ptr   = ptr_z;
    signal_t *ptr_y = ptr_z;
    goPtrdiff_t* dx   = signal.getXDiff();
    goPtrdiff_t* dy   = signal.getYDiff();
    goPtrdiff_t* dz   = signal.getZDiff();
    goSize_t i, j, k;
    godwt_t  tmp1, tmp2;

    godwt_t f0;
    godwt_t f1;
    f0 = 1 / (2 * tp0);
    f1 = 1 / (2 * tp1);

    for (i = 0; i < signal.getSizeZ(); i++)
    {										
        ptr = ptr_z;								
        for (j = 0; j < signal.getSizeX(); j++)					
        {										
            ptr_y = ptr;								
            dy = signal.getYDiff(); 
            for (k = 0; k < (signal.getSizeY() >> 1); k++)   /* one less than size! */	
            {										
                tmp1 = *ptr_y * f0;			
                tmp2 = *(ptr_y + *dy) * f0;		
                *ptr_y = tmp1 + tmp2;		
                ptr_y += *dy;
                ++dy;
                *ptr_y = tmp1 - tmp2;		
                ptr_y += *dy;					
                ++dy; 
            }										
            ptr += dx[j];								
        }										
        ptr_z += dz[i];									
    }											
}

template<class signal_t>
static inline void
haarReverseZInplace (goSignal3DBase<signal_t>& signal, goDouble tp0, goDouble tp1)
{
    signal_t *ptr_z = signal.getPtr();
    signal_t *ptr   = ptr_z;
    signal_t *ptr_y = ptr_z;
    goPtrdiff_t* dx   = signal.getXDiff();
    goPtrdiff_t* dy   = signal.getYDiff();
    goPtrdiff_t* dz   = signal.getZDiff();
    goSize_t i, j, k;
    godwt_t  tmp1, tmp2;

    godwt_t f0;
    godwt_t f1;
    f0 = 1 / (2 * tp0);
    f1 = 1 / (2 * tp1);

    for (i = 0; i < signal.getSizeX(); i++)						
    {											
        ptr_y = ptr;								
        for (j = 0; j < signal.getSizeY(); j++)				
        {										
            ptr_z = ptr_y;								
            dz = signal.getZDiff(); 
            for (k = 0; k < (signal.getSizeZ() >> 1); k++)   /* one less than size! */	
            {										
                tmp1 = *ptr_z * f0;			
                tmp2 = *(ptr_z + *dz) * f0;		
                *ptr_z = tmp1 + tmp2;		
                ptr_z += *dz;
                ++dz;
                *ptr_z = tmp1 - tmp2;	
                ptr_z += *dz;
                ++dz;
            }										
            ptr_y += dy[j];								
        }										
        ptr += dx[i];									
    }											
}

inline
static
void
F1 (goSignal3D<godwt_t>& input_signal, goSignal3D<godwt_t>& output_signal)
{
  
}

template<class T>
void
goDWT<T>::haar(goSignal3D<T>& signal, goSignal3D<godwt_t>& targetSignal)
{
  goSignal3D<godwt_t>	temp1;  // 
  goSignal3D<godwt_t>	temp2;  
  temp1.make (signal.getSizeX(), signal.getSizeY(), signal.getSizeZ());
  temp2.make (signal.getSizeX(), signal.getSizeY(), signal.getSizeZ());
  
  goDouble tp0 = 0.5;
  goDouble tp1 = tp0;
  
  haarFilterX (signal, temp1, tp0, tp1);

  /*
   * temp1: L H L H L H ....
   *        L H L H L H ....
   */
  haarFilterY (temp1,temp2,tp0,tp1);

  /*
   * temp2: LL HL ...
   *        LH HH ...
   *        .....
   */
  haarFilterZ (temp2,targetSignal,tp0,tp1);

  /*
   * targetSignal:    .
   *                .
   *         LLL HLL ....
   *         LHL HHL ....
   *         ......
   */

  temp1.destroy();
  temp2.destroy();
}

// template<class T>
// void
// goDWT<T>::haar (goSignal3D<T>& signal)
// {
//   goError::note("goDWT::haar()","Not implemented for this data type. Please use haar(goSignal3D, goSignal3D)");
//   exit(2);
// }
#if 0
template<> void
goDWT<goFloat>::haar(goSubSignal3D<goFloat>& signal)
{
  goDouble tp0 = 0.5;
  goDouble tp1 = tp0;
  
  haarFilterXInplace (signal, tp0, tp1);

  /*
   * temp1: L H L H L H ....
   *        L H L H L H ....
   */
  haarFilterYInplace (signal, tp0, tp1);

  /*
   * temp2: LL HL ...
   *        LH HH ...
   *        .....
   */
  haarFilterZInplace (signal, tp0, tp1);
}

template<> void
goDWT<goDouble>::haar(goSubSignal3D<goDouble>& signal)
{
  goDouble tp0 = 0.5;
  goDouble tp1 = tp0;

  haarFilterXInplace (signal, tp0, tp1);   
 
  /*
   * temp1: L H L H L H ....
   *        L H L H L H ....
   */
  haarFilterYInplace (signal, tp0, tp1);   

  /*
   * temp2: LL HL ...
   *        LH HH ...
   *        .....
   */
  haarFilterZInplace (signal, tp0, tp1);   
}
#endif

template< class T >
void
goDWT<T>::unHaar (goSignal3D<godwt_t>& haarSignal, goSignal3D<T>& targetSignal)
{
  goDouble tp0 = 0.5;
  goDouble tp1 = tp0;
  
  goSignal3D<godwt_t>	temp1;
  goSignal3D<godwt_t>	temp2;
  temp1.make (haarSignal.getSizeX(), haarSignal.getSizeY(), haarSignal.getSizeZ());
  temp2.make (haarSignal.getSizeX(), haarSignal.getSizeY(), haarSignal.getSizeZ());

  haarReverseZ (haarSignal, temp1, tp0, tp1);
  
  haarReverseY (temp1, temp2, tp0, tp1);

  haarReverseX (temp2, targetSignal, tp0, tp1);

  temp1.destroy();
  temp2.destroy();
} 

#if 0
template<> void
goDWT<goFloat>::unHaar(goSubSignal3D<goFloat>& haarSignal)
{
  goDouble tp0 = 0.5;
  goDouble tp1 = tp0;
  
  haarReverseZInplace (haarSignal, tp0, tp1);
  haarReverseYInplace (haarSignal, tp0, tp1);
  haarReverseXInplace (haarSignal, tp0, tp1);
} 

template<> void
goDWT<goDouble>::unHaar (goSubSignal3D<goDouble>& haarSignal)
{
  goDouble tp0 = 0.5;
  goDouble tp1 = tp0;
  
  haarReverseZInplace (haarSignal, tp0, tp1);
  haarReverseYInplace (haarSignal, tp0, tp1);
  haarReverseXInplace (haarSignal, tp0, tp1);
} 
#endif

template< class T >
int
goDWT<T>::haar (goSignal3D<T>& signal, int stage)
{
    assert (stage > 0);
    int i;
    goSubSignal3D<T>  s;
    s.setParent(&signal);
    s.setSize (signal.getSizeX(),
            signal.getSizeY(),
            signal.getSizeZ());
    for (i = 0; i < stage; i++)
    {
        haar (s);
        s.shiftRightSize(1);
        s.shiftLeftDiff(1);
    }
    return stage;
}

template< class T >
int
goDWT<T>::unHaar(goSignal3D<T>& signal, int stage)
{
    assert (stage > 0);
    int i;
    goSubSignal3D<T>  s;
    s.setParent(&signal);
    s.setSize (signal.getSizeX() >> (stage - 1),
               signal.getSizeY() >> (stage - 1),
               signal.getSizeZ() >> (stage - 1));
    //s.setDiff (signal.getXDiff() << (stage - 1),
    //     signal.getYDiff() << (stage - 1),
    //     signal.getZDiff() << (stage - 1));
    s.setSkip ((1 << (stage - 1)) - 1, 
               (1 << (stage - 1)) - 1, 
               (1 << (stage - 1)) - 1);

    for (i = 0; i < stage; i++)
    {
        unHaar (s);
        s.shiftLeftSize(1);
        s.shiftRightDiff(1);
    }
    return stage;
}


/***********************************************************************************/
/* Specialised integer routines follow						   */
/***********************************************************************************/

/*******************************************************/
/* Don't present this to the open public ---           */
/* nobody wants to read messy macros.                  */
/*******************************************************/

#define ST_XY_BEGIN(__block, __T) {\
  __T *p;\
  goSize_t x = __block.getSizeX();\
  goSize_t y = __block.getSizeY();\
/*  goSize_t z = __block.getSizeZ(); */\
  goPtrdiff_t* dx       = __block.getXDiff();\
  goPtrdiff_t* dy       = __block.getYDiff();\
/*  goPtrdiff_t* dz       = __block.getZDiff(); */ \
  goPtrdiff_t* zAddress = __block.getZJump();\
  register int r1,r2,r3,r4,r5,r6,r7,r8; /* __T */ \
  register __T *p1;\
  __T *p2;\
  __T *p3;\
  __T *p4;\
  __T *p5;\
  __T *p6;\
  __T *p7;\
  __T *p8;\
  register goSize_t i,j;\
  int tmp1, tmp2, tmp3, tmp4;  /* __T */

#define ST_XY_END() }

/* Use 32 bit for storage of the
 * transform. Unfortunately, the modified S-Transform implies this.
 * S Transform a slice.
 * Call format:
 * STSlice (go3DBlock<GO_STRANSFORM_IN_T> *block, 
 *          go3DBlock<GO_STRANSFORM_OUT_T> *newBlock, 
 *          goIndex_t slice);
 */
#define ST_XY(__block, __slice) { \
  p = __block.getPtr(); \
  p += zAddress[__slice];\
  dy = __block.getYDiff();\
\
  for (i = y; i > 0; i -= 2) {\
    dx = __block.getXDiff();\
    p1 = p;\
    p2 = p1 + *dx;\
    p3 = p1 + *dy;\
    p4 = p3 + *dx; /* Works as long as the line offsets are the same for all lines (true) */\
    for (j = x; j > 0; j -= 2) {\
      r1 = *p1 + *(p1 + *dy);\
      r2 = *(p1 + *dx + *dy) + *(p1 + *dx);\
      /* division by 2: integer shift */\
      r3 = r1 >> 1;\
      r4 = r2 >> 1;\
\
      /* S(m,n) */\
      tmp1 = (r3 + r4) >> 1;\
      /* S(m+1,n) */\
      tmp2 = r3 - r4;\
      /* S(m,n+1) */\
      tmp3 = (*p1 - *(p1 + *dy) - *(p1 + *dx + *dy) + *(p1 + *dx)) >> 1;\
      /* S(m+1,n+1) */\
      tmp4 = *p1 - *(p1 + *dy) + *(p1 + *dx + *dy) - *(p1 + *dx);\
\
      *p1 = tmp1;\
      *p2 = tmp2;\
      *p3 = tmp3;\
      *p4 = tmp4;\
      \
      /* p1 += dx << 1; */\
      /* p2 += dx << 1; */\
      /* p3 += dx << 1; */\
      /* p4 += dx << 1; */\
      p1 += *dx + *(dx + 1);\
      p2 += *dx + *(dx + 1);\
      p3 += *dx + *(dx + 1);\
      p4 += *dx + *(dx + 1);\
      ++dx;\
    }\
    /* p += dy << 1; */\
    p += *dy + *(dy + 1);\
    ++dy;\
  }\
}

#define ST_REVERSE_XY_BEGIN(__block, __T) {\
  __T *p = __block.getPtr();\
  goSize_t x = __block.getSizeX();\
  goSize_t y = __block.getSizeY();\
/*  goSize_t z = __block.getSizeZ(); */\
  goPtrdiff_t* dx = __block.getXDiff();\
  goPtrdiff_t* dy = __block.getYDiff();\
  /* register goPtrdiff_t dx_2 = dx << 1;*/ \
  /* register goPtrdiff_t dy_2 = dy << 1;*/ \
\
  goSize_t i,j;\
  register int a_,b_,a,b,A_,B_,A,B; /* __T */ \
  int r1,r2,r3,r4,r5,r6,r7,r8; /* __T */ \
  int a1,a2,a3,a4,a5,a6,a7,a8; /* __T */ \
  __T *p1,*p2,*p3,*p4,*p5,*p6,*p7,*p8;

#define ST_REVERSE_XY_END() }

/* Reverse transform a slice
 * Call format:
 * TSSlice (go3DBlock<GO_STRANSFORM_OUT_T> *block, 
 *          go3DBlock<GO_STRANSFORM_IN_T> *newBlock, 
 *          goIndex_t slice);
 */
#define ST_REVERSE_XY(__block, __slice) {\
  p = __block.getPtr(); \
  /* p += __slice * __block.getZDiff();*/ \
  p += __block.getZJump()[__slice];\
  dy = __block.getYDiff(); \
  \
  /* OUT_T tmp1,tmp2,tmp3,tmp4; */\
  for (i = (y >> 1); i > 0; i--) {\
    dx = __block.getXDiff(); \
    p1 = p;\
/*      p2 = p1 + ( (x >> 1) * dx ); */\
/*      p3 = p1 + ( (y >> 1) * dy ); */\
/*      p4 = p3 + ( (x >> 1) * dx ); */\
    p2 = p1 + *dx;\
    p3 = p1 + *dy;\
    p4 = p3 + *dx;\
    for (j = (x >> 1); j > 0; j--) {\
      A_ = *p1;\
      B_ = *p2;\
      A  = *p3;\
      B  = *p4;\
      a_ = A_ - ( (-B_) >> 1 );\
      b_ = a_ - B_;\
      a  = A - ( (-B) >> 1 );\
      b  = a - B;\
\
      /* *newP1			= a_ - ( (-a) >> 1 ); */\
      A				= a_ - ( (-a) >> 1 );\
      *p1 = A;\
      *(p1 + *dy)		= A - a;\
      /* *(newP1 + newdx)	= b_ - ( (-b) >> 1 ); */\
      B				= b_ - ( (-b) >> 1 );\
      *(p1 + *dx) = B;\
      *(p1 + *dx + *dy)	= B - b;\
      \
      \
      /* p1	+= dx_2;*/ \
      /* p2	+= dx_2;*/ \
      /* p3	+= dx_2;*/ \
      /* p4	+= dx_2;*/ \
      p1 += *dx + *(dx + 1); \
      p2 += *dx + *(dx + 1); \
      p3 += *dx + *(dx + 1); \
      p4 += *dx + *(dx + 1); \
      ++dx; \
    }\
    /* p += dy_2; */ \
    p += *dy + *(dy + 1); \
    ++dy; \
  }\
}


/*
 * STZ and TSZ are done in-place since they use the same data type for
 * input and output. 
 * The data organisation in {ST|TS}Slice is as described in godwt.h.
 * The coder has to step through the data
 * not in 1-steps but in 2-steps with an appropriate offset for each 
 * subband.
 */
#define ST_Z(STZ_block, __T) {\
  __T *STZ_p	= STZ_block.getPtr();\
\
  goSize_t STZ_x = STZ_block.getSizeX();\
  goSize_t STZ_y = STZ_block.getSizeY();\
  goSize_t STZ_z = STZ_block.getSizeZ();\
\
  register goPtrdiff_t* STZ_dx = STZ_block.getXDiff();\
  register goPtrdiff_t* STZ_dy = STZ_block.getYDiff();\
  goPtrdiff_t* STZ_dz = STZ_block.getZDiff();\
\
  register goIndex_t STZ_i,STZ_j,STZ_k;\
  /* register OUT_T r1,r2,r3,r4,r5,r6,r7,r8; */\
  /* register OUT_T a1,a2,a3,a4,a5,a6,a7,a8; */\
  /* OUT_T *p1,*p2,*p3,*p4,*p5,*p6,*p7,*p8; */\
  __T *pSave = STZ_p;\
  for (STZ_k = (STZ_z >> 1); STZ_k > 0; --STZ_k) {\
    STZ_p = pSave;\
    STZ_dy = STZ_block.getYDiff(); \
    for (STZ_j = (STZ_y >> 1); STZ_j > 0; --STZ_j) {\
      STZ_dx = STZ_block.getXDiff(); \
      p1 = STZ_p;\
      /* FIXME: This may be an error source. Check if errors occur in the transform. */ \
      p2 = p1 + *STZ_dz;\
      p3 = p1 + *STZ_dx;\
      p4 = p3 + *STZ_dz;\
      p5 = p1 + *STZ_dy;\
      p6 = p5 + *STZ_dz;\
      p7 = p3 + *STZ_dy;\
      p8 = p7 + *STZ_dz;\
/*        newP2 = newP1 + (newdz * (z >> 1)); */\
/*        newP3 = newP1 + (newdx * (x >> 1)); */\
/*        newP4 = newP3 + (newdz * (z >> 1)); */\
/*        newP5 = newP1 + (newdy * (y >> 1)); */\
/*        newP6 = newP5 + (newdz * (z >> 1)); */\
/*        newP7 = newP3 + (newdy * (y >> 1)); */\
/*        newP8 = newP7 + (newdz * (z >> 1)); */\
      for (STZ_i = (STZ_x >> 1); STZ_i > 0; STZ_i--) {\
	r1 = *p1;\
	r2 = *p2;\
	r3 = *p3;\
	r4 = *p4;\
	r5 = *p5;\
	r6 = *p6;\
	r7 = *p7;\
	r8 = *p8;\
\
	*p1 = (r1 + r2) >> 1;\
	*p2 = r1 - r2;\
	*p3 = (r3 + r4) >> 1;\
	*p4 = r3 - r4;\
	*p5 = (r5 + r6) >> 1;\
	*p6 = r5 - r6;\
	*p7 = (r7 + r8) >> 1;\
	*p8 = r7 - r8;\
\
	/* p1 += STZ_dx << 1;*/\
	/* p2 += STZ_dx << 1;*/\
	/* p3 += STZ_dx << 1;*/\
	/* p4 += STZ_dx << 1;*/\
	/* p5 += STZ_dx << 1;*/\
	/* p6 += STZ_dx << 1;*/\
	/* p7 += STZ_dx << 1;*/\
	/* p8 += STZ_dx << 1;*/\
    p1 += *STZ_dx + *(STZ_dx + 1); \
    p2 += *STZ_dx + *(STZ_dx + 1); \
    p3 += *STZ_dx + *(STZ_dx + 1); \
    p4 += *STZ_dx + *(STZ_dx + 1); \
    p5 += *STZ_dx + *(STZ_dx + 1); \
    p6 += *STZ_dx + *(STZ_dx + 1); \
    p7 += *STZ_dx + *(STZ_dx + 1); \
    p8 += *STZ_dx + *(STZ_dx + 1); \
    ++STZ_dx; \
      }\
      /* STZ_p += STZ_dy << 1; */\
      STZ_p += *STZ_dy + *(STZ_dy + 1); \
      ++STZ_dy; \
    }\
    /* pSave += STZ_dz << 1; */\
    pSave += *STZ_dz + *(STZ_dz + 1);\
    ++STZ_dz; \
  }\
}


#define ST_REVERSE_Z(TSZ_block, __T) {\
  __T *TSZ_p = TSZ_block.getPtr();\
\
  goSize_t TSZ_x = TSZ_block.getSizeX();\
  goSize_t TSZ_y = TSZ_block.getSizeY();\
  goSize_t TSZ_z = TSZ_block.getSizeZ();\
\
  register goPtrdiff_t* TSZ_dx = TSZ_block.getXDiff();\
  register goPtrdiff_t* TSZ_dy = TSZ_block.getYDiff();\
  goPtrdiff_t* TSZ_dz = TSZ_block.getZDiff();\
\
  /* register goPtrdiff_t TSZ_dx_2 = TSZ_dx << 1;*/\
  /* register goPtrdiff_t TSZ_dy_2 = TSZ_dy << 1;*/\
  /* goPtrdiff_t TSZ_dz_2 = TSZ_dz << 1;*/\
\
  register goIndex_t TSZ_i,TSZ_j,TSZ_k;\
  /* register OUT_T r1,r2,r3,r4,r5,r6,r7,r8; */ \
  /* register OUT_T a1,a2,a3,a4,a5,a6,a7,a8; */ \
  /* OUT_T *p1,*p2,*p3,*p4,*p5,*p6,*p7,*p8; */ \
  __T *pSave = TSZ_p;\
  \
  for (TSZ_k = (TSZ_z >> 1); TSZ_k > 0; --TSZ_k) {\
    TSZ_p = pSave;\
    TSZ_dy = TSZ_block.getYDiff(); \
    for (TSZ_j = (TSZ_y >> 1); TSZ_j > 0; --TSZ_j) {\
      TSZ_dx = TSZ_block.getXDiff(); \
      p1 = TSZ_p;\
      p2 = p1 + *TSZ_dz;\
      p3 = p1 + *TSZ_dx;\
      p4 = p3 + *TSZ_dz;\
      p5 = p1 + *TSZ_dy;\
      p6 = p5 + *TSZ_dz;\
      p7 = p3 + *TSZ_dy;\
      p8 = p7 + *TSZ_dz;\
      for (TSZ_i = (TSZ_x >> 1); TSZ_i > 0; --TSZ_i) {\
	r1 = *p1;\
	r2 = *p2;\
	r3 = *p3;\
	r4 = *p4;\
	r5 = *p5;\
	r6 = *p6;\
	r7 = *p7;\
	r8 = *p8;\
	\
	a1 = (r1 - ( (-r2) >> 1 ));\
	a2 = (a1 - r2);\
	a3 = (r3 - ( (-r4) >> 1 ));\
	a4 = (a3 - r4);\
	a5 = (r5 - ( (-r6) >> 1 ));\
	a6 = (a5 - r6);\
	a7 = (r7 - ( (-r8) >> 1 ));\
	a8 = (a7 - r8);\
	*p1 = a1;\
	*p2 = a2;\
	*p3 = a3;\
	*p4 = a4;\
	*p5 = a5;\
	*p6 = a6;\
	*p7 = a7;\
	*p8 = a8;\
\
	/* p1 += TSZ_dx_2;*/\
	/* p2 += TSZ_dx_2;*/\
	/* p3 += TSZ_dx_2;*/\
	/* p4 += TSZ_dx_2;*/\
	/* p5 += TSZ_dx_2;*/\
	/* p6 += TSZ_dx_2;*/\
	/* p7 += TSZ_dx_2;*/\
	/* p8 += TSZ_dx_2;*/\
    p1 += *TSZ_dx + *(TSZ_dx + 1); \
    p2 += *TSZ_dx + *(TSZ_dx + 1); \
    p3 += *TSZ_dx + *(TSZ_dx + 1); \
    p4 += *TSZ_dx + *(TSZ_dx + 1); \
    p5 += *TSZ_dx + *(TSZ_dx + 1); \
    p6 += *TSZ_dx + *(TSZ_dx + 1); \
    p7 += *TSZ_dx + *(TSZ_dx + 1); \
    p8 += *TSZ_dx + *(TSZ_dx + 1); \
    ++TSZ_dx; \
    }\
    /* TSZ_p += TSZ_dy_2; */\
    TSZ_p += *TSZ_dy + *(TSZ_dy + 1); \
    ++TSZ_dy; \
   }\
    /* pSave += TSZ_dz_2;*/\
    pSave += *TSZ_dz + *(TSZ_dz + 1); \
    ++TSZ_dz; \
  }\
}

/* This method uses not exactly a Haar transform, but rather an enhanced
 * S Transform using only integers, additions, and shifts, which makes it faster
 * than a floating point transform on some machines.
 */											
#define GO_DWT_INTEGER_HAAR_METHOD(__TYPE)						 \
template<> void											 \
goDWT<__TYPE>::haar (goSubSignal3D<__TYPE> &signal) {					 \
  /* 2D ST of each xy slice */								 \
  goIndex_t i_st;									 \
  ST_XY_BEGIN(signal, __TYPE)								 \
  for (i_st = 0; i_st < (goIndex_t)signal.getSizeZ(); i_st++) 				 \
    {											 \
      ST_XY (signal, i_st);								 \
    }											 \
  /* 1D ST in z direction */								 \
  ST_Z (signal, __TYPE);								 \
  ST_XY_END()										 \
}

#define GO_DWT_INTEGER_UNHAAR_METHOD(__TYPE)					\
template<> void										\
goDWT<__TYPE>::unHaar (goSubSignal3D<__TYPE> &signal) {				\
  goIndex_t i_ts;								\
  ST_REVERSE_XY_BEGIN(signal, __TYPE);						\
  /* 1D reverse ST in z direction */						\
  ST_REVERSE_Z (signal, __TYPE);  						\
  for (i_ts = 0; i_ts < (goIndex_t)signal.getSizeZ(); i_ts++) {			\
    ST_REVERSE_XY (signal, i_ts);						\
  }										\
  ST_REVERSE_XY_END()								\
}


/*
 * Yes, I know this is bad style. Yes, I COULD handle it differently.
 * BUT I want to keep it this way in case this class will be needed for some special types 
 * and I have to do some specialised member.
 * Unfortunately, my c++ compiler apparently does not handle inline templates.
 */
#if 0
GO_DWT_INTEGER_HAAR_METHOD(goInt8)
//GO_DWT_INTEGER_HAAR_METHOD(goUInt8)
GO_DWT_INTEGER_HAAR_METHOD(goInt16)
//GO_DWT_INTEGER_HAAR_METHOD(goUInt16)
//GO_DWT_INTEGER_HAAR_METHOD(goInt32)
//GO_DWT_INTEGER_HAAR_METHOD(goUInt32)

GO_DWT_INTEGER_UNHAAR_METHOD(goInt8)
//GO_DWT_INTEGER_UNHAAR_METHOD(goUInt8)
GO_DWT_INTEGER_UNHAAR_METHOD(goInt16)
//GO_DWT_INTEGER_UNHAAR_METHOD(goUInt16)
//GO_DWT_INTEGER_UNHAAR_METHOD(goInt32)
//GO_DWT_INTEGER_UNHAAR_METHOD(goUInt32)
#endif 

/*
 * Static function STZ is used to perform the transform along the 3rd axis.
 * <CODE>STSlice()</CODE> and <CODE>STZ()</CODE> should be merged to 
 * enhance the performance.
 */
// int
// goDWT<goInt16>::haar (goSignal3D<goInt16> &signal, int stage) {
//   /* This method uses not exactly a Haar transform, but rather an enhanced
//    * S Transform using only integers, additions, and shifts, which makes it faster
//    * than a floating point transform on some machines. 
//    */

//   /* 2D ST of each xy slice */
//   goIndex_t i_st;
//   ST_XY_BEGIN(signal, goInt16)
//   for (i_st = 0; i_st < (goIndex_t)signal.getSizeZ(); i_st++) 
//     {
//       ST_XY (signal, i_st);
//     }
//   /* 1D ST in z direction */
//   ST_Z (signal, goInt16);
//   ST_XY_END()
  
//   return stage;
// }

/*
 * Reverse of <CODE>haar(integer_type)</CODE>.
 * See <CODE>haar(integer_type)</CODE> for comments.
 */
// int
// goDWT<goInt16>::unHaar (goSignal3D<goInt16> &signal, int stage) {
//   goIndex_t i_ts;
//   ST_REVERSE_XY_BEGIN(signal, goInt16);
//   /* 1D reverse ST in z direction */
//   ST_REVERSE_Z (signal, goInt16);  
//   for (i_ts = 0; i_ts < (goIndex_t)signal.getSizeZ(); i_ts++) {
//     ST_REVERSE_XY (signal, i_ts);
//   }
//   ST_REVERSE_XY_END()
//   return stage;
// }

#if 0
template class goDWT<goInt8>;
//template class goDWT<goUInt8>;
template class goDWT<goInt16>;
//template class goDWT<goUInt16>;
//template class goDWT<goInt32>;
//template class goDWT<goUInt32>;
template class goDWT<goFloat>;
template class goDWT<goDouble>;
#endif
