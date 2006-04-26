/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id: gosignal3dgenericiterator.h,v 1.1.1.1 2006/04/19 15:27:07 gosch Exp $
 */

#ifndef GOSIGNAL3DGENERICITERATOR_H
#define GOSIGNAL3DGENERICITERATOR_H

#ifndef GOTYPES_H
# include <gotypes.h>
#endif

#include <gosignal3dbase.h>

/** 
 * @addtogroup signal
 */
/** @{ */
/**
 * @brief  Iterator for goSignal3DBase<void> class signals.
 *
 * This is an iterator class for <void> type signals, the preferred signal class.
 *
 * Usage is like this:
 * <pre>
 * <code>
 *  goSignal3D<void> sig;
 *  ....
 *  goSignal3DGenericIterator it(&sig);
 *  while (!it.endZ())
 *  {
 *      it.resetY();
 *      while (!it.endY())
 *      {
 *          it.resetX();
 *          while (!it.endX())
 *          {
 *              // We *know* this is a int16 signal
 *              *(goInt16*)*it = // ... do something
 *              it.incrementX();
 *          }
 *          it.incrementY();
 *      }
 *      it.incrementZ();
 *  }
 * </code>
 * </pre>
 * @note It may be faster to use the GO_SIGNAL3D_EACHELEMENT* macros for 
 * iterating through a signal. However, with void type signals, you should be ok with
 * this since the typical inner-loop methods are inlined. 
 * If you are optimising for speed, it may still be a good idea to simply write your own loop.
 * @note Mind that if you want to reuse the iterator from the example above, 
 * you must reset it with resetZ(); resetY(); resetX();
 * @note If you want to iterate over a subwindow of a signal, it may be a good idea
 * to use goSubSignal3D<void>.
 * @note Be careful when you use <b>multichannel signals</b>. There is a pitfall:
 *  <pre>
 *       <code>
 *        sig.setChannel(1);
 *        goSignal3DGenericIterator it(sig);
 *        sig.setChannel(2);
 *       </code>
 *  </pre>
 *       The iterator will run on channel 1 !!!!
 *       It will run on channel 2 when you put a it.resetZ() or a it.setPosition(...)
 *       after setting the channel in sig.
 *
 * @todo Maybe do something about the pitfall described in the note (documentation).
 *       Low priority.
 * 
 **/
class goSignal3DGenericIterator
{
    public:
        goSignal3DGenericIterator  (goSignal3DBase<void>* s);
        goSignal3DGenericIterator  (const goSignal3DGenericIterator& other);
        ~goSignal3DGenericIterator ();
        
        /**
         * @brief Sets the iterator position to x,y,z in the signal.
         *
         * @param x  X position
         * @param y  Y position
         * @param z  Z position
         **/
        inline void setPosition (goIndex_t x, goIndex_t y, goIndex_t z)
        {
            posX = x; posY = y; posZ = z;
            dx = sig->getXDiff() + x;
            dy = sig->getYDiff() + y;
            dz = sig->getZDiff() + z;
            px = (goByte*)sig->getPtr(x,y,z);
            py = px; // (goByte*)sig->getPtr(x,y,z);
            pz = px; // (goByte*)sig->getPtr(x,y,z);
        }
        
        /**
         * @return True if the end of the current x-line is reached, false otherwise.
         */
        inline bool     endX        ()
        {
            return posX > maxX;
        };
        /**
         * @return True if the end of the current y-plane is reached, false otherwise.
         */
        inline bool     endY        ()
        {
            return posY > maxY;
        };
        /**
         * @return True if the end of the current z-plane is reached (end of the 3D signal), false otherwise.
         */
        inline bool     endZ        ()
        {
            return posZ > maxZ;
        };
        inline void     incrementX  ()
        {
            px += *dx;
            ++posX;
            ++dx;
        };
        inline void     incrementY  ()
        {
            py += *dy;
            ++posY;
            ++dy;
        };
        /** --------------------------------------------------------------------------
         * @bug See todo.
         * @todo This runs over the edge of the dz array when there is no 
         *       border (holds for all increment functions). This leads
         *       to an invalid read operation which may lead to problems.
         *       Check if there is a way to fix this without having to change
         *       all iterator calls.
         ----------------------------------------------------------------------------*/
        inline void     incrementZ  ()
        {
            pz += *dz;
            ++posZ;
            ++dz;
        };
        inline void     decrementX  ()
        {
            px -= *(dx-1);
            --posX;
            --dx;
        };
        inline void     decrementY  ()
        {
            py -= *(dy-1);
            --posY;
            --dy;
        };
        inline void     decrementZ  ()
        {
            pz -= *(dz-1);
            --posZ;
            --dz;
        };
        /**
         * @brief Resets the X pointer and internals concerning X to the 
         * beginning of the current x-line.
         **/
        inline void     resetX      ()
        {
            dx   = dxStart;
            posX = 0;
            px   = py;
        };
        /**
         * @brief Resets the Y pointer and internals concerning Y to the 
         * beginning of the current z-plane.
         **/
        inline void     resetY      ()
        {
            dy   = dyStart;
            posY = 0;
            py   = pz;
            resetX ();
        };
        void     resetZ      ();
        /** 
         * @return Pointer to the data element at the current iterator position.
         */
        inline goByte*  operator*   () { return px; };
        inline goByte*  leftX       () { return px - *(dx-1); }
        inline goByte*  leftY       () { return px - *(dy-1); }
        inline goByte*  leftZ       () { return px - *(dz-1); }
        inline goByte*  rightX      () { return px + *dx; }
        inline goByte*  rightY      () { return px + *dy; }
        inline goByte*  rightZ      () { return px + *dz; }
        
        inline goByte* leftUp    () { return px - *(dx-1) - *(dy-1); }  //= Works because dy at left is the same as at px.
        inline goByte* leftDown  () { return px - *(dx-1) + *dy; }  
        inline goByte* rightUp   () { return px + *dx - *(dy-1); }  
        inline goByte* rightDown () { return px + *dx + *dy; }  
        
        inline const goByte* operator*   ()  const { return px; };
       
        goSignal3DBase<void>* sig;
        goPtrdiff_t*          dx;
        goPtrdiff_t*          dy;
        goPtrdiff_t*          dz;
        goPtrdiff_t*          dxStart;
        goPtrdiff_t*          dyStart;
        goPtrdiff_t*          dzStart;
        goByte*               px;
        goByte*               py;
        goByte*               pz;
        goIndex_t             posX;
        goIndex_t             posY;
        goIndex_t             posZ;
        goIndex_t             maxX;
        goIndex_t             maxY;
        goIndex_t             maxZ;
};

class goSignal3DGenericConstIterator
{
    public:
        goSignal3DGenericConstIterator  (const goSignal3DBase<void>* s);
        goSignal3DGenericConstIterator  (const goSignal3DGenericConstIterator& other);
        ~goSignal3DGenericConstIterator ();
        
        /**
         * @brief Sets the iterator position to x,y,z in the signal.
         *
         * @param x  X position
         * @param y  Y position
         * @param z  Z position
         **/
        inline void setPosition (goIndex_t x, goIndex_t y, goIndex_t z)
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
         * @return True if the end of the current x-line is reached, false otherwise.
         */
        inline bool     endX        ()
        {
            return posX > maxX;
        };
        /**
         * @return True if the end of the current y-plane is reached, false otherwise.
         */
        inline bool     endY        ()
        {
            return posY > maxY;
        };
        /**
         * @return True if the end of the current z-plane is reached (end of the 3D signal), false otherwise.
         */
        inline bool     endZ        ()
        {
            return posZ > maxZ;
        };
        inline void     incrementX  ()
        {
            px += *dx;
            ++posX;
            ++dx;
        };
        inline void     incrementY  ()
        {
            py += *dy;
            ++posY;
            ++dy;
        };
        inline void     incrementZ  ()
        {
            pz += *dz;
            ++posZ;
            ++dz;
        };
        inline void     decrementX  ()
        {
            px -= *(dx-1);
            --posX;
            --dx;
        };
        inline void     decrementY  ()
        {
            py -= *(dy-1);
            --posY;
            --dy;
        };
        inline void     decrementZ  ()
        {
            pz -= *(dz-1);
            --posZ;
            --dz;
        };
        /**
         * @brief Resets the X pointer and internals concerning X to the 
         * beginning of the current x-line.
         **/
        inline void     resetX      ()
        {
            dx   = dxStart;
            posX = 0;
            px   = py;
        };
        /**
         * @brief Resets the Y pointer and internals concerning Y to the 
         * beginning of the current z-plane.
         **/
        inline void     resetY      ()
        {
            dy   = dyStart;
            posY = 0;
            py   = pz;
            resetX ();
        };
        void     resetZ      ();
        /** 
         * @return Pointer to the data element at the current iterator position.
         */
        inline const goByte*  leftX    () const { return px - *(dx-1); }
        inline const goByte*  leftY    () const { return px - *(dy-1); }
        inline const goByte*  leftZ    () const { return px - *(dz-1); }
        inline const goByte*  rightX   () const { return px + *dx; }
        inline const goByte*  rightY   () const { return px + *dy; }
        inline const goByte*  rightZ   () const { return px + *dz; }
        inline const goByte* leftUp    () const { return px - *(dx-1) - *(dy-1); }  //= Works because dy at left is the same as at px.
        inline const goByte* leftDown  () const { return px - *(dx-1) + *dy; }  
        inline const goByte* rightUp   () const { return px + *dx - *(dy-1); }  
        inline const goByte* rightDown () const { return px + *dx + *dy; }  
        inline const goByte* operator* () const { return px; };
       
        const goSignal3DBase<void>* sig;
        const goPtrdiff_t*    dx;
        const goPtrdiff_t*    dy;
        const goPtrdiff_t*    dz;
        const goPtrdiff_t*    dxStart;
        const goPtrdiff_t*    dyStart;
        const goPtrdiff_t*    dzStart;
        const goByte*         px;
        const goByte*         py;
        const goByte*         pz;
        goIndex_t             posX;
        goIndex_t             posY;
        goIndex_t             posZ;
        goIndex_t             maxX;
        goIndex_t             maxY;
        goIndex_t             maxZ;
};
/** @} */
#endif
