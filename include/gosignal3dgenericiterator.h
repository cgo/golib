/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 */

#ifndef GOSIGNAL3DGENERICITERATOR_H
#define GOSIGNAL3DGENERICITERATOR_H

#ifndef GOTYPES_H
# include <gotypes.h>
#endif

template<class T> class goSignal3DBase;

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
 * iterating through a signal. However, with <void> type signals, you should be ok with
 * this since the typical inner-loop methods are inlined. 
 * If you are optimising for speed, it may still be a good idea to simply write your own loop.
 * 
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
        
        void     setPosition (goIndex_t x, goIndex_t y, goIndex_t z);
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
        inline goByte*  operator*   () { return px; };
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
        
        void     setPosition (goIndex_t x, goIndex_t y, goIndex_t z);
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
        inline const goByte* operator*   ()  const { return px; };
       
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
