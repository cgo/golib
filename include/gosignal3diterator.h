/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 */

#ifndef GOSIGNAL3DITERATOR_H
#define GOSIGNAL3DITERATOR_H

#ifndef GOTYPES_H
# include <gotypes.h>
#endif

template<class T> class goSignal3DBase;

/** 
 * @addtogroup signal
 */
/** @{ */
/**
 * @brief  Iterator for goSignal3DBase<T> class signals.
 *
 * This is an iterator class for signals. Please refrain from using
 * special type goSignal3DBase if you don't absolutely have to (I don't see why).
 * Use goSignal3DBase<void> and goSignal3DGenericIterator instead or code your
 * own loops.
 *
 * Usage is like this:
 *
 * <pre>
 * <code>
 *  goSignal3D<goInt16> sig;
 *  ....
 *  goSignal3D<goInt16>::iterator it(&sig);
 *  while (!it.endZ())
 *  {
 *      it.resetY();
 *      while (!it.endY())
 *      {
 *          it.resetX();
 *          while (!it.endX())
 *          {
 *              **it = // ... do something
 *              it.incrementX();
 *          }
 *          it.incrementY();
 *      }
 *      it.incrementZ();
 *  }
 * </code>
 * </pre>
 * @note It is faster to use the GO_SIGNAL3D_EACHELEMENT* macros for 
 * iterating through a signal, since there are function calls
 * in the inner loop when you are using the iterator. 
 * However, with <void> type signals, you should be ok with
 * goSignal3DGenericIterator since the typical inner-loop methods are inlined. 
 * If you are optimising for speed, it may still be a good idea to simply write your own loop.
 * 
 * @todo This needs some testing for functionality and <b>speed</b>
 **/
template <class T>
class goSignal3DIterator
{
    public:
        goSignal3DIterator  (goSignal3DBase<T>* s);
        goSignal3DIterator  (const goSignal3DIterator& other);
        ~goSignal3DIterator ();
        
        void     setPosition (goIndex_t x, goIndex_t y, goIndex_t z);
        bool     endX        ();
        bool     endY        ();
        bool     endZ        ();
        void     incrementX  ();
        void     incrementY  ();
        void     incrementZ  ();
        void     decrementX  ();
        void     decrementY  ();
        void     decrementZ  ();
        void     resetX      ();
        void     resetY      ();
        void     resetZ      ();
        T*       operator*   ();
        const T* operator*   () const;
       
        goSignal3DBase<T>* sig;
        goPtrdiff_t*       dx;
        goPtrdiff_t*       dy;
        goPtrdiff_t*       dz;
        T*                 px;
        T*                 py;
        T*                 pz;
        goIndex_t          posX;
        goIndex_t          posY;
        goIndex_t          posZ;
        goIndex_t          maxX;
        goIndex_t          maxY;
        goIndex_t          maxZ;
};
/** @} */
#endif

