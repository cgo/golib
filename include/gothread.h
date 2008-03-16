/*
 * (C) Christian Gosch.
 * This file is licensed under the GNU General Public License Version 2.
 */

#ifndef GOTHREAD_H
#define GOTHREAD_H

#include <goconfig.h>

#ifdef HAVE_LIBPTHREAD
#include <pthread.h>
#include <semaphore.h>
#else
 #ifdef WIN32
 #include <windows.h>
 #include <process.h>
 #else
  #error "gothread.h: no thread model available"
 #endif
#endif  

typedef void    *(*goThread_exec_t)(void *);

/*!
 * \addtogroup thread
 * @{
 */
/*!
 * \brief A mutual exclusion class.
 *
 * @author Christian Gosch
 * @note Should work on both POSIX and win32. I recommend to always use POSIX where possible and to avoid 
 * non-POSIX thread implementations. However, If you feel like it you are most warmly welcome to 
 * implement a specialized Solaris threads version of goMutex, goThread and all related classes.
 */ 
class goMutex {
 public:
  goMutex ();
  virtual ~goMutex ();
  
  void lock ();
  void unlock ();
#ifdef HAVE_LIBPTHREAD
  pthread_mutex_t *getPthreadMutex () { return &mutex; }
#endif
 protected:
#ifdef HAVE_LIBPTHREAD
  pthread_mutex_t mutex;
#endif
#ifdef WIN32
  HANDLE mutex;
#endif
};

#ifdef HAVE_LIBPTHREAD
/** 
* @brief Condition variable.
* @author Christian Gosch
*/
class goCondition {
 public:
    goCondition();
    virtual ~goCondition();
    void signal();
    void broadcast();
    void wait();
 protected:
    pthread_cond_t cond;
    goMutex mutex;
};
#endif

#if defined HAVE_LIBPTHREAD || defined DOXYGEN
/** 
 * @brief Semaphore class.
 *
 * This class implements a semaphore.
 * A semaphore represents a value that can be incremented and decremented.
 * If the semaphore's value is 0 and decrement is called, the calling thread
 * is blocked until the semaphore is > 0, then the value is decremented.
 * Increments and decrements are atomic operations, i.e. they are thread-safe.
 *
 * @author Christian Gosch
 */
class goSemaphore {
 public:
    goSemaphore();
    virtual ~goSemaphore();
    /*!
     * Waits until semaphore is > 0, then decrements it atomically.
     */
    void dec();
    /*!
     * Increments the semaphore atomically.
     */ 
    void inc();
 protected:
    sem_t semaphore;
};
#else
#if defined WIN32
class goSemaphore {
 public:
    goSemaphore();
    virtual ~goSemaphore();
    /*!
     * Waits until semaphore is > 0, then decrements it atomically.
     */
    void dec();
    /*!
     * Increments the semaphore atomically.
     */ 
    void inc();
 protected:
    HANDLE semaphore;
};
#endif
#endif

/*!
 * Convenient frontend to the pthread standard.
 * @author Christian Gosch
 */
class goThread {
public:
  ///
  goThread ();
  ///
  virtual ~goThread ();

  /// Creates {\tt howMany} threads of {\function} with parameter {\tt param}.
#ifdef WIN32
  // void		create (void (__cdecl *function) (void*), void* param, int howMany);
  void		create(void (*function) (void*), void* param, int howMany);
#else
  void		create(void* (*function) (void*), void* param, int howMany);
#endif
  /// Joins threads (blocks caller).
  void		join ();
#ifndef WIN32  
  ///	
  void		suspend();
  ///
  void		resume();
  ///
  void		kill();
  ///
  void		cancel();
#endif

  /**
   * @returns A unique integer, starting with 0. This can be used to determine a
   * thread ID.
   */
  int		makeInt ();
  /// Resets the ints made by {\tt makeInt()} to zero.
  void		resetInt ();

  void      getIndexRange (int threadNumber, int N, int& myStart, int& myEnd);

  ///
  int		getNumOfThreads () { return numberOfThreads; }
  
  /**
   * @returns Number of processors online in this host.
   */
  static int	howManyProcessors ();
#ifdef HAVE_LIBPTHREAD
  static void	checkCancel () { pthread_testcancel(); }
  static void	setCancelType (int t)
      {
	  pthread_setcancelstate (PTHREAD_CANCEL_ENABLE, NULL);
	  switch (t)
	      {
	      case 0: pthread_setcanceltype (PTHREAD_CANCEL_ASYNCHRONOUS, NULL); break;
	      case 1: pthread_setcanceltype (PTHREAD_CANCEL_DEFERRED, NULL); 
		  			// this is default in pthreads break;
	      }
      }
#endif

  bool isCurrentThread (int threadNumber = 0) const;
  
protected:
  void*		threadFunction;
  void*		threadParam;
#ifdef HAVE_LIBPTHREAD
  pthread_attr_t threadAttr;
  pthread_t	*threads;
#endif
#ifdef WIN32
  unsigned long  *threads;
#endif
  int		numberOfThreads;

  goMutex	intMutex;
  int			intCount;
};
/*! @} */

#endif
