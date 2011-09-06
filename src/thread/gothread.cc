/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goconfig.h>
#include <gothread.h>
#ifndef WIN32
#include <signal.h>
#include <unistd.h>
#endif
#include <golog.h>

#include <assert.h>

#ifdef GO_GUILE_MULTITHREAD
# include <libguile/threads.h>
#endif

goThread::goThread () {
  intCount		= 0;
  numberOfThreads	= 0;
  threads		= 0;
#ifdef HAVE_LIBPTHREAD
  // pthread_mutex_init (&intMutex, NULL);
  pthread_attr_init (&threadAttr);
  pthread_attr_setscope (&threadAttr, PTHREAD_SCOPE_SYSTEM);
#else
#ifdef WIN32
  
#endif
#endif
}

goThread::~goThread () {
}

#ifdef HAVE_LIBPTHREAD
void
goThread::create (void* (*function) (void*), void* param, int howMany) {
  int i = 0;
  numberOfThreads = howMany;
  if (threads) delete threads;
  threads = new pthread_t[howMany];
  int pthreadret = 0;
  for (i = 0; i < howMany; i++) {
    pthreadret = pthread_create (&threads[i], &threadAttr, function, param);
	assert(pthreadret == 0);
  }
}
#endif
#ifdef WIN32
void
//goThread::create (void (__cdecl *function) (void*), void* param, int howMany) {
goThread::create (void (*function) (void*), void* param, int howMany) {
  int i = 0;
  numberOfThreads = howMany;
  if (threads) delete threads;
  threads = new unsigned long[howMany];
  for (i = 0; i < howMany; i++) {
    threads[i] = _beginthread (function, 0, param);
  }
}
#endif

#ifdef HAVE_LIBPTHREAD
void
goThread::join () {
  int i = 0;
#ifdef GO_GUILE_MULTITHREAD
  scm_t_guile_ticket ticket = scm_leave_guile ();
  for (i = 0; i < numberOfThreads; i++) 
  {
    pthread_join (threads[i], NULL);
  }
  scm_enter_guile (ticket);
#else
  for (i = 0; i < numberOfThreads; i++) 
  {
    pthread_join (threads[i], NULL);
  }
#endif
}
#else
#ifdef WIN32
void
goThread::join () {
  int i = 0;
  for (i = 0; i < numberOfThreads; i++) {
    WaitForSingleObject ((HANDLE)threads[i], INFINITE);
  }
}
#endif
#endif

#ifdef HAVE_LIBPTHREAD
void
goThread::suspend () {
  for (int i = 0; i < numberOfThreads; i++) {
      pthread_kill (threads[i], SIGSTOP);
  }					      
}
#endif

#ifdef HAVE_LIBPTHREAD
void
goThread::resume () {
  for (int i = 0; i < numberOfThreads; i++) {
    pthread_kill (threads[i], SIGCONT);
  }					      
}
#endif

#ifdef HAVE_LIBPTHREAD
void
goThread::kill () {
  for (int i = 0; i < numberOfThreads; i++) {
    pthread_kill (threads[i], SIGKILL);
  }					      
}
#endif

#ifdef HAVE_LIBPTHREAD
void
goThread::cancel() {
  for (int i = 0; i < numberOfThreads; i++) {
      pthread_cancel (threads[i]);
  }					      
}
#endif

int
goThread::makeInt () {
  int retVal;
  intMutex.lock();
  retVal = intCount++;
  intMutex.unlock();
  return retVal;
}

void
goThread::resetInt () {
  intCount = 0;
}

/** 
 * @brief Calculate a mutually exclusive index range for this thread.
 * 
 * Given you are working on a data set of N-1 data of index range 0..N-1, this
 * method calculates a range startRet..endRet that belongs to this thread.
 * You can calculate a thread number by using makeInt(); remember to
 * reset the values using resetInt() if using a thread object multiple times.
 *
 * Notice \c endRet and \c startRet are used as in
 * <code>for (i = startRet; i < endRet; ++i) {...}<\code>.
 * \c startRet is the first index, \c endRet is the last index plus one.
 *
 * @param threadNumber Number of this thread (e.g. with makeInt())
 * @param N Number of data items
 * @param startRet Start index (on return)
 * @param endRet End index (on return)
 */
void goThread::getIndexRange (int threadNumber, int N, int& startRet, int& endRet)
{
    // const goSize_t N = this->null_basis->getColumns();
    int myStart = 0;
    int myN = 0;
    int count = this->getNumOfThreads();
    //printf ("myID: %d started.\n", myID);
    if (threadNumber < count - 1) //= not the last thread
    {
        myN = N / count;
        myStart = threadNumber * myN;
    }
    else
    {
        myStart = threadNumber * (N / count);
        myN = N - myStart;
    }

    myN += myStart;
    startRet = myStart;
    endRet = myN;
}

#ifndef WIN32
int
goThread::howManyProcessors () {
  int retVal;
#ifdef _SC_NPROCESSORS_ONLN
  retVal = (int)(sysconf (_SC_NPROCESSORS_ONLN));
#else
  retVal = (int)(sysconf (_SC_NPROC_ONLN));
#endif
  return retVal;
}
#endif

#ifdef WIN32
bool goThread::isCurrentThread (int threadNumber) const
{
    goLog::warning ("goThread::isCurrentThread() is not implemented for Windows.");
    return false;
}
#else
//= Assume pthread model
/** --------------------------------------------------------------------------
 * @brief Check if the thread currently in control is the given one.
 * 
 * @param threadNumber  Number of the thread. Default is 0 (the first one of this goThread object).
 * 
 * @return  True if the current thread is the one asked for, false otherwise.
 ----------------------------------------------------------------------------*/
bool goThread::isCurrentThread (int threadNumber) const
{
    if (threadNumber >= this->numberOfThreads || threadNumber < 0)
        return false;
    pthread_t self = pthread_self();
    if (pthread_equal(self,this->threads[threadNumber]) != 0)
        return true;
    else
        return false;
}
#endif

goMutex::goMutex () {
#ifdef HAVE_LIBPTHREAD  
  pthread_mutex_init (&mutex, NULL);
#else
#ifdef WIN32
  mutex = CreateMutex (NULL, FALSE, NULL);
#endif
#endif
}

goMutex::~goMutex () {
}

void
goMutex::lock () {
#ifdef HAVE_LIBPTHREAD
# ifdef GO_GUILE_MULTITHREAD
    scm_pthread_mutex_lock (&mutex);
# else
    pthread_mutex_lock (&mutex);
# endif
#else
#ifdef WIN32
  WaitForSingleObject (mutex,INFINITE);
#endif
#endif
}

void
goMutex::unlock () {
#ifdef HAVE_LIBPTHREAD
  pthread_mutex_unlock (&mutex);
#else
#ifdef WIN32
  ReleaseMutex (mutex);
#endif
#endif
}


#ifdef HAVE_LIBPTHREAD
goCondition::goCondition()
{
    pthread_cond_init (&cond, NULL);
}

goCondition::~goCondition ()
{
    if (pthread_cond_destroy (&cond) != 0)
	{
        goLog::warning("goCondition::~goCondition(): condition variable seems to be busy");
	}
}

/** 
 * @brief Signals the condition is met.
 * Signalling the condition is met restarts exactly one waiting thread,
 * if there is any. Use broadcast to restart all waiting threads.
 */
void
goCondition::signal ()
{
    mutex.lock();
    pthread_cond_signal (&cond);
    mutex.unlock();
}

/** 
 * @brief Broadcast the condition is met to all waiting threads.
 * Restarts all waiting threads, if any.
 */
void
goCondition::broadcast ()
{
    mutex.lock();    
    pthread_cond_broadcast (&cond);
    mutex.unlock();
}

/** 
 * @brief Wait for the condition to be met.
 * The calling thread waits until signal() chooses it to
 * be continued or until broadcast() is called.
 */
void
goCondition::wait ()
{
    mutex.lock();
#ifdef GO_GUILE_MULTITHREAD
    scm_pthread_cond_wait (&cond, mutex.getPthreadMutex());
#else
    pthread_cond_wait (&cond, mutex.getPthreadMutex());
#endif
    mutex.unlock();
}
#endif

#if defined HAVE_LIBPTHREAD
goSemaphore::goSemaphore()
{
    sem_init (&semaphore, 0, 0);     // Make a non-shared semaphore with initial value = 0
}

goSemaphore::~goSemaphore()
{
    sem_destroy (&semaphore);
}

/** 
 * @brief Decrement semaphore.
 * Waits until the semaphore is > 0 and atomically decrements it by one.
 */
void
goSemaphore::dec ()
{
    // cout << "Calling sem_wait" << endl;
#ifdef GO_GUILE_MULTITHREAD
    scm_t_guile_ticket ticket = scm_leave_guile();
    sem_wait (&semaphore);
    scm_enter_guile (ticket);
#else
    sem_wait (&semaphore);
#endif
}

/** 
 * @brief Atomically increment the semaphore.
 */
void
goSemaphore::inc ()
{
    // cout << "Calling sem_post" << endl;
    sem_post (&semaphore);
}
#elif defined WIN32
goSemaphore::goSemaphore()
{
    this->semaphore = CreateSemaphore (NULL, 0, std::numeric_limits<LONG>::max(), NULL);
    // sem_init (&semaphore, 0, 0);     // Make a non-shared semaphore with initial value = 0
}

goSemaphore::~goSemaphore()
{
    // sem_destroy (&semaphore);
}

/** 
 * @brief Decrement semaphore.
 * Waits until the semaphore is > 0 and atomically decrements it by one.
 */
void
goSemaphore::dec ()
{
    WaitForSingleObject (this->semaphore, INFINITE);
}

/** 
 * @brief Atomically increment the semaphore.
 */
void
goSemaphore::inc ()
{
    ReleaseSemaphore (this->semaphore, 1, NULL);
}
#endif
