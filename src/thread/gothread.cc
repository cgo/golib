#include <goconfig.h>
#include <goerror.h>
#include <gothread.h>
#ifndef WIN32
#include <signal.h>
#include <unistd.h>
#endif

#include <assert.h>

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
  for (i = 0; i < numberOfThreads; i++) {
    pthread_join (threads[i], NULL);
  }
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
  pthread_mutex_lock (&mutex);
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
	    goError::print("goCondition::~goCondition()","condition variable seems to be busy");
	}
}

void
goCondition::signal ()
{
    mutex.lock();
    pthread_cond_signal (&cond);
    mutex.unlock();
}

void
goCondition::broadcast ()
{
    mutex.lock();    
    pthread_cond_broadcast (&cond);
    mutex.unlock();
}

void
goCondition::wait ()
{
    mutex.lock();
    pthread_cond_wait (&cond, mutex.getPthreadMutex());
    mutex.unlock();
}

goSemaphore::goSemaphore()
{
    sem_init (&semaphore, 0, 0);     // Make a non-shared semaphore with initial value = 0
}

goSemaphore::~goSemaphore()
{
    sem_destroy (&semaphore);
}

void
goSemaphore::dec ()
{
    // cout << "Calling sem_wait" << endl;
    sem_wait (&semaphore);
}

void
goSemaphore::inc ()
{
    // cout << "Calling sem_post" << endl;
    sem_post (&semaphore);
}

#endif




