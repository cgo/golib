#ifndef GOTHREADOBJECT_H
#define GOTHREADOBJECT_H

#include <gotypes.h>
#include <goobjectbase.h>

class goThread;

/*!
 * \addtogroup thread
 * @{
 */
/*!
 * \brief Base class for objects that implement threads.
 *
 * Reimplement method threadMethod() in derived classes.
 * threadMethod() will be run in an extra thread when run() is called.
 *
 * @date 7.1.2002
 * @author Christian Gosch
 */
class goThreadObject : public goObjectBase
{
 public:
    goThreadObject();
    virtual ~goThreadObject();
    
    /*!
     * Creates a new thread of execution. The thread function is threadMethod().
     * @param nt Number of threads to spawn. 1 by default. If this differs
     * from 1, then threadMethod() is executed in nt threads at the same
     * time.
     */
    virtual void run(int nt = 1);
    /*!
     * This method must be reimplemented by a subclass. This method
     * contains the code of the new thread.
     */
    virtual void threadMethod();
    /*!
     * @return A reference to the goThread object used to run the thread(s).
     */
    goThread& getThread() { return thisThread; }
 private:
    goThread thisThread;
    
};
/*! @} */
#endif
