/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOSYNCHRONIZED_H
#define GOSYNCHRONIZED_H

#include <gothread.h>
#include <goarray.h>

/*!
 * Synchronizing concurrent objects.
 * This class is using a condition variable to synchronize with other goSynchronized objects.
 * Do not use this for consumer/producer problems, rather use it for making one object to wait for another if
 * you know exactly that it has to wait.
 * @author Christian Gosch
 * @date 21.9.2001
 * @see goConsumer
 * @see goProducer
 */ 
class
goSynchronized
{
 public:
    goSynchronized();
    virtual ~goSynchronized();

    /*!
     * @param s Object using waitSynchronize() to wait for a sync signal.
     */
    void addSynchronizedListener(goSynchronized* s);

    /*!
     * Signal all listening objects the sync event.
     */
    void signalSynchronize();

    goCondition& getSynchronizeCondition () { return condition; }

 protected:
    /*!
     * Wait until a sync is signalled.
     */
    void waitSynchronize();

 private:
    goCondition condition;
    /*
     * Pointers to goSynchronized objects that are using waitSynchronize() to wait for a sync signal.
     */
    goArray<void*> listeners;
};

#endif
