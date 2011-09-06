/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOPRODUCER_H
#define GOPRODUCER_H

#include <gothread.h>

/*
 * \addtogroup thread
 * @{
 */
/*
 * Simple producer object.
 * In this implementation, each consumer can only be connected to one producer.
 *
 * \todo Not thoroughly tested. Report bugs.
 * 
 * @author Christian Gosch
 * @date 22.9.2001
 * @see goConsumer
 */ 
class
goProducer
{
 public:
    goProducer();
    virtual ~goProducer();

    /*
     * Internally, increments the object's semaphore variable to notify threads waiting for data that data is available.
     * Each consumer waiting for data decrements the semaphore atomically before doing something with any data
     * or waits until the semaphore becomes larger than zero and then decrements it.
     */
    void signalProduction();
    
    /*
     * @return A pointer to the goSemaphore object used as semaphore variable. This is used by
     * goConsumer to "connect" a consumer to a producer.
     * @see goConsumer::setProducer()
     */
    goSemaphore* getSemaphore() { return &semaphore; }
 private:
    goSemaphore semaphore;
};
/* @} */
#endif
