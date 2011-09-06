/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOCONSUMER_H
#define GOCONSUMER_H

#include <gothread.h>
#include <goproducer.h>

/*
 * \addtogroup thread
 * @{
 */
/*
 * Simple consumer object.
 * In this implementation, each consumer can only be connected to one producer.
 *
 * \todo When using this, report bugs! Not thoroughly tested.
 * 
 * @author Christian Gosch
 * @date 22.9.2001
 * @see goProducer
 */ 
class
goConsumer
{
 public:
    goConsumer();
    virtual ~goConsumer();

	/*
	 * Waits for an internal semaphore and returns as soon as it was incremented by 
	 * another thread (a producer). The semaphore is decremented.
	 * @note If using pthreads (which should normally be true), this is a cancellation point.
	 */
    void waitProduction();

	/*
	 * Sets the internal semaphore to the semaphore provided by the given producer.
	 * @attention If the producer gets deleted, the semaphore is no longer valid.
	 * This is a potential source of faults, so make sure no consumer is using any
	 * producer that gets killed before the consumer.
	 */
    void setProducer (goProducer& p) { semaphore = p.getSemaphore(); }
 private:
    goSemaphore *semaphore;
};
/* @} */
#endif
