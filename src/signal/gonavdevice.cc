/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gonavdevice.h>

goNavDevice::goNavDevice()
{
	navSlot = 0;	
}

goNavDevice::~goNavDevice()
{
	
}

void
goNavDevice::connect(goNavSlot* s)
{
	navSlot = s;
}

void
goNavDevice::navDeviceUpdate()
{
}

goNavSlot::goNavSlot()
{
}

goNavSlot::~goNavSlot()
{
}

void
goNavSlot::motion(goNavSlot::motionType t, void* arg)
{
}
