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
