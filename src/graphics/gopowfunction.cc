#include <goarray.h>
#include <gopowfunction.h>
#include <math.h>

goPowFunction::goPowFunction()
{
}

goPowFunction::~goPowFunction()
{
	table.resize(0);
}

void
goPowFunction::init(int order,goSize_t _samples_log)
{
	samples_log = _samples_log;
	table.resize((1 << samples_log) + 1);
	goSize_t i;
	goDouble diff = 1 / (float)(1 << samples_log);
	goDouble v = 0.0f;
	goDouble p = 1 << order;
	for(i = 0; i < table.getSize(); i++)
	{
		table[i] = pow(v,p);
		v += diff;
	}
}

goDouble
goPowFunction::operator[] (goDouble v)
{
	goDouble float_index = v * (table.getSize() - 1);
	goSize_t i = (goSize_t)(float_index);
	//First try: just take the next lower
	return table[i];
}
