#ifndef GOPOWFUNCTION_H
#define GOPOWFUNCTION_H

#include <gotypes.h>
template<class T> class goArray;

class goPowFunction
{
	public:
		goPowFunction();
		virtual ~goPowFunction();
	
		void init(int order,goSize_t _samples_log);
		goDouble operator[] (goDouble);
		
	protected:
		goSize_t samples_log;
	private:
		goArray<goDouble> table;
};

#endif
