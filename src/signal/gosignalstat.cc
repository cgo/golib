#include <gosignalstat.h>
#include <gosignalmacros.h>
#include <math.h>

template<class T>
goSignalStat<T>::goSignalStat ()
{
    mean	= 0;
    deviation	= 0.0f;
    energy	= 0.0f;
}

template<class T>
goSignalStat<T>::~goSignalStat()
{
}

#define GO_SST_MEAN() { \
	temp = 1 / n;	\
	m = (1 - temp) * m + temp * *__ptr; \
	n += 1; \
}

template<class T>
goDouble
goSignalStat<T>::getMean (goSignal3D<T>& signal)
{
	goDouble temp;
	goDouble m = 0;
	goFloat n = 1;
    GO_SIGNAL3D_EACHELEMENT((GO_SST_MEAN()), signal, T);
	mean = m;
	return m;
	
	/*
    goDouble factor = 1 / (goDouble)(signal.getSizeX() * signal.getSizeY() * signal.getSizeZ());
    goDouble temp = 0.0f;
    GO_SIGNAL3D_EACHELEMENT((temp += *__ptr * factor), signal, T);
    mean = temp;
    return temp;*/
}


template<class T>
goDouble
goSignalStat<T>::getMeanFast (goSignal3D<T>& signal)
{
    goDouble factor = 1 / (goDouble)(signal.getSizeX() * signal.getSizeY() * signal.getSizeZ());
    goDouble temp = 0.0f;
    GO_SIGNAL3D_EACHELEMENT((temp += *__ptr * factor), signal, T);
    mean = temp;
    return temp;
}

template<class T>
goDouble
goSignalStat<T>::getDeviation (goSignal3D<T>& signal, goDouble __mean)
{
    goDouble factor = 1 / (goDouble)(signal.getSizeX() * signal.getSizeY() * signal.getSizeZ() - 1);
    goDouble temp = 0.0f;
    GO_SIGNAL3D_EACHELEMENT((temp += (*__ptr - __mean) * (*__ptr - __mean) * factor), signal, T);
    temp = sqrt(temp);
    deviation = temp;
    return temp;
}

template<class T>
goDouble
goSignalStat<T>::getEnergy (goSignal3D<T>& signal)
{
    goDouble temp = 0.0f;
    GO_SIGNAL3D_EACHELEMENT((temp += *__ptr * *__ptr), signal, T);
    energy = temp;
    return temp;
}



template class goSignalStat<goInt8>;
template class goSignalStat<goUInt8>;
template class goSignalStat<goInt16>;
template class goSignalStat<goUInt16>;
template class goSignalStat<goInt32>;
template class goSignalStat<goUInt32>;
template class goSignalStat<goFloat>;
template class goSignalStat<goDouble>;
