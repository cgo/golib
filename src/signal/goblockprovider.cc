#include <goblockprovider.h>
#include <gotypes.h>

namespace Vol {

template <class T>
goBlockProvider<T>::goBlockProvider()
{
}

template <class T>
goBlockProvider<T>::~goBlockProvider()
{
}

template <class T>
int
goBlockProvider<T>::getStages()
{
  return 0;
}

template <class T>
goSignal3D<T>*
goBlockProvider<T>::getBlock(goSize_t nr)
{
	cout << "goBlockProvider::getBlock()" << endl;
  return 0;
}

template <class T>
void
goBlockProvider<T>::releaseBlock (goSize_t)
{

}

template <class T>
goSize3D&
goBlockProvider<T>::getBlockSize() 
{
  return dummy;
}

}

template class Vol::goBlockProvider<goInt8>;
template class Vol::goBlockProvider<goUInt8>;
template class Vol::goBlockProvider<goInt16>;
template class Vol::goBlockProvider<goUInt16>;
template class Vol::goBlockProvider<goInt32>;
template class Vol::goBlockProvider<goUInt32>;
template class Vol::goBlockProvider<goFloat>;
template class Vol::goBlockProvider<goDouble>;
