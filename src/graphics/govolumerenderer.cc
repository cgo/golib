#include <govolumerenderer.h>
#include <gotypes.h>

namespace Vol {

template <class T>
goVolumeRenderer<T>::goVolumeRenderer()
    : goStatusObject()
{
    setViewPlaneSampleDistance (1.0, 1.0);
    setSampleDistance (1.0f, 1.0f, 1.0f);
}

template <class T>
goVolumeRenderer<T>::~goVolumeRenderer()
{
}

template <class T>
void
goVolumeRenderer<T>::renderInit()
{
}

template <class T>
void
goVolumeRenderer<T>::render()
{
}

}

template class Vol::goVolumeRenderer<goInt8>;
template class Vol::goVolumeRenderer<goUInt8>;
template class Vol::goVolumeRenderer<goInt16>;
template class Vol::goVolumeRenderer<goUInt16>;
template class Vol::goVolumeRenderer<goInt32>;
template class Vol::goVolumeRenderer<goUInt32>;
template class Vol::goVolumeRenderer<goFloat>;
template class Vol::goVolumeRenderer<goDouble>;
