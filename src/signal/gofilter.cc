#include <gofilter.h>

template <class T>
goFilter<T>::goFilter () {
}

template <class T>
goFilter<T>::~goFilter () {
}

template <class T>
void
goFilter<T>::filter () {
}

template <class T>
void 
goFilter<T>::filter (goSignal2D<T>& s) {
}

template class goFilter<goDouble>;
template class goFilter<goFloat>;
template class goFilter<goInt32>;
// template class goFilter<double>;
// template class goFilter<int>;
