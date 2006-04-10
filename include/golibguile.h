#ifndef GOLIBGUILE_H
#define GOLIBGUILE_H

#ifndef GOLIST_H
# include <golist.h>
#endif
#ifndef GOTYPES_H
# include <gotypes.h>
#endif
#ifndef GOARRAY_H
# include <goarray.h>
#endif
#ifndef GOFILEIO_H
# include <gofileio.h>
#endif

bool goReadImage (const char* filename, goSignal3D<void>* signal);
bool goWriteImage (const char* filename, const goSignal3DBase<void>* signal);

// SCM golib_test_list ();
// SCM golib_test_list_2 (SCM l);

template <class T>
SCM goRealListToSCM (const goList<T>& l);

template <class T>
bool goSCMToRealList (SCM l, goList<T>& gol);

template <class arrayT>
SCM goRealArrayToSCM (const arrayT& a);

template <class arrayT>
bool goSCMToRealArray (SCM l, arrayT& gov);

#endif
