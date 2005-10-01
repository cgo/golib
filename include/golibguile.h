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

// SCM golib_test_list ();
// SCM golib_test_list_2 (SCM l);

template <class T>
SCM goRealListToSCM (const goList<T>& l);

template <class T>
bool goSCMToRealList (SCM l, goList<T>& gol);

template <class T>
SCM goRealArrayToSCM (const goArray<T>& a);

template <class T>
bool goSCMToRealArray (SCM l, goArray<T>& gov);

#endif
