#ifndef GOLIBGUILE_H
#define GOLIBGUILE_H

#ifndef GOLIST_H
# include <golist.h>
#endif
#ifndef GOTYPES_H
# include <gotypes.h>
#endif

// SCM golib_test_list ();
// SCM golib_test_list_2 (SCM l);

template <class T>
SCM goRealListToSCM (const goList<T>& l);

template <class T>
bool goSCMToRealList (SCM l, goList<T>& gol);

#endif
