#include <libguile.h>
#include <golist.h>
#include <golibguile.h>

template <class T>
SCM goRealListToSCM (const goList<T>& l)
{
    SCM ret = SCM_EOL;
    if (l.isEmpty())
    {
        return SCM_EOL;
    }
    
    typename goList<T>::ConstElement* el = l.getTailElement();
    while (el)
    {
        ret = scm_cons (scm_make_real(el->elem), ret);
        el = el->prev;
    }
    return ret;
    // SCM scm_list_1 (SCM e1);
    // double scm_num2dbl (SCM d, const char* why);
    // SCM scm_make_real (double x);
    // SCM scm_cons (SCM CAR, SCM CDR);
}

template <class T>
bool goSCMToRealList (SCM l, goList<T>& gol)
{
    if (SCM_NULLP(l))
    {
        return true;
    }
    SCM_ASSERT (SCM_CONSP(l), l, SCM_ARG1, "goSCMToRealList");
    gol.erase();

    while (!SCM_NULLP(l))
    {
        SCM_ASSERT(SCM_NUMBERP(SCM_CAR(l)), SCM_CAR(l), SCM_ARG1, "goSCMToRealList");
        gol.append (scm_num2dbl(SCM_CAR(l), "goSCMToRealList"));
        l = SCM_CDR(l);
    }
    return true;
}

/*
SCM golib_test_list ()
{
    goList<goDouble> l;
    l.append (2.5);
    l.append (3.5);
    l.append (4.5);
    l.append (5.5);

    return goRealListToSCM (l);
}

SCM golib_test_list_2 (SCM l)
{
    goList<goDouble> gol;
    goSCMToRealList (l, gol);
    
    goList<goDouble>::Element* el = gol.getFrontElement();
    while (el)
    {
        printf ("List Element: %f\n", el->elem);
        el = el->next;
    }
    return SCM_BOOL_T;
}
*/

template SCM goRealListToSCM (const goList<goFloat>& l);
template SCM goRealListToSCM (const goList<goDouble>& l);
template bool goSCMToRealList (SCM l, goList<goFloat>& l2);
template bool goSCMToRealList (SCM l, goList<goDouble>& l2);
