#include <libguile.h>
#include <golist.h>
#include <govector.h>
#include <golibguile.h>

bool goReadImage (const char* filename, goSignal3D<void>* signal) 
{
    if (!signal)
        return false;
    if (!filename)
        return false;
    bool ok = true;
    try
    {
        ok = goFileIO::readImage (filename, signal);
    }
    catch (goException)
    {
        throw;
        return false;
    }
    return ok;
}

bool goWriteImage (const char* filename, const goSignal3DBase<void>* signal)
{
    if (!filename || !signal)
        return false;
    bool ok = true;
    try
    {
        ok = goFileIO::writeImage (filename, signal);
    }
    catch (goException)
    {
        throw;
        return false;
    }
    return ok;
}

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

template <class arrayT>
SCM goRealArrayToSCM (const arrayT& a)
{
    SCM ret = SCM_EOL;
    if (a.getSize() == 0)
    {
        return SCM_EOL;
    }
    goIndex_t sz = a.getSize();
    goIndex_t i = 0;
    for (i = sz-1; i >= 0; --i)
    {
        ret = scm_cons (scm_make_real(a[i]), ret);
    }
    return ret;
    // SCM scm_list_1 (SCM e1);
    // double scm_num2dbl (SCM d, const char* why);
    // SCM scm_make_real (double x);
    // SCM scm_cons (SCM CAR, SCM CDR);
}

template <class arrayT>
bool goSCMToRealArray (SCM l, arrayT& gov)
{
    if (SCM_NULLP(l))
    {
        return true;
    }
    SCM_ASSERT (SCM_CONSP(l), l, SCM_ARG1, "goSCMToRealArray");
    gov.resize (scm_num2int(scm_length(l), SCM_ARG1, "goSCMToRealArray"));

    goIndex_t i = 0;
    while (!SCM_NULLP(l))
    {
        SCM_ASSERT(SCM_NUMBERP(SCM_CAR(l)), SCM_CAR(l), SCM_ARG1, "goSCMToRealArray");
        gov[i] = scm_num2dbl(SCM_CAR(l), "goSCMToRealArray");
        ++i;
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
template SCM goRealArrayToSCM (const goArray<goFloat>& l);
template SCM goRealArrayToSCM (const goArray<goDouble>& l);
template bool goSCMToRealArray (SCM l, goArray<goFloat>& l2);
template bool goSCMToRealArray (SCM l, goArray<goDouble>& l2);
template SCM goRealArrayToSCM (const goVector<goFloat>& l);
template SCM goRealArrayToSCM (const goVector<goDouble>& l);
template bool goSCMToRealArray (SCM l, goVector<goFloat>& l2);
template bool goSCMToRealArray (SCM l, goVector<goDouble>& l2);
