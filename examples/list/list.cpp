/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <golist.h>
#include <govector.h>
#include <gosort.h>

#include <list>

int main ()
{
    goList<int> li;
    li.append (1);
    li.append (3);
    li.append (0);
    li.append (5);
    li.append (4);

    {
        std::list<int> std_li;
        std_li.push_back (1);
        std_li.push_back (2);
        std_li.push_back (3);
        goList<int> li (std_li);

        goList<int>::iterator it = li.begin ();
        goList<int>::iterator itend = li.end ();
        while (it != itend)
        {
            printf ("%d ", *it);
            ++it;
        }
        printf ("\n");
    }
    {
        std::list<int> l = li;
        std::list<int>::iterator it = l.begin ();
        while (it != l.end ())
        {
            printf ("%d ", *it);
            ++it;
        }
        exit (1);
    }

    goList<int>::iterator a = li.begin ();
    goList<int>::iterator b (li.getTailElement());
    goQuickSort<goList<int>, int> (a, b);

    const goList<int>* lp = &li;
    goList<int>::const_iterator it = lp->begin ();
    goList<int>::const_iterator itend = lp->end ();
    while (it != itend)
    {
        printf ("%d ", *it);
        ++it;
    }
    exit (1);

    goList<goVectorf> l;

    while (true)
    {
        for (goSize_t i = 0; i < 100; ++i)
        {
            l.append (goVectorf (650));
        }
        printf ("%d\n", l.getSize());
    }
}
