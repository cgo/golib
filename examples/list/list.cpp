#include <golist.h>
#include <govector.h>
#include <gosort.h>

int main ()
{
    goList<int> li;
    li.append (1);
    li.append (3);
    li.append (0);
    li.append (5);
    li.append (4);

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
