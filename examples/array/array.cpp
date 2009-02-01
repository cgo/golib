#include <gofixedarray.h>
#include <stdio.h>

template<class T>
void print (goFixedArray<T>& a)
{
    for (int i = 0; i < (int)a.getSize(); ++i)
    {
        printf ("%d ", a[i]);
    }
    printf ("\n");
    printf ("   Reserved: %d\n   Overhead: %d\n", a.getReserved(), a.getResizeOverhead());
}

int main ()
{
    goFixedArray<goInt32> a (3,6,3);

    a.fill(1);
    a(0) = 1;
    a(1) = 2;
    a(2) = 3;

    print (a);

    for (int i = 4; i < 20; ++i)
    {
        a.resize (i);
        a[i-1] = i;
        print (a);
    }
    for (int i = a.getSize(); i > 1 ; --i)
    {
        a.resize (i);
        print (a);
    }

    exit(1);
}
