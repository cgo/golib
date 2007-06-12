#include <gofixedarray.h>
#include <stdio.h>

int main ()
{
    goFixedArray<goInt32> a (3,2,2);

    a.fill(1);
    a(-2) = -2;
    a(-1) = -1;
    a(3) = 3;
    a(4) = 4;

    for (goIndex_t i = 0; i < 3+4; ++i)
    {
        printf ("a(%d) = %d\n", i-2,a(i-2));
    }
    exit(1);
}
