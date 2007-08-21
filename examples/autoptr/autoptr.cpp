#include <goautoptr.h>
#include <stdio.h>

int main ()
{
    goAutoPtr<int> a = goAutoPtr<int> (new int(5));

    printf ("Size of goAutoPtr<int>: %d\n", sizeof(a));
    printf ("Size of respective goRRefPtr: %d\n", sizeof(*a.getRRefPtr()));

    goAutoPtr<int> b;
    {
        goAutoPtr<int> a = goAutoPtr<int> (new int(5));
        b = a;
    }

    printf("%d\n", *b);

    exit (1);
}
