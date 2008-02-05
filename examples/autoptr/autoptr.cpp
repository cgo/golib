#include <goautoptr.h>
#include <stdio.h>

#include <gomatrix.h>

        class MM : public goMatrix<goFloat>
        {
            public:
                MM () : goMatrix<goFloat> (4,4),
                    mySponkiness (false)
                    { };
                virtual ~MM () { printf ("~MM\n"); };

                bool sponk () { return mySponkiness; };

            protected:
                bool mySponkiness;
        };

int main ()
{
    goAutoPtr<int> a = goAutoPtr<int> (new int(5));

    printf ("Size of goAutoPtr<int>: %d\n", sizeof(a));
    printf ("Size of respective goRRefPtr: %d\n", sizeof(*a.getRRefPtr()));

    {
        goAutoPtr<int> b;
        {
            goAutoPtr<int> a = goAutoPtr<int> (new int(5));
            b = a;
        }
        printf("%d\n", *b);

        goAutoPtr<unsigned int> c = b;
    }
    {
        goAutoPtr<goMatrixf> b;
        {
            goAutoPtr<MM> a = goAutoPtr<MM> (new MM);
            a->print();
            b = a;
        }
        b->print ();
    }

    exit (1);
}
