#include <gomath.h>
#include <gomatrix.h>
#include <golapack.h>

int main ()
{
    goFloat a[] = {1.0, 2.0, 3.0,
                   10.0, 5.0, 1.0,
                   7.0, 8.0, 9.0};
    goMatrixf A;
    A.setData (a, 3, 3);
    goFloat b[] = {1.0, 2.0, 3.0,
                   2.0, 3.0, 4.0};
    goMatrixf B;
    B.setData (b, 2, 3);

    printf ("B:\n");
    B.print();
    printf ("A:\n");
    A.print();

    A.invert();
    A.invert();
    A.print();

    goVector<int> ipiv;
    goMath::Lapack::getrf (A, ipiv);
    goMath::Lapack::getrs (A, false, B, ipiv);

    B.print ();
}
