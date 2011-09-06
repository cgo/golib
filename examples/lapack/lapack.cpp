/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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

    goMatrixf A2(A);

    goVector<int> ipiv;
    goMath::Lapack::getrf (A, ipiv);
    goMath::Lapack::getrs (A, false, B, ipiv);
    B.print ();

#if 0
    {
        printf ("Testing gels()\n");
        printf ("A2:\n");
        A2.print ();
        goVectorf B(3);
        B[0] = 1.0; B[1] = 2.0; B[2] = 3.0;
        printf ("B:\n");
        B.print ();
        //if (!goMath::Lapack::gels (A, false, B))
        //{
        //    printf ("gels() failed.\n");
        //}
        goMath::Lapack::gelss (A, false, B);
        printf ("Solution:\n");
        B.print ();
    }
#endif
}
