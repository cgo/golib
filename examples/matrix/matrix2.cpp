/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gomatrix.h>
#include <stdio.h>

template <class T>
void print_cols (const goMath::Matrix<T>& M)
{
    typename goMath::Matrix<T>::const_vector_iterator iter = M.colBegin ();
    for (; iter != M.colEnd(); ++iter)
    {
        iter->print ();
    }
}

int main ()
{
    {
        goMath::Matrix<float> M (10, 2);
        goMath::Matrix<float>::vector_iterator iter = M.rowBegin ();

        goMath::Matrix<float>::vector_iterator iter_end = M.rowEnd ();

        M.print ();

        int base = 10;
        for (int i = 0; iter != iter_end; ++iter, ++i)
        {
            iter->fillRange (base, 1, base + M.getColumns());
            base += 10;
        }

        M.print ();

        print_cols (M);

        printf ("Size of M: %d, size of iterator: %d\n", sizeof (M), sizeof (iter));

        printf ("Blah.\n");
        exit (1);
    }
}
