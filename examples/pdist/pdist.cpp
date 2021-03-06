/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <stdio.h>
#include <gomath.h>
#include <gomatrix.h>

int main ()
{
    goFloat data[] = {1, 2, 3,
                      4, 5, 6,
                      7, 8, 9,
                      10, 11, 12};
    goMatrixf X (data, 4, 3);
    X.print();
    
    goMatrixf d;
    goMath::pdist (X, 0, d);
    d.print();
    goMath::pdist (X, 1, d);
    d.print();
    exit(1);
}
