/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <stdio.h>
#include <goheap.h>
#include <gostring.h>

int main ()
{
    goFloat keys[] = {0.1, 0.05, 3.2, 0.05, 0.2};
    goString values[] = {"Hund", "Katze", "Maus", "Elefant", "Giraffe"};
    goSize_t length = 5;
    
    goHeap<goString, goFloat> heap;
    heap.buildHeap (values, keys, length);
    
    for (goSize_t i = 0; i < length; ++i)
    {
        printf ("%s, %f\n", values[0].toCharPtr(), keys[0]);
        heap.removeRoot ();
    }

    exit (1);
}
