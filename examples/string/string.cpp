/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <stdio.h>
#include <gostring.h>

int main ()
{
    goString s = "  Hallo, mein Name ist    Hase!\n";
    goString s2 = "Hallo";
    goList<goString> l;
    s2.getWords(l);
    goList<goString>::Element* el = l.getFrontElement();
    while (el)
    {
        printf ("|%s|\n", el->elem.toCharPtr());
        el = el->next;
    }
    exit(1);
}
