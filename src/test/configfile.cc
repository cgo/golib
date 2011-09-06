/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <misc/goconfigfile.h>
#include <iostream.h>

int main (int argc, char* argv[]) {
  goConfigFile f;

  if (argc < 2) {
    cout << "dumbass\n";
    exit (1);
  }

  f.read (argv[1]);

  cout << f.get ("testchapter","testsection !!","this") << "\n";
  if (f.fail()) {
    cout << "Fehler !!\n";
  }

  f.write (argv[2]);
  exit (0);
}
