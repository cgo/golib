/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goposition.h>

goPosition::goPosition ()
  : go3Vector<goInt32> () {
}

goPosition::goPosition (goInt32 x, goInt32 y, goInt32 z)
    : go3Vector<goInt32> (x, y, z)
{
}

goPosition::~goPosition () {
}

