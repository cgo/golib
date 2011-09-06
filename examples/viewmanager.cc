/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goviewmanager.h>
#include <goviewvolume.h>
#include <goexception.h>
#include <gotypes.h>

int main()
{
  goViewManager<goInt16> vm;
  goViewVolume v;
  vm.setFileName("transvolume.dwtv");
  vm.setViewVolume(&v);
  try
    {
      vm.init();
    }
  catch (goException &e)
    {
      e.print();
      return 2;
    }
  vm.update();
  return 1;
}
