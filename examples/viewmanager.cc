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
