#include <govolumerendererexp.h>
#include <goviewmanager.h>
#include <goviewvolume.h>
#include <goexception.h>
#include <gotypes.h>
#include <go3vector.h>

#include <gosignal2d.h>
#include <gofileio.h>

int main(int argc, char* argv[])
{
  if (argc < 2)
    {
      cout << argv[0] << " <transformed volume file>" << endl;
      exit(2);
    }
  Vol::goViewManager<goInt16> vm;
  goViewVolume v;
  vm.setFileName(argv[1]);
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

  Vol::goVolumeRendererEXP<goInt16,goInt16> renderer;
  renderer.setBlockProvider (vm.getBlockStore());
  go3Vector<goDouble> vec;
  v.setPosition (1,1,-100);
  v.setSize (256,256);
  vec.x = 0; vec.y = -1; vec.z = 0;
  v.setUp (vec);
  vec.x = 0; vec.y = 0; vec.z = 1;
  v.setNormal (vec);
  v.setEyeDistance (1000);
  renderer.setViewPlane ((goViewPlane&)v);
  go3Vector<goDouble> volPos;
  volPos.x = 0;
  volPos.y = 0;
  volPos.z = 0;
  renderer.setPosition (volPos);
  go3Vector<goDouble> volSize;
  volSize.x = vm.getVolumeSize().x;
  volSize.y = vm.getVolumeSize().y;
  volSize.z = vm.getVolumeSize().z;
  renderer.setSize (volSize);

  cout << "Little viewplane test:" << endl;
  cout << "----------------------" << endl;
  v.update();
  cout << "ok" << endl;
  cout << "----------------------" << endl;
  
  goInt16* image = new goInt16[256*256];
  renderer.renderInit (image, 256, 256);
  renderer.render ();
  goSignal2D<goInt32> tmp (256,256);
  int x,y;
  for (y = 0; y < 256; y++)
    {
      for (x = 0; x < 256; x++)
	{
	  *tmp.getPtr(x,y) = image[x + y*256];
	}
    }
  goFileIO::writePGM ("renderedimage.pgm",tmp);
  
  delete[] image;
  return 1;
}


