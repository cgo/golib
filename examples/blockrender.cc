#include <goblockrenderer.h>
#include <goviewvolume.h>
#include <goviewmanager.h>
#include <goresolutionmanager.h>
#include <gotypes.h>
#include <go3vector.h>
#include <goexception.h>

#include <gotransferfunction.h>

#include <gosignal2d.h>
#include <gofileio.h>

#define TYPE goInt16
#define IMG_W 256
#define IMG_H 256

int main(int argc, char *argv[])
{

  if (argc < 2)
    {
      cout << "goBlockRenderer example, 2001 Christian Gosch" << endl;
      cout << argv[0] << " <transformed volume file>" << endl;
      return 2;
    }
  
  Vol::goBlockRenderer<TYPE> r;
  goViewVolume v;

  Vol::goViewManager<TYPE>	vm;
  goString		fileName;
  fileName = argv[1];

  vm.setFileName (fileName);
  vm.setViewVolume (&v);
  
  try
    {
      vm.init();
    }
  catch (goException& e)
    {
      e.print();
      return 2;
    }

  vm.update();

  v.setPosition (1,1,-100);
  v.setEyeDistance (1000);
  v.setSize (256,256);
  go3Vector<goDouble> vec;
  vec.x = 0;   vec.y = -1;   vec.z = 0;
  v.setUp (vec);
  vec.x = 0;   vec.y = 0;   vec.z = 1;
  v.setNormal (vec);

  r.setViewPlane((goViewPlane&)v);
  r.setResolutionManager (vm.getResolutionManager());
  r.setStages (vm.getFileInfo().stages);
  r.setBlockProvider (vm.getBlockStore());
  r.setImageSize (IMG_W,IMG_H);

  vec.x = vm.getFileInfo().size.x;
  vec.y = vm.getFileInfo().size.y;
  vec.z = vm.getFileInfo().size.z;
  r.setSize (vec);
  vec.x = 0;
  vec.y = 0;
  vec.z = 0;
  r.setPosition (vec);
  r.setStages (vm.getFileInfo().stages);

  goTransferFunction <TYPE,goDouble> alpha;
  goTransferFunction <TYPE,goDouble> color;
  alpha.addSegment (20, .6, 4000, 1.0);
  color.addSegment (20, 0.001, 1000, 0.04);
  r.setAlpha(&alpha);
  r.setColor(&color);

  try 
    {
      r.init();
    }
  catch (goExceptionString& e) 
    {
      e.print();
      return 2;
    }

  goArray<goSize_t> indices;
  indices.resize(0);
  goSize_t blocks;
  blocks = vm.getFileInfo().blocks.x * 
    vm.getFileInfo().blocks.y * 
    vm.getFileInfo().blocks.z; 
  goSize_t i;
  for (i = 0; i < blocks; i++)
    indices += i;
  r.render (&indices);


  goTransferFunction<goDouble, goInt16> tf2;
  tf2.addSegment (0.01, 11, 1.0, 250);
  goDouble *tempImage = r.getTempImage();
  goSignal2D<goInt32> image(IMG_W,IMG_H);
  goSize_t x,y;
  for (y = 0; y < IMG_H; y++)
    {
      for (x = 0; x < IMG_W; x++)
	{
	  *image.getPtr(x,y) = tf2[tempImage[x + y * IMG_W]];
	}
    }
  goFileIO::writePGM ("image.pgm",image);

  return 1;
}










