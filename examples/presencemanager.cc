#include <gotypes.h>
#include <gopresencemanager.h>
#include <goresolutionmanager.h>
#include <gosignal3d.h>
#include <stdio.h>
#include <goexception.h>

#define bitsize 64
#define TYPE goInt16

int main()
{
    goBitArray bits(bitsize);

    /*
     * Test setting and deleting bits by first setting each bit
     * and then deleting it again.
     * After setting/deleting each bit, the whole string is printed.
     */
#if 0
    int i;
    int c = 0;
    int i2;
    for (i2 = 0; i2 < bitsize; i2++)
    {
	bits.set(i2);
	c = 0;
	for (i = 0; i < bitsize; i++)
	{
	    cout << bits[i];
	    c++;
	    if (c == 8)
	    {
		cout << " ";
		c = 0;
	    }
	}
	cout << endl;
    }
    for (i2 = 0; i2 < bitsize; i2++)
    {
	bits.unSet(i2);
	c = 0;
	for (i = 0; i < bitsize; i++)
	{
	    cout << bits[i];
	    c++;
	    if (c == 8)
	    {
		cout << " ";
		c = 0;
	    }
	}
	cout << endl;
    }
#endif

    /*
     * Test the presence manager.
     */
    goPresenceManager<TYPE> pm;
    goResolutionManager rm;
    goVolumeFile<TYPE> file;
    goBlockStore<TYPE> store;
    file.setTransFileName ("transvolume.dwtv");
    pm.setVolumeFile (&file);
    pm.setResolutionManager (&rm);
    pm.setBlockStore(&store);
    try 
      {
	pm.init ();
      }
    catch (goException &e)
      {
	e.print();
	exit (1);
      }
    
    int i,i2;
    /*
     * Make a few blocks present and test presence flags
     */
    int noblocks = file.getFileInfo().blocks.x * 
      file.getFileInfo().blocks.y * 
      file.getFileInfo().blocks.z;
#if 0
    for (i2 = 0; i2 < 2; i2++)
      {
	for (i = 0; i < noblocks; i++)
	  {
	    if (!pm.isPresent(i))
	      {
		cout << i << " is not present" << endl;
		// rm.setResolution (i, file.getFileInfo().stages);
		// rm.setResolution (i, 1);
		// rm.setResolution (i, 0);
		rm.setResolution (i, file.getFileInfo().stages - 1);
		pm.makePresent(i);
		cout << "makePresent called" << endl;
	      } else {
		cout << i << " is present" << endl;
	      }
	  }
      }
#endif
    /*
     * Test for one single block to load.
     */
    rm.setResolution(0, 2);
    pm.makePresent(0);
    goSignal3D<TYPE> *block = store.getBlock(0);
    goSize_t x,y;
    cout << "First slice of block:" << endl;
    cout << "---------------------" << endl;
    for (y = 0; y < block->getSizeY(); y++)
      {
	for (x = 0; x < block->getSizeX(); x++)
	  {
	    cout << *block->getPtr(x,y,0) << " ";
	  }
	cout << endl;
      }
    

    exit(1);
}
