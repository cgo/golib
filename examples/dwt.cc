#include <godwt.h>
#include <gosignal3d.h>
#include <gosignalmacros.h>
#include <time.h>

#define TYPE goUInt8

int main()
{
  goSignal3D<TYPE>	block;
  block.make(8,8,8,0,0,0);
  goSignal3D<TYPE> block2;
  block2.make (block.getSizeX(), block.getSizeY(), block.getSizeZ(),
	       0,0,0);
  goDWT<TYPE>	dwt;
//   goSignal3D<godwt_t>	dwtBlock;
//   dwtBlock.make(64,64,64,0,0,0);

  clock_t t1,t2;
  
  int i,j,k;

  for (i = 0; i < block.getSizeZ(); i++)
    {
      for (j = 0; j < block.getSizeY(); j++)
	{
	  for (k = 0; k < block.getSizeX(); k++)
	    {
	      // *block.getPtr(k,j,i) = i / 0.3 + j / 0.2 + k * 1.76523;
	      *block.getPtr(k,j,i) = i + k;
	    }
	}
    }

  block.fill(128);

  cout << "Original block first x/y slice: " << endl;
  cout << "-------------------------------" << endl;
  for (i = 0; i < block.getSizeY(); i++)
  {
      for (j = 0; j < block.getSizeX(); j++)
      {
	  cout << (int)*block.getPtr(j,i,0) << " ";
      }
      cout << endl;
  }
  cout << endl;
  
  /*
   * Test Haar transform.
   */
  GO_SIGNAL3D_EACHELEMENT_2(*__ptr_target = *__ptr, block, block2, TYPE, TYPE);
  t1 = clock();
  dwt.haar(block, 2);
  t2 = clock();
  cout << "Time for transform: " << (t2-t1) / (float)CLOCKS_PER_SEC << "s" << endl;
  
  cout << "DWT block first x/y slice: " << endl;
  cout << "--------------------------" << endl;
  for (i = 0; i < block.getSizeY(); i++)
    {
      for (j = 0; j < block.getSizeX(); j++)
	{
	  cout << (int)*block.getPtr(j,i,0) << " ";
	}
      cout << endl;
    }
  cout << endl;

  /*
   * Test reverse Haar transform.
   */
  t1 = clock();
  dwt.unHaar (block, 2);
  t2 = clock();
  cout << "Time for reverse transform: " << (t2-t1) / (float)CLOCKS_PER_SEC << "s" << endl;

  cout << "iDWT block first x/y slice: " << endl;
  cout << "--------------------------" << endl;
  for (i = 0; i < block.getSizeY(); i++)
    {
      for (j = 0; j < block.getSizeX(); j++)
	{
	  cout << (int)*block.getPtr(j,i,0) << " ";
	}
      cout << endl;
    }
  cout << endl;

  cout << "block";
  goDouble var = 0.0f;
  if (block == block2)
    {
      cout << " == ";
    }
  else 
    {
      cout << " != ";
      for (i = 0; i < block.getSizeX(); i++)
	{
	  for (j = 0; j < block.getSizeY(); j++)
	    {
	      for (k = 0; k < block.getSizeZ(); k++)
		{
		  var += fabs(*block.getPtr(i,j,k) - *block2.getPtr(i,j,k));
		}
	    }
	}
      var /= (float)(i * j * k);
    }
  cout << "block2" << endl;
  cout << "Mean error: " << var << endl;
  
  cout << "Memory usage" << endl;
  cout << "------------" << endl;
  cout << "block: " << block.getSize() << " bytes" << endl;
  
  block.destroy();
  block2.destroy();

  exit(1);
}











