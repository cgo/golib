#include <godwt.h>
#include <gosignal3d.h>
#include <gosignalmacros.h>
#include <time.h>
#include <iostream>

#define TYPE goInt8

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
	      	//	*block.getPtr(k,j,i) = i + k;
				*block.getPtr(k,j,i) = 127 * (k % 2);
	    	}
		}
    }

  // block.fill(128);

  std::cout << "Original block first x/y slice: " << std::endl;
  std::cout << "-------------------------------" << std::endl;
  for (i = 0; i < block.getSizeY(); i++)
  {
      for (j = 0; j < block.getSizeX(); j++)
      {
          std::cout << (int)*block.getPtr(j,i,0) << " ";
      }
      std::cout << std::endl;
  }
  std::cout << std::endl;
  
  /*
   * Test Haar transform.
   */
  GO_SIGNAL3D_EACHELEMENT_2(*__ptr_target = *__ptr, block, block2, TYPE, TYPE);
  t1 = clock();
  dwt.haar(block, 2);
  t2 = clock();
  std::cout << "Time for transform: " << (t2-t1) / (float)CLOCKS_PER_SEC << "s" << std::endl;
  
  std::cout << "DWT block first x/y slice: " << std::endl;
  std::cout << "--------------------------" << std::endl;
  for (i = 0; i < block.getSizeY(); i++)
    {
      for (j = 0; j < block.getSizeX(); j++)
	{
        std::cout << (int)*block.getPtr(j,i,0) << " ";
	}
      std::cout << std::endl;
    }
  std::cout << std::endl;

  /*
   * Test reverse Haar transform.
   */
  t1 = clock();
  dwt.unHaar (block, 2);
  t2 = clock();
  std::cout << "Time for reverse transform: " << (t2-t1) / (float)CLOCKS_PER_SEC << "s" << std::endl;

  std::cout << "iDWT block first x/y slice: " << std::endl;
  std::cout << "--------------------------" << std::endl;
  for (i = 0; i < block.getSizeY(); i++)
    {
      for (j = 0; j < block.getSizeX(); j++)
	{
        std::cout << (int)*block.getPtr(j,i,0) << " ";
	}
      std::cout << std::endl;
    }
  std::cout << std::endl;

  std::cout << "block";
  goDouble var = 0.0f;
  if (block == block2)
    {
        std::cout << " == ";
    }
  else 
    {
        std::cout << " != ";
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
  std::cout << "block2" << std::endl;
  std::cout << "Mean error: " << var << std::endl;
  
  std::cout << "Memory usage" << std::endl;
  std::cout << "------------" << std::endl;
  std::cout << "block: " << block.getSize() << " bytes" << std::endl;
  
  block.destroy();
  block2.destroy();

  exit(1);
}











