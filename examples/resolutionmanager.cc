#include <goresolutionmanager.h>
#include <iostream.h>

#define SIZE 8

int main()
{
  goNibbleArray nibbles(SIZE);

  /*
   * Test the nibble array used by the resolution manager
   */

  int i;
  int i2;
  for (i = 0; i < SIZE; i++)
    {
      nibbles.set(i, i);
      for (i2 = 0; i2 < SIZE; i2++)
	{
	  cout << (int)nibbles[i2] << " ";
	}
      cout << endl;
    }
  
  for (i = 0; i < SIZE; i++)
    {
      nibbles.set(i, 14);
      for (i2 = 0; i2 < SIZE; i2++)
	{
	  cout << (int)nibbles[i2] << " ";
	}
      cout << endl;
    }

  exit(1);
}
