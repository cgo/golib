#include <gohashtable.h>
#include <iostream.h>
#include <gotypes.h>

int main()
{
  goHashTable<goUInt32, void*> ht;

  int entries[10];
  int i;
  int val;
  for (i = 0; i < 10; i++)
    {
      entries[i] = i;
    }
  ht.setModValue (127);
  for (i = 10; i > 0; i--)
    {
      ht.add ((goUInt32)i, (void*)&entries[10 - i]);
    }
  cout << "Removing one element" << endl;
  int *iptr = (int*)ht.remove (5);
  cout << "Removed value was " << *iptr << endl;
  void *p;
  for (i = 10; i > 0; i--)
    {
      cout << "key = " << i << ": ";
      p = ht[(goUInt32)i];
      if (ht.fail())
	cout << "FAIL" << endl;
      else cout << *(int*)p << endl;
    }

}
