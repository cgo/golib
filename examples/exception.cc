#include <goexception.h>
#include <iostream.h>

class
testclass
{
public:
  void test();
};

void
testclass::test()
{
  if (true)
    {
      goExceptionString e("TesT");
      e.st = "aaaargh!";
      throw e;
    }
}

int main()
{
  testclass t;
  try
    {
      t.test();
    }
  catch (goException &e)
    {
      e.print();
    }
  cout << "Ende!" << endl;
  return 1;
}
