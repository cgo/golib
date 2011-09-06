/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
  
  goSize_t test = 1;
  test = test >> 1;
  cout << "test == " << test << "\n";
  
  return 1;
}
