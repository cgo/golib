#ifndef __GOEQSYSTEM_H__
#define __GOEQSYSTEM_H__

#include <gomatrix.h>
#include <goarray.h>
#include <gotypes.h>
#include <iostream>

class goEqSystem : public goMatrix<goDouble> {
public:
  goEqSystem (goSize_t y = 4, goSize_t x = 4);
  virtual ~goEqSystem ();

  bool			solve ();
  goArray<goDouble>&	getSolution () { return solution; }

  friend std::ostream& operator<< (std::ostream& o, goEqSystem& s);

protected:
  goArray<goDouble> solution;
};


#endif
