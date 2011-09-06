/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gocomplex.h>

std::ostream& operator<< (std::ostream& o, const goComplex<double>& c) {
  o << c.re() << " + " << c.im() << "i";
  return o;
}

std::ostream& operator<< (std::ostream& o, const goComplex<float>& c) {
  o << c.re() << " + " << c.im() << "i";
  return o;
}
