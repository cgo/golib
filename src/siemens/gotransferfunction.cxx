#include <gotransferfunction.h>

template <class IN_T,class OUT_T>
goTransferFunction<IN_T,OUT_T>::
goTransferFunction () {
  initialise ();
}

template <class IN_T,class OUT_T>
goTransferFunction<IN_T,OUT_T>::
~goTransferFunction () {
  delete min;
  delete max;
  delete start;
  delete slope;
}

template <class IN_T,class OUT_T>
void
goTransferFunction<IN_T,OUT_T>::
initialise () {
  min = new goArray<IN_T>;
  max = new goArray<IN_T>;
  start = new goArray<OUT_T>;
  slope = new goArray<goDouble>;
  min->resize (0);
  max->resize (0);
  start->resize(0);
  slope->resize(0);
}


template <class IN_T,class OUT_T>
void
goTransferFunction<IN_T,OUT_T>::
addSegment (IN_T in1, OUT_T out1, IN_T in2, OUT_T out2) {
  IN_T mi, ma;
  OUT_T o1 = out1;
  in1 < in2 ? (mi = in1, ma = in2) : (mi = in2, ma = in1);

  min->resize (min->getSize() + 1);
  (*min)[min->getSize() - 1] = mi;

  max->resize (max->getSize() + 1);
  (*max)[max->getSize() - 1] = ma;

  start->resize (start->getSize() + 1);
  (*start)[start->getSize() - 1] = o1;

  goDouble f;
  f = (goDouble) ( (out2 - out1) / (goFloat)(ma - mi) );
  slope->resize (slope->getSize() + 1);
  (*slope)[slope->getSize() - 1] = f;

  cout << "transferFunction: Added " << mi << "->" << ma << " --> " << out1 << "," << f << endl;
}


template class goTransferFunction < goDouble,goDouble >;
template class goTransferFunction < goInt8,goDouble >;
template class goTransferFunction < goDouble,goInt8 >;
template class goTransferFunction < goUInt8,goDouble >;
template class goTransferFunction < goDouble,goUInt8 >;
template class goTransferFunction < goInt16,goDouble >;
template class goTransferFunction < goDouble,goInt16 >;
