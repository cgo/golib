#ifndef GOEDGEDETECT_H
#define GOEDGEDETECT_H

#include <gosignal2d.h>
#include <goarray.h>

/**
 * Detection of edges and lines in a signal (currently using Hough transform). <br>
 * This class was written as an assignment for a computer vision class at the University of Adelaide.<br>
 * @author Christian Gosch
 */
template <class T>
class goEdgeDetect {
 public:
  ///
  goEdgeDetect ();
  ///
  virtual ~goEdgeDetect ();

  /**
   * Take care of boundary violations. They are not checked for (yet).
   */
  void run (goSignal2D<T> *in, goFloat threshold, goFloat lineThresh,
	    goFloat distFactor = 1.0, goInt32 angles = 180, bool doLowPass = false);
  ///
  goSignal2D<goFloat> *getHiPass () { return result; }
  ///
  goSignal2D<goFloat> *getHistogram () { return histogram; }

  ///
  goArray<goInt32>    *getLines () { return &lines; }

  /// Experimental. Not used.
  void		      connectLines();

 protected:
  goSignal2D<goFloat> *result;
  goSignal2D<goFloat> *histogram;
  goArray<goInt32>     lines;
  goArray<goFloat>     lineAngles;
};

#endif
