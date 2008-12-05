#ifndef GOSIGNAL_H
#define GOSIGNAL_H

#include <gotypes.h>
#include <gonvector.h>

class goSignalInfo {
public:
  goDouble	energy;
  goDouble	power;
  bool		energyValid;
  bool		powerValid;
};

/**
 * goSignal can handle one-dimensional (and of course discrete) signals. It was mainly written to enable one to do 
 * various transformations using a subclass of goSignal.
 * @author Christian Gosch
 */
template <class T>
class goSignal : public goNVector<T> {
 public:
  ///
  goSignal ();
  ///
  virtual ~goSignal ();

  /**
   * Calculates the signals' energy. If the energy is infinity, infinity is returned.
   * The energy is calculated as follows:
   * $E = \sum_{n=0}^{N-1} |x(n)|^2$
   * @return Signal energy
   */
  goDouble	energy ();
  /**
   * Calculates signals' power if possible.
   * @return Signal power
   */
  goDouble	power ();
  
  /**
   * Sets the time step between two samplepoints in seconds.
   * @return True if the setting was successful
   */
  bool		setTimeStep (goTime_t t);
  ///
  goTime_t	getTimeStep ();
  /**
   * Does zero padding on the signal. I.e. adding zeroes at the tail of the original signal,
   * so the signal is made longer.
   * @return True if zero padding was successful
   */
  bool		zeroPadding (goSize_t s);

  /**
   * Sets the time axis offset in units of timeStep.
   * I.e., the offset value describes the coordinate systems' offspring.
   */
  void		setOffset (goInt32 o);

  /**
   * @return The time axis offset in units of timeStep
   */
  goInt32	getOffset ();
  
 protected:
  goTime_t	timeStep;
  goInt32	offset;
  bool		last_failed;
  goSignalInfo	info;

};

#endif
