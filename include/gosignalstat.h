#ifndef GOSIGNALSTAT_H
#define GOSIGNALSTAT_H

#include <gotypes.h>
#include <gosignal3d.h>

/*!
 * Signal statistics.
 * Currently supported signal types:
 * - goSignal3D, scalar type elements
 * @author Christian Gosch
 * @date 18.9.2001
 */ 
template<class T>
class
goSignalStat
{
 public:
    goSignalStat();
    virtual ~goSignalStat();

    /*!
     * Calculates the mean value of a goSignal3D and stores it internally.
	 * Uses a lot of divisions. If possible in special cases, use the getMeanFast() method.
     * @param signal goSignal3D of which the mean value is to be calculated
     * @return The mean value as a goDouble
     */
    goDouble   	getMean (goSignal3D<T>& signal);
	/*!
     * Same as getMean(goSignal3D<T>&), but faster. It accumulates and then divides,
	 * which means it does not work for large signals and in any case when the signal 
	 * values were accumulating to result in an overflow.
	 * Uses significantly less divisions.
	 */
    goDouble   	getMeanFast (goSignal3D<T>& signal);
    /*!
     * Calculates the deviation of a goSignal3D and stores it internally.
     * @param signal goSignal3D of which the deviation is to be calculated
     * @return The deviation as a goDouble
     */
    goDouble	getDeviation (goSignal3D<T>& signal, goDouble __mean);
    /*!
     * Calculates the energy of a goSignal3D and stores it internally.
     * The energy of a signal \f$ f(t) \f$ is defined as \f$ \int\limits_{-\infty}^{\infty} \left|f(t)\right|^2 \, dt  \f$.
     * This function computes \f$ \sum\limits_{n = 0}^{N-1} |f_n|^2 \f$ with \f$ f_n \f$ being the signal samples.
     * @param signal goSignal3D of which the energy is to be calculated
     * @return The energy as a goDouble
     */
    goDouble	getEnergy (goSignal3D<T>& signal);
    /*!
     * @return The last calculated mean value
     */
    goDouble	getMean () { return mean; }
    /*!
     * @return The last calculated deviation
     */
    goDouble	getDeviation () { return deviation; }
    /*!
     * @return The last calculated energy
     */
    goDouble	getEnergy () { return energy; }
 protected:
 private:
    goDouble   	mean;
    goDouble	deviation;
    goDouble	energy;
    
};

#endif
