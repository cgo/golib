#ifndef GONURBS_H
#define GONURBS_H

#ifndef GOCURVE_H
# include <gocurve.h>
#endif

class goNURBSPrivate;

/*!
 * @brief Cubic Non-uniform rational B-spline approximation.
 * 
 * Uses cubic NURBS to approximate a curve.
 * A curve can either be given in the constructor as a goCurve* or
 * as a goList<goPointf> to interpolate().
 * In order to get knots with multiplicity > 0, define points with the desired multiplicity.
 * If you want periodic functions (closed curves), append the first point of the curve at its end.
 * That's not tested, but should suffice.
 *
 * @todo B is currently calculated recursively. Use de Boor instead.
 * @author Christian Gosch
 */
class goNURBS
{
    public:
        goNURBS ();
        goNURBS (const goCurve* curve);
        virtual ~goNURBS ();

        goDouble getCurveLength () const;
        void     interpolate ();
        bool     interpolate (const goList<goPointf>& points);
        goPointf operator() (goFloat u);

    private:
        goNURBSPrivate* myPrivate;
};

#endif
