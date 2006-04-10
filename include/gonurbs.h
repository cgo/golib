#ifndef GONUBS_H
#define GONUBS_H

#ifndef GOCURVE_H
# include <gocurve.h>
#endif

class goNUBSPrivate;

/*!
 * @brief Cubic Non-uniform non-rational B-spline approximation.
 * 
 * Uses cubic NUBS to approximate a curve.
 * A curve can either be given in the constructor as a goCurve* or
 * as a goList<goPointf> to setControlPoints().
 * In order to get knots with multiplicity > 0, define points with the desired multiplicity.
 * If you want periodic functions (closed curves), append the first point of the curve at its end.
 * That's not tested, but should suffice.
 *
 * @todo B is currently calculated recursively. Use de Boor instead.
 * @author Christian Gosch
 */
class goNUBS
{
    public:
        goNUBS ();
        goNUBS (const goCurvef* curve);
        goNUBS (const goCurved* curve);
        virtual ~goNUBS ();

        goDouble getCurveLength () const;
        bool     calculate ();
        bool     setControlPoints (const goList<goPointf>& points);
        bool     setControlPoints (const goList<goPointd>& points);
        bool     setControlPoints (goList<goPointf>::ConstElement* begin, goList<goPointf>::ConstElement* end, bool closed = false);
        bool     setControlPoints (goList<goPointd>::ConstElement* begin, goList<goPointd>::ConstElement* end, bool closed = false);
        bool     setControlPoints (goList<goPointf>::ConstElement* begin, goIndex_t count, bool closed = false);
        bool     setControlPoints (goList<goPointd>::ConstElement* begin, goIndex_t count, bool closed = false);
        goPointf operator() (goFloat u);

    private:
        goNURBSPrivate* myPrivate;

        goNUBS(goNUBS&);
        goNUBS& operator=(goNUBS&);
};

#endif
