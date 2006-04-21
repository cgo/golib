#include "mex.h"
#include <gonurbs.h>
#include <golist.h>
#include <gopoint.h>

#define USAGE()\
{\
    mexErrMsgTxt ("closedcurvekappa: Usage error.");\
}

template <class pointT>
static goDouble getTurn (const pointT& p1, const pointT& p2, const pointT& p3);
template <class pointT>
static goDouble kappa (const pointT& p1, const pointT& p2, const pointT& p3);

/* --------------------------------------------------------------------------
* @brief kappa = closedcurvekappa(curve_points,sigma);
* Calculates a mean curvature over the neighbourhood of size 2*sigma around
* each curve point.
* Mean curvature calculation as in Delingette et al., 
* "Shape and Topology Constraints on Parametric Active Contours", page 21.
----------------------------------------------------------------------------*/
void mexFunction (int            nlhs, 
                  mxArray*       plhs[],
                  int            nrhs,
                  const mxArray* prhs[])
{
    if (nlhs != 1)
    {
        USAGE();
    }
    if (nrhs != 2)
    {
        USAGE();
    }

    const mxArray* pointsArray = prhs[0];
    int sigma         = (int)mxGetScalar(prhs[1]);
    int pointsRows    = mxGetM (pointsArray);
    int pointsColumns = mxGetN (pointsArray);
    int nPoints       = pointsColumns;
    if (pointsRows != 2)
    {
        mexErrMsgTxt ("Points (first argument) must be 2xN matrix.");
    }
    double* points = mxGetPr (pointsArray);
    plhs[0] = mxCreateDoubleMatrix (1, nPoints, mxREAL);

    goList<goPointd> pointList;
    int i;
    for (i = 0; i < pointsColumns; ++i)
    {
        pointList.append(goPointd(points[2*i],points[2*i+1]));
    }
    pointList.close();

    double* kappaRet = mxGetPr(plhs[0]);
    i = 0;
    goList<goPointd>::Element* el = pointList.getFrontElement();
    while (el && i < nPoints)
    {
        //= Calculate k*
        goIndex_t j = 0;
        goList<goPointd>::Element* el_left  = el->prev;
        goList<goPointd>::Element* el_right = el->next;
        goDouble l_left      = 0.0;
        goDouble l_right     = 0.0;
        goDouble enumerator  = 0.0;
        goDouble denominator = 0.0;
        while (el_left && el_right && j < sigma)
        {
            goPointd p = el_left->elem - el_left->next->elem;
            l_left  += p.abs();
            p        = el_right->elem - el_right->prev->elem;
            l_right += p.abs();
            goDouble k_left = kappa(el_left->prev->elem, 
                                    el_left->elem, 
                                    el_left->next->elem);
            goDouble k_right = kappa(el_right->prev->elem, 
                                     el_right->elem, 
                                     el_right->next->elem);
            enumerator += k_left*l_right + k_right*l_left;
            denominator += l_left + l_right;
            
            el_left = el_left->prev;
            el_right = el_right->next;
            ++j;
        }
        assert (denominator != 0.0);
        kappaRet[i] = enumerator / denominator;

        el = el->next;
        ++i;
    }
}

static goDouble kappa (goDouble phi, goDouble r)
{
    return sin(phi) / r;
}

template <class pointT>
static goDouble kappa (const pointT& p1, const pointT& p2, const pointT& p3)
{
    goDouble phi = getTurn(p1,p2,p3);
    pointT p = p3 - p1;
    return kappa(phi,p.abs()*0.5);
}

template <class pointT>
static goDouble getTurn (const pointT& p1, const pointT& p2, const pointT& p3)
{
    pointT base = p3 - p1;
    goDouble f = base.abs();
    if (f == 0.0)
    {
        assert ("p3 == p1" == 0);
        return 0.0;  //= This should not happen. It would mean p3 == p1.
    }
    base *= 1.0 / f;
    pointT s1 = p2 - p1;
    pointT s2 = p3 - p2;
    goDouble l1 = s1.abs();
    goDouble l2 = s2.abs();
    if (l1 == 0.0 || l2 == 0.0)
    {
        return 0.0;
    }
    assert (l1 != 0.0 && l2 != 0.0);

    goDouble alpha1 = (s1 * base) / l1;
    //= acos seems to result in nan when the argument is exactly 1.0 (contrary to the manpage!).
    //= Catch that.
    if (alpha1 <= -1.0)
    {
        alpha1 = M_PI;
    }
    else
    {
        if (alpha1 >= 1.0)
        {
            alpha1 = 0.0;
        }
        else
        {
            alpha1 = acos (alpha1);
        }
    }
    goDouble alpha2 = (s2 * base) / l2;
    if (alpha2 <= -1.0)
    {
        alpha2 = M_PI;
    }
    else
    {
        if (alpha2 >= 1.0)
        {
            alpha2 = 0.0;
        }
        else
        {
            alpha2 = acos (alpha2);
        }
    }

    goDouble beta = alpha1 + alpha2;

    //= The sign of the turn angle is determined by whether the triangle {p1,p2,p3} 
    //= turns clockwise or counter-clockwise, which in turn is determined
    //= by the z-component of the cross product {s1 0} x {s2 0}.

    return beta * ((s1.x * s2.y < s1.y * s2.x) ? 1.0 : -1.0);
}
