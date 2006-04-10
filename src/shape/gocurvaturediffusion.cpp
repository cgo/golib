#include <gomath.h>
#include <gopoint.h>
#include <golist.h>
#include <golog.h>
#include <gostring.h>
#include <goplot.h>

#include <gocurvaturediffusion.h>

static goDouble kappa (goDouble phi, goDouble r);
template <class pointT>
static goDouble kappa (const pointT& p1, const pointT& p2, const pointT& p3);
template <class REAL>
static REAL vertexNormalComponent (REAL r, REAL phi, REAL e);
template <class pointT>
static goDouble metricParameter (pointT p1, pointT p2, pointT p3);
template <class pointT>
static goDouble getTurn (const pointT& p1, const pointT& p2, const pointT& p3);

//====================================================================

/* Playground. Backup of the original function is below. */
template <class pointT>
bool goCurvatureDiffusionFlowTest (goList<pointT>& points, int sigma, goFixedArray<pointT>& f_normal_ret)
{
    if (!points.isClosed())
    {
        goLog::warning ("goCurveDiffusionRegularisation(): points must be a closed list (for a closed curve).\nIf you need open curves, change the implementation.");
        return false;
    }
    if (sigma > points.getSize() / 2)
    {
        goLog::warning ("goCurveDiffusionRegularisation(): try a smaller sigma.");
        return false;
    }
   
    assert(f_normal_ret.getSize() == (goSize_t)points.getSize());
    
    goIndex_t nPoints = points.getSize();
    goDouble l_left   = 0.0;
    goDouble l_right  = 0.0;
    
    typename goList<pointT>::Element* el = points.getFrontElement();
    goIndex_t i = 0;
    goDouble enumerator = 0.0;
    goDouble denominator = 0.0;
    goFixedArray<goDouble> k_star   (nPoints);
    goFixedArray<goDouble> r        (nPoints);
    goFixedArray<goDouble> phi      (nPoints);
    goFixedArray<goDouble> e        (nPoints);

    goFixedArray<goDouble> L_star_abs (nPoints);
    goFixedArray<goDouble> L_abs (nPoints);

    goFixedArray<goDouble> normalX (nPoints);
    goFixedArray<goDouble> normalY (nPoints);
    
    while (el && i < nPoints)
    {
        //= Calculate e, r, and phi
        {
            pointT p = el->next->elem - el->prev->elem;
            r[i]     = p.abs();
            e[i]     = metricParameter(el->prev->elem, el->elem, el->next->elem);
            phi[i]   = getTurn(el->prev->elem, el->elem, el->next->elem);
        }
        
        //= Calculate normal vectors n.
        
        
        //= Calculate k*
        goIndex_t j = 0;
        typename goList<pointT>::Element* el_left  = el->prev;
        typename goList<pointT>::Element* el_right = el->next;
        l_left      = 0.0;
        l_right     = 0.0;
        enumerator  = 0.0;
        denominator = 0.0;
        while (el_left && el_right && j < sigma)
        {
            pointT p = el_left->elem - el_left->next->elem;
            l_left += p.abs();
            p = el_right->elem - el_right->prev->elem;
            l_right += p.abs();
            goDouble k_left = kappa(el_left->prev->elem, el_left->elem, el_left->next->elem);
            goDouble k_right = kappa(el_right->prev->elem, el_right->elem, el_right->next->elem);
            enumerator += k_left*l_right + k_right*l_left;
            denominator += l_left + l_right;
            
            el_left = el_left->prev;
            el_right = el_right->next;
            ++j;
        }
        assert (denominator != 0.0);
        k_star[i] = enumerator / denominator;

        //= Calculate the normal force vectors
        f_normal_ret[i]   = (el->next->elem - el->prev->elem) / (2*r[i]);
        goDouble temp     = -f_normal_ret[i].y;
        f_normal_ret[i].y = f_normal_ret[i].x;
        f_normal_ret[i].x = temp;
        f_normal_ret[i]  *= phi[i] < 0.0 ? 1.0 : -1.0;
        // goDouble phi_star = asin(k_star[i] * r[i]);
        f_normal_ret[i]  *= k_star[i] - kappa(el->prev->elem, el->elem, el->next->elem);

        el = el->next;
        ++i;
    }

//    goString file1;
//    goString file2;
//    goPlot::gnuplot (phi,file1,file2,"Phi");
//    goPlot::gnuplot (r,file1,file2,"r");
//    goPlot::gnuplot (e,file1,file2,"e");
//    goPlot::gnuplot (k_star,file1,file2,"k_star");
    
    return true;
}

/* Backup of the original version, Thu Jan 26 12:16:40 CET 2006 */
/*
 * After Delingette et al.
 */
template <class pointT>
bool goCurvatureDiffusionFlow (goList<pointT>& points, int sigma, goFixedArray<pointT>& f_normal_ret)
{
    if (!points.isClosed())
    {
        goLog::warning ("goCurveDiffusionRegularisation(): points must be a closed list (for a closed curve).\nIf you need open curves, change the implementation.");
        return false;
    }
    if (sigma > points.getSize() / 2)
    {
        goLog::warning ("goCurveDiffusionRegularisation(): try a smaller sigma.");
        return false;
    }

    if (f_normal_ret.getSize() != points.getSize())
    {
        f_normal_ret.setSize (points.getSize());
    }
    
    assert(f_normal_ret.getSize() == (goSize_t)points.getSize());
    
    goIndex_t nPoints = points.getSize();
    goDouble l_left   = 0.0;
    goDouble l_right  = 0.0;
    
    typename goList<pointT>::Element* el = points.getFrontElement();
    goIndex_t i = 0;
    goDouble enumerator = 0.0;
    goDouble denominator = 0.0;
    goFixedArray<goDouble> k_star   (nPoints);
    goFixedArray<goDouble> r        (nPoints);
    goFixedArray<goDouble> phi      (nPoints);
    goFixedArray<goDouble> e        (nPoints);

    goFixedArray<goDouble> L_star_abs (nPoints);
    goFixedArray<goDouble> L_abs (nPoints);

    while (el && i < nPoints)
    {
        //= Calculate e, r, and phi
        {
            pointT p = el->next->elem - el->prev->elem;
            r[i]     = p.abs();
            e[i]     = metricParameter(el->prev->elem, el->elem, el->next->elem);
            phi[i]   = getTurn(el->prev->elem, el->elem, el->next->elem);
        }
        
        //= Calculate k*
        goIndex_t j = 0;
        typename goList<pointT>::Element* el_left  = el->prev;
        typename goList<pointT>::Element* el_right = el->next;
        l_left      = 0.0;
        l_right     = 0.0;
        enumerator  = 0.0;
        denominator = 0.0;
        while (el_left && el_right && j < sigma)
        {
            pointT p = el_left->elem - el_left->next->elem;
            l_left += p.abs();
            p = el_right->elem - el_right->prev->elem;
            l_right += p.abs();
            goDouble k_left = kappa(el_left->prev->elem, el_left->elem, el_left->next->elem);
            goDouble k_right = kappa(el_right->prev->elem, el_right->elem, el_right->next->elem);
            enumerator += k_left*l_right + k_right*l_left;
            denominator += l_left + l_right;
            
            el_left = el_left->prev;
            el_right = el_right->next;
            ++j;
        }
        assert (denominator != 0.0);
        k_star[i] = enumerator / denominator;

        //= Calculate the normal force vectors
        f_normal_ret[i]   = (el->next->elem - el->prev->elem) / (2*r[i]);
        goDouble temp     = -f_normal_ret[i].y;
        f_normal_ret[i].y = f_normal_ret[i].x;
        f_normal_ret[i].x = temp;
        {
            goDouble temp = sqrt(f_normal_ret[i].x * f_normal_ret[i].x + f_normal_ret[i].y + f_normal_ret[i].y);
            if (temp == 0.0)
            {
                goLog::warning("goCurvatureDiffusionFlow(): |normal| == 0.0\n");
            }
            else
            {
                f_normal_ret[i].x *= temp;
                f_normal_ret[i].y *= temp;
            }
        }
        goDouble phi_star = 0.0;
        if (fabs(k_star[i] * r[i]) <= 1.0)
        {
            phi_star = asin(k_star[i] * r[i]);
        }
        if (phi_star == 0.0)
        {
            phi_star = 1e-5;
        }
        printf ("vertexNormalComponent(%f,%f,%f): %f\n",r[i],phi_star,0.5,vertexNormalComponent(r[i],phi_star,0.5));
        printf ("vertexNormalComponent(%f,%f,%f): %f\n",r[i],phi[i],e[i],vertexNormalComponent(r[i],phi[i],e[i]));
        f_normal_ret[i]  *= vertexNormalComponent(r[i],phi_star,0.5) -
            vertexNormalComponent(r[i],phi[i],e[i]);

        L_star_abs[i] = vertexNormalComponent(r[i],phi_star,0.5);
        L_abs[i] = vertexNormalComponent(r[i],phi[i],e[i]);
        
        el = el->next;
        ++i;
    }

//    goString file1;
//    goString file2;
//    goPlot::gnuplot (phi,file1,file2,"Phi");
//    goPlot::gnuplot (r,file1,file2,"r");
//    goPlot::gnuplot (e,file1,file2,"e");
//    goPlot::gnuplot (k_star,file1,file2,"k_star");
//    goPlot::gnuplot (L_star_abs,file1,file2,"L_star");
//    goPlot::gnuplot (L_abs,file1,file2,"L");
    
    return true;
}

template <class pointT>
bool goCurvatureDiffusion (goList<pointT>& points, int sigma)
{
    return false;
}

//====================================================================

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

/* Here lies the rabbit in the pepper. Haha. */
template <class REAL>
static REAL vertexNormalComponent (REAL r, REAL phi, REAL e)
{
    REAL mu      = REAL(1.0f);
    REAL abs_phi = fabs(phi);
    if (abs_phi > M_PI*0.5)
        mu = REAL(-1.0f);

//    return phi;
    
    //= If |phi| is 'close' to 0, assume the distance to be linear in phi.
//    if (abs_phi < 0.25f)
//    {
//        return -phi;
//    }
    //= Close to +/- pi/2, use a series expansion (from Mathematica Series[]).
#if 0
    if (fabs(abs_phi - M_PI*0.5) < M_PI*0.25)
    {
        REAL e_root   = sqrt(4*e-4*e*e);
        REAL phi_pi_2 = REAL(0.0f); 
        if (phi > 0.0f)
            phi_pi_2 = phi - M_PI*0.5;
        else
            phi_pi_2 = phi + M_PI*0.5;
        return -e_root * mu * r - r * phi_pi_2 + 
            (-1.0/3.0 * e_root * mu - (1 - 8*e/3.0 + 8*e*e/3.0)*mu / (2*e_root)) *
            r * phi_pi_2 * phi_pi_2;
    }
#endif
    goDouble tan_phi = tan(phi);
    return r / tan_phi * (1 + mu * sqrt(1 + 4*e*(1-e)*tan_phi*tan_phi));
}

template <class pointT>
static goDouble metricParameter (pointT p1, pointT p2, pointT p3)
{
    pointT D = p3 - p1;
    goDouble r2 = D.abs();
    D *= 1.0 / D.abs();
    return 1-((p2-p1)*D)/r2;
    // goDouble F = (p2-p1) * D;
    // return 1 - F / (2.0f*r);
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
    return beta * ((s1.x * s2.y < s1.y * s2.x) ? -1.0 : 1.0);
}

template 
bool goCurvatureDiffusionFlow<goPointf> (goList<goPointf>& points, int sigma, goFixedArray<goPointf>& f_normal_ret);

template 
bool goCurvatureDiffusionFlow<goPointd> (goList<goPointd>& points, int sigma, goFixedArray<goPointd>& f_normal_ret);

template 
bool goCurvatureDiffusion (goList<goPointf>& points, int sigma);
template 
bool goCurvatureDiffusion (goList<goPointd>& points, int sigma);
