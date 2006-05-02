#include <gomath.h>
#include <gopoint.h>
#include <golist.h>
#include <golog.h>
#include <gostring.h>
#include <goplot.h>
#include <gocurvature.h>
#include <gocurvaturediffusion.h>

template <class REAL>
static REAL vertexNormalComponent (REAL r, REAL phi, REAL e);
template <class pointT>
static goDouble metricParameter (pointT p1, pointT p2, pointT p3);

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
            phi[i]   = goTurnAngle(el->prev->elem, el->elem, el->next->elem);
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
            goDouble k_left = goCurvature(el_left->prev->elem, el_left->elem, el_left->next->elem);
            goDouble k_right = goCurvature(el_right->prev->elem, el_right->elem, el_right->next->elem);
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
        f_normal_ret[i]  *= k_star[i] - goCurvature(el->prev->elem, el->elem, el->next->elem);

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

/*
 * After Delingette et al.
 */
template <class pointT>
bool goCurvatureDiffusionFlow (goList<pointT>& points, int sigma, goFixedArray<pointT>& f_normal_ret, goFixedArray<pointT>& f_tangent_ret, goFixedArray<goDouble>* Lret, goFixedArray<goDouble>* phiRet, goFixedArray<goDouble>* rRet)
{
    if (!points.isClosed())
    {
        goLog::warning ("goCurvatureDiffusionFlow(): points must be a closed list (for a closed curve).\nIf you need open curves, change the implementation.");
        return false;
    }
    if (sigma > points.getSize() / 2)
    {
        goLog::warning ("goCurvatureDiffusionFlow(): try a smaller sigma.");
        return false;
    }

    if (f_normal_ret.getSize() != static_cast<goSize_t>(points.getSize()))
    {
        f_normal_ret.setSize (points.getSize());
    }
    if (f_tangent_ret.getSize() != static_cast<goSize_t>(points.getSize()))
    {
        f_tangent_ret.setSize (points.getSize());
    }
    if (Lret)
    {
        if (Lret->getSize() != f_normal_ret.getSize())
        {
            Lret->setSize (f_normal_ret.getSize());
        }
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
            r[i]     = p.abs() * 0.5;
            e[i]     = metricParameter(el->prev->elem, el->elem, el->next->elem);
            phi[i]   = goTurnAngle(el->prev->elem, el->elem, el->next->elem);
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
            goDouble k_left = goCurvature(el_left->prev->elem, el_left->elem, el_left->next->elem);
            goDouble k_right = goCurvature(el_right->prev->elem, el_right->elem, el_right->next->elem);
            enumerator += k_left*l_right + k_right*l_left;
            denominator += l_left + l_right;
            
            el_left = el_left->prev;
            el_right = el_right->next;
            ++j;
        }
        assert (denominator != 0.0);
        k_star[i] = enumerator / denominator;

        //= Calculate the normal force vectors
        assert (r[i] != 0.0);
        // f_normal_ret[i]   = (el->next->elem - el->prev->elem) / (2.0*r[i]); //= Numerically bad for small r[i]?
        f_tangent_ret[i]   = (el->next->elem - el->prev->elem); 
        f_normal_ret[i].y =  f_tangent_ret[i].x;
        f_normal_ret[i].x = -f_tangent_ret[i].y;
        {
            goDouble temp = sqrt(f_normal_ret[i].x * f_normal_ret[i].x + f_normal_ret[i].y * f_normal_ret[i].y);
            if (temp == 0.0)
            {
                goLog::warning("goCurvatureDiffusionFlow(): |normal| == 0.0\n");
            }
            else
            {
                temp = 1.0 / temp;
                f_normal_ret[i].x *= temp;
                f_normal_ret[i].y *= temp;
                f_tangent_ret[i].x *= temp;
                f_tangent_ret[i].y *= temp;
            }
        }
        goDouble phi_star = 0.0;
        if (fabs(k_star[i] * r[i]) <= 1.0)
        {
            phi_star = asin(k_star[i] * r[i]);
        }
//        if (phi_star == 0.0)
//        {
//            phi_star = 1e-5;
//        }
//        printf ("vertexNormalComponent(%f,%f,%f): %f\n",r[i],phi_star,0.5,vertexNormalComponent(r[i],phi_star,0.5));
//        printf ("vertexNormalComponent(%f,%f,%f): %f\n",r[i],phi[i],e[i],vertexNormalComponent(r[i],phi[i],e[i]));
        
        goDouble vnc_star = vertexNormalComponent(r[i],phi_star,0.5); // goMath::min<goDouble>(vertexNormalComponent(r[i],phi_star,0.5),2.0);
        goDouble vnc      = vertexNormalComponent(r[i],phi[i],e[i]); // goMath::min<goDouble>(vertexNormalComponent(r[i],phi[i],e[i]),2.0);
        // vnc_star = goMath::min<goDouble> (goMath::max<goDouble>(vnc_star,-2.0),2.0);
        // vnc = goMath::min<goDouble> (goMath::max<goDouble>(vnc,-2.0),2.0);
        if (Lret)
        {
            (*Lret)[i] = vertexNormalComponent(r[i],phi[i],e[i]);
            (*phiRet)[i] = phi[i];
            (*rRet)[i] = r[i];
        }
        f_normal_ret[i] *= vnc_star - vnc;
        f_tangent_ret[i] *= ((el->prev->elem + el->next->elem)*0.5 - el->elem) * f_tangent_ret[i];
        
        //f_normal_ret[i]  *= vertexNormalComponent(r[i],phi_star,0.5) -
        //    vertexNormalComponent(r[i],phi[i],e[i]);

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

//====================================================================

/* Here lies the rabbit in the pepper. Haha. */
template <class REAL>
static REAL vertexNormalComponent (REAL r, REAL phi, REAL e)
{
    if (phi == 0.0)
    {
        return 0.0;
    }
    REAL mu      = REAL(1.0f);
    REAL abs_phi = fabs(phi);
    //= This sign seems to be flipped in the paper.
    if (abs_phi < M_PI*0.5)
        mu = REAL(-1.0f);
    goDouble tan_phi = tan(phi);
    return r / tan_phi * (1 + mu * sqrt(1 + 4*e*(1-e)*tan_phi*tan_phi));
}

template <class pointT>
static goDouble metricParameter (pointT p1, pointT p2, pointT p3)
{
    pointT D = p3 - p1;
    goDouble r2 = D.abs();
    assert(D.abs() != 0.0);
    D *= 1.0 / D.abs();
    return 1-((p2-p1)*D)/r2;
    // goDouble F = (p2-p1) * D;
    // return 1 - F / (2.0f*r);
}

template 
bool goCurvatureDiffusionFlow<goPointf> (goList<goPointf>& points, int sigma, goFixedArray<goPointf>& f_normal_ret, goFixedArray<goPointf>&, goFixedArray<goDouble>*, goFixedArray<goDouble>*, goFixedArray<goDouble>*);

template 
bool goCurvatureDiffusionFlow<goPointd> (goList<goPointd>& points, int sigma, goFixedArray<goPointd>& f_normal_ret, goFixedArray<goPointd>&, goFixedArray<goDouble>*, goFixedArray<goDouble>*, goFixedArray<goDouble>*);
