#ifndef GODISCRETECURVEPARTITION_H
#define GODISCRETECURVEPARTITION_H

#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOCURVE_H
# include <gocurve.h>
#endif
#ifndef GOPOINT_H
# include <gopoint.h>
#endif
#ifndef GOTYPES_H
# include <gotypes.h>
#endif

template<class T> class goDiscreteCurvePartitionPrivate;

/**
 * @brief Discrete "curve partitioning" after Latecki et al.
 *
 * What this class does is actually removing the
 * "least significant" or "least relevant" points from a digital curve.
 * The relevance is measured as in Longin Jan Latecki's convexity rule paper:
 * L.J. Latecki & R. Lak�mper 
 * Convexity Rule for Shape Decomposition Based on Discrete Contour Evolution 
 * Computer Vision and Image Understanding, 1999, 73, 441-454
 *
 * @note This is not a fast implementation. It does not sort the points, but searches for the minimum
 *       in each step (it's experimental and may be dropped, that's why I didn't put too much effort
 *       into it).
 **/
template <class T>
class goDiscreteCurvePartition : public goObjectBase 
{
    public:
        goDiscreteCurvePartition();
        virtual ~goDiscreteCurvePartition();
        
        void                  setCurve (goCurve<goPoint<T> >* curve);
        goCurve<goPoint<T> >* getCurve ();
        bool                  lateckiSimplify (goIndex_t minPoints = 3, goDouble maxLength = -1.0);
        goDouble              lateckiRelevanceMeasure (const goPoint<T>& p1, const goPoint<T>& p2, const goPoint<T>& p3, goDouble totalCurveLength, goDouble& betaRet, goDouble& maxSegmentLength);

    private:
        goDiscreteCurvePartition (goDiscreteCurvePartition&);
        goDiscreteCurvePartition& operator= (goDiscreteCurvePartition&);
        
    private:
        goDiscreteCurvePartitionPrivate<T>* myPrivate;
};

#endif
