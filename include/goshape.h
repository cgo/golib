#ifndef GOSHAPE_H
#define GOSHAPE_H

#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOPOINT_H
# include <gopoint.h>
#endif
#ifndef GOARRAY_H
# include <goarray.h>
#endif
#ifndef GOLIST_H
# include <golist.h>
#endif
#include <gocurve.h>

class goShapePrivate;

class goShape : public goObjectBase
{
    public:
        goShape (goCurvef* c = 0);
        goShape (goShape& other);
        virtual ~goShape ();

        goCurvef*       getCurve ();
        const goCurvef* getCurve () const;
        void           setCurve (goCurvef*);
        bool           getWeights (goArray<goFloat>& weights) const;
        
        static bool align         (goCurvef& curve, const goCurvef& other);
        static bool center        (goCurvef& curve);
        static bool normalize     (goCurvef& curve);
        static bool procrustesFit (goList<goCurvef*>& shapes, goCurvef& meanShapeRet);

        virtual bool callObjectMethod (int methodID, goObjectMethodParameters* param = NULL);
        
    protected:
        virtual void receiveObjectMessage (const goObjectMessage& message);

    private:
        goShapePrivate* myPrivate;
};

template <class pointT>
goIndex_t getConvexConcaveParts (goArray<goIndex_t>& partEndRet, goList<pointT>& points);
template <class T>
bool goLinearizeCurve (goCurve<goPoint<T> >& curve);
template <class pointT>
goDouble relevanceMeasure (const pointT& p1, const pointT& p2, const pointT& p3, goDouble totalCurveLength);

#endif
