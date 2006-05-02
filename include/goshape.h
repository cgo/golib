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

template <class pointT> class goShapePrivate;

template <class pointT>
class goShape : public goObjectBase
{
    public:
        goShape ();
        goShape (const goShape<pointT>& other);
        virtual ~goShape ();
        const goShape<pointT>& operator= (const goShape<pointT>&);

        void addCurve (const goCurve<pointT>& curve);
        void addCurve (const goList<pointT>&  curve);
        goList< goCurve<pointT> >&       getCurves ();
        const goList< goCurve<pointT> >& getCurves () const;

        goSize_t getCurveCount () const;
        bool     readASCII     (const goString& filename);
        
        // DEPRECATED bool           getWeights (goArray<goFloat>& weights) const;
        
        static bool     align         (goCurve<pointT>& curve, const goCurve<pointT>& other);
        static bool     center        (goCurve<pointT>& curve);
        static goDouble normalize     (goCurve<pointT>& curve);
        static goDouble normalize     (goList<pointT>& points);
        static bool     procrustesFit (goList<goCurve<pointT>*>& shapes, goCurve<pointT>& meanShapeRet);

        virtual bool callObjectMethod (int methodID, goObjectMethodParameters* param = NULL);
        
    protected:
        virtual void receiveObjectMessage (const goObjectMessage& message);

    private:
        goShapePrivate<pointT>* myPrivate;
};

template <class pointT>
goIndex_t getConvexConcaveParts (goArray<goIndex_t>& partEndRet, goList<pointT>& points);
template <class T>
bool goLinearizeCurve (goCurve<goPoint<T> >& curve);
template <class pointT>
goDouble relevanceMeasure (const pointT& p1, const pointT& p2, const pointT& p3, goDouble totalCurveLength);

#endif
