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

class goShapePrivate;
class goCurve;

class goShape : public goObjectBase
{
    public:
        goShape (goCurve* c = 0);
        goShape (goShape& other);
        virtual ~goShape ();

        goCurve*       getCurve ();
        const goCurve* getCurve () const;
        void           setCurve (goCurve*);
        bool           getWeights (goArray<goFloat>& weights) const;
        
        static bool align         (goCurve& curve, const goCurve& other);
        static bool center        (goCurve& curve);
        static bool normalize     (goCurve& curve);
        static bool procrustesFit (goList<goCurve*>& shapes, goCurve& meanShapeRet);

        virtual bool callObjectMethod (int methodID, goObjectMethodParameters* param = NULL);
        
    protected:
        virtual void receiveObjectMessage (const goObjectMessage& message);

    private:
        goShapePrivate* myPrivate;
};


#endif
