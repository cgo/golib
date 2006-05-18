#ifndef GOCURVE_H
#define GOCURVE_H

#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOPOINT_H
# include <gopoint.h>
#endif
#ifndef GOPOINTCLOUD_H
# include <gopointcloud.h>
#endif
#ifndef GO4VECTOR_H
# include <go4vector.h>
#endif
#ifndef GOVECTOR_H
# include <govector.h>
#endif

class goCurvePrivate;

/**
 * @brief Curve representation.
 **/
template <class pointT>
class goCurve : public goPointCloud<pointT>
{
    public:
        goCurve ();
        goCurve (const goCurve<pointT>&);
        goCurve (const goList<pointT>&);
        virtual ~goCurve ();
        goCurve& operator= (const goCurve<pointT>&);

        virtual bool setPoints (const goList<pointT>&);
        bool     setPoints        (typename goList<pointT>::ConstElement* sourceBegin, 
                                   goIndex_t                              sourcePointCount, 
                                   goIndex_t                              destPointCount, 
                                   bool                                   closed = false);
        bool     setPoints        (typename goList<pointT>::ConstElement* sourceBegin, goIndex_t sourcePointCount, bool closed = false);
        bool     resample         (goIndex_t pointCount, goList<pointT>& ret) const;
        bool     resample         (goIndex_t pointCount, goCurve<pointT>& ret) const;
        bool     resampleNUBS     (goIndex_t pointCount, goCurve<pointT>& ret) const;
        bool     getGradNorm      (goArray<goFloat>& diffNorm) const;
        bool     getGrad          (goList<go4Vectorf>& diff) const;
        bool     getCurvNorm      (goArray<goFloat>& curvNorm) const;
        bool     getAngleFunction (goArray<goFloat>& angles, const go4Vectorf& axis) const;
        bool     getTurningFunction (goVectord& ret) const;
        goDouble getLength        () const;

        void     affineTransform  (const go44Matrixd& m);

        goDouble euclideanDistance (const goCurve<pointT>& other, bool forward = true) const;
        
        static bool resampleNUBS (typename goList<pointT>::ConstElement* begin, typename goList<pointT>::ConstElement* end, goIndex_t pointCount, goList<pointT>& ret);
        static bool resampleNUBS (typename goList<pointT>::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<pointT>& ret);
        static bool resample (typename goList<pointT>::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<pointT>& ret, bool closedCurve);
        static bool resampleLinear (typename goList<pointT>::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<pointT>& ret, bool closedCurve);
        
        static bool readASCII  (FILE* f, goList<pointT>& ret);
        static bool writeASCII (FILE* f, const goList<pointT>& ret);

        static goSize_t removeDuplicates (goList<pointT>& pl);

        bool readASCII  (FILE* f);
        bool writeASCII (FILE* f) const;

        goSize_t removeDuplicates ();

        
        virtual bool callObjectMethod (int methodID, goObjectMethodParameters* param = NULL);

        virtual bool writeObjectFile (FILE* f) const;
        virtual bool readObjectFile  (FILE* f);
        
    protected:
        virtual void receiveObjectMessage (const goObjectMessage& message);

    private:
        goCurvePrivate* myPrivate;
};

typedef goCurve<goPointf> goCurvef;
typedef goCurve<goPointd> goCurved;
#endif
