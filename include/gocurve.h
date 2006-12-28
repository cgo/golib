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
 * @addtocurve math
 * @{
 */
/**
 * @brief Curve representation.
 **/
template <class T>
class goCurve : public goPointCloud<T>
{
    public:
        goCurve (goSize_t dim = 2);
        goCurve (const goCurve<T>&);
        goCurve (const goList<goVector<T> >&);
        virtual ~goCurve ();
        goCurve& operator= (const goCurve<T>&);

        virtual bool setPoints (const goList<goVector<T> >&);
        bool     setPoints        (typename goList<goVector<T> >::ConstElement* sourceBegin, 
                                   goIndex_t                              sourcePointCount, 
                                   goIndex_t                              destPointCount, 
                                   bool                                   closed = false);
        bool     setPoints        (typename goList<goVector<T> >::ConstElement* sourceBegin, goIndex_t sourcePointCount, bool closed = false);
        bool     resample         (goIndex_t pointCount, goList<goVector<T> >& ret) const;
        bool     resample         (goIndex_t pointCount, goCurve<T>& ret) const;
        bool     resampleNUBS     (goIndex_t pointCount, goCurve<T>& ret) const;
        bool     getGradNorm      (goArray<goFloat>& diffNorm) const;
        bool     getGrad          (goList<goVector<T> >& diff) const;
        bool     getCurvNorm      (goArray<goFloat>& curvNorm) const;
        bool     getAngleFunction (goArray<goFloat>& angles, const go4Vectorf& axis) const;
        bool     getTurningFunction (goVector<T>& ret) const;
        goDouble getLength        () const;

        // void     affineTransform  (const goMatrix<T>& m);   // Now in pointcloud.

        goDouble euclideanDistance (const goCurve<T>& other, bool forward = true) const;

        static goDouble getLength (const goList<goVector<T> >& pl);
        static bool resampleNUBS (typename goList<goVector<T> >::ConstElement* begin, typename goList<goVector<T> >::ConstElement* end, goIndex_t pointCount, goList<goVector<T> >& ret);
        static bool resampleNUBS (typename goList<goVector<T> >::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<goVector<T> >& ret);
        static bool resample (typename goList<goVector<T> >::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<goVector<T> >& ret, bool closedCurve);
        static bool resampleLinear (typename goList<goVector<T> >::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<goVector<T> >& ret, bool closedCurve);
        
        static bool readASCII  (FILE* f, goList<goVector<T> >& ret);
        static bool writeASCII (FILE* f, const goList<goVector<T> >& ret);

        static goSize_t removeDuplicates (goList<goVector<T> >& pl);
        
        static bool     filter (const goFloat* mask, goSize_t size, goSize_t center, goList<goVector<T> >& pl, goSize_t count = 1);

        bool readASCII  (FILE* f);
        bool writeASCII (FILE* f) const;

        bool readASCIISimple (const char* filename, goSize_t dimension, bool closed);

        goSize_t removeDuplicates ();

        
        virtual bool callObjectMethod (int methodID, goObjectMethodParameters* param = NULL);

        virtual bool writeObjectFile (FILE* f) const;
        virtual bool readObjectFile  (FILE* f);
        
    protected:
        virtual void receiveObjectMessage (const goObjectMessage& message);

    private:
        goCurvePrivate* myPrivate;
};

typedef goCurve<goFloat> goCurvef;
typedef goCurve<goDouble> goCurved;

/**
 * @}
 */
#endif
