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
 * @addtogroup math
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
        goCurve (const goList<goMath::Vector<T> >&);
        goCurve (const goMath::Matrix<T>& confMatrix);
        virtual ~goCurve ();
        goCurve& operator= (const goCurve<T>&);


        virtual bool setPoints    (const goList<goMath::Vector<T> >&);
        virtual bool setPoints    (const goMath::Matrix<T>&);
        bool     setPoints        (typename goList<goMath::Vector<T> >::ConstElement* sourceBegin, 
                                   goIndex_t                              sourcePointCount, 
                                   goIndex_t                              destPointCount, 
                                   bool                                   closed = false);
        bool     setPoints        (typename goList<goMath::Vector<T> >::ConstElement* sourceBegin, goIndex_t sourcePointCount, bool closed = false);
        bool     resample         (goIndex_t pointCount, goList<goMath::Vector<T> >& ret) const;
        bool     resample         (goIndex_t pointCount, goCurve<T>& ret) const;
        // bool     resampleNUBS     (goIndex_t pointCount, goCurve<T>& ret) const;
        bool     getGradNorm      (goArray<goFloat>& diffNorm) const;
        bool     getGrad          (goList<goMath::Vector<T> >& diff) const;
        bool     getCurvNorm      (goArray<goFloat>& curvNorm) const;
        // bool     getAngleFunction (goArray<goFloat>& angles, const go4Vectorf& axis) const;
        bool     getTurningFunction (goMath::Vector<T>& ret) const;
        goDouble getLength        () const;

        // void     affineTransform  (const goMath::Matrix<T>& m);   // Now in pointcloud.

        goDouble euclideanDistance (const goCurve<T>& other, bool forward = true) const;

        static goDouble getLength (const goList<goMath::Vector<T> >& pl);

        bool sample (goDouble position, goMath::Vector<T>& ret) const;

        bool resample (goDouble start, goDouble end, goSize_t samples, goList<goMath::Vector<T> >& ret) const;

        // static bool resampleNUBS (typename goList<goMath::Vector<T> >::ConstElement* begin, typename goList<goMath::Vector<T> >::ConstElement* end, goIndex_t pointCount, goList<goMath::Vector<T> >& ret);
        // static bool resampleNUBS (typename goList<goMath::Vector<T> >::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<goMath::Vector<T> >& ret);
        static bool resample (typename goList<goMath::Vector<T> >::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<goMath::Vector<T> >& ret, bool closedCurve);
        static bool resampleLinear (typename goList<goMath::Vector<T> >::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<goMath::Vector<T> >& ret, bool closedCurve);
        
        static bool readASCII  (FILE* f, goList<goMath::Vector<T> >& ret);
        static bool writeASCII (FILE* f, const goList<goMath::Vector<T> >& ret);

        static goSize_t removeDuplicates (goList<goMath::Vector<T> >& pl);
        static goSize_t removeCloseDuplicates (goList<goMath::Vector<T> >& pl, goDouble epsilon);
        
        static bool     filter (const goFloat* mask, goSize_t size, goSize_t center, goList<goMath::Vector<T> >& pl, goSize_t count = 1);

        bool readASCII  (FILE* f);
        bool writeASCII (FILE* f) const;
        bool writeASCII (const char* filename) const;

        bool readASCIISimple (const char* filename, goSize_t dimension, bool closed);

        goSize_t removeDuplicates ();
        goSize_t removeCloseDuplicates (goDouble epsilon);

        
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
