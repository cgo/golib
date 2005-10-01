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
        virtual ~goCurve ();
        goCurve& operator= (const goCurve<pointT>&);

        bool     resample         (goIndex_t pointCount, goCurve<pointT>& ret);
        bool     getGradNorm      (goArray<goFloat>& diffNorm) const;
        bool     getGrad          (goList<go4Vectorf>& diff) const;
        bool     getCurvNorm      (goArray<goFloat>& curvNorm) const;
        bool     getAngleFunction (goArray<goFloat>& angles, const go4Vectorf& axis) const;
        goDouble getLength        () const;
        
        // bool operator!= (const goCurve& other);
        
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
