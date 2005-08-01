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
class goCurve : public goPointCloud
{
    public:
        goCurve ();
        goCurve (const goCurve&);
        virtual ~goCurve ();
        goCurve& operator= (const goCurve&);

        bool resample    (goIndex_t pointCount, goCurve& ret);
        bool getGradNorm (goArray<goFloat>& diffNorm) const;
        bool getGrad     (goList<go4Vectorf>& diff) const;
        bool getCurvNorm (goArray<goFloat>& curvNorm) const;
        bool getAngleFunction (goArray<goFloat>& angles, const go4Vectorf& axis) const;
        
        // bool operator!= (const goCurve& other);
        
        virtual bool callObjectMethod (int methodID, goObjectMethodParameters* param = NULL);

        virtual bool writeObjectFile (FILE* f) const;
        virtual bool readObjectFile  (FILE* f);
        
    protected:
        virtual void receiveObjectMessage (const goObjectMessage& message);

    private:
        goCurvePrivate* myPrivate;
};

#endif
