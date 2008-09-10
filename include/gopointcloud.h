#ifndef GOPOINTCLOUD_H
#define GOPOINTCLOUD_H

#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOPOINT_H
# include <gopoint.h>
#endif
#ifndef GOLIST_H
# include <golist.h>
#endif
#ifndef GO44MATRIX_H
# include <go44matrix.h>
#endif
#ifndef GOMATRIX_H
# include <gomatrix.h>
#endif
#ifndef GOVECTOR_H
# include <govector.h>
#endif

template<class T> class goPointCloudPrivate;

/**
* \addtogroup math
* @{
*/
/**
* @brief Point cloud.
*
* Points are represented using goMath::Vector<T> objects.
* 
* @note The only currently supported template parameters are 
*       goFloat and goDouble. 
*
* @todo Add function to create a configuration matrix goMath::Matrix (N,getDim()) and/or a configuration vector
* from a point cloud.
**/
template <class T>
class goPointCloud : public goObjectBase
{
    public:
        goPointCloud (goSize_t dim = 2);
        goPointCloud (const goPointCloud<T>&);
        goPointCloud (const goList<goMath::Vector<T> >&);
        goPointCloud (const goMath::Matrix<T>&);
        virtual ~goPointCloud ();
        goPointCloud<T>& operator= (const goPointCloud<T>&);

        bool operator!= (const goPointCloud<T>& other) const;
        bool operator== (const goPointCloud<T>& other) const;

        goIndex_t                   getPointCount () const;
        goList<goMath::Vector<T> >&       getPoints ();
        const goList<goMath::Vector<T> >& getPoints () const;
        virtual bool                setPoints (const goList<goMath::Vector<T> >&);
        virtual bool                setPoints (const goMath::Matrix<T>&);
        void                        addPoint (const goMath::Vector<T>& p);

        void              setChanged         ();

        goSize_t          getDim             () const;

        // bool              getMean            (goMath::Vector<T>& mean) const;
        bool              getCenterOfMass    (goMath::Vector<T>& comRet) const;
        static bool       getCenterOfMass    (const goFixedArray<goMath::Vector<T> >&, goMath::Vector<T>& comRet);
        bool              translate          (const goMath::Vector<T>& d);
        bool              scale              (T s);
        void              getConfigurationMatrix (goMath::Matrix<T>& cmRet) const;
        void              getConfigurationVector (goMath::Vector<T>& cvRet) const;

        bool              getPrincipalAxes2D (goMath::Vectorf& a1, goMath::Vectorf& a2, const goArray<goFloat>* weights = 0) const; // REPLACE THIS WITH GENERIC
        bool              getPrincipalAxes   (goMath::Matrix<T>& axes) const;
        static bool       getPrincipalAxes   (const goFixedArray<goMath::Vector<T> >&, goMath::Matrix<T>& axes);
        bool              unitScale          (goFloat factor = 1.0f);

        template<class matrixT> inline bool transform (const matrixT& m)
        {
            typename goList<goMath::Vector<T> >::Element* el = this->getPoints().getFrontElement();
            goIndex_t sz = this->getPointCount();
            goIndex_t i = 0;
            while (el && i < sz)
            {
                el->elem *= m;
                el = el->next;
                ++i;
            }
            return true;
        };

        void    affineTransform (const goMath::Matrix<T>& m);

        static bool readASCII (const char* filename, goSize_t dimension, goList<goMath::Vector<T> >& pointList);

        virtual bool writeObjectFile (FILE*) const;
        virtual bool readObjectFile  (FILE*);
        
        virtual bool callObjectMethod (int methodID, goObjectMethodParameters* param = NULL);
        
    protected:
        virtual void receiveObjectMessage (const goObjectMessage& message);

    private:
        goPointCloudPrivate<T>* myPrivate;
};

typedef goPointCloud<goFloat> goPointCloudf;

/** @} */
#endif
