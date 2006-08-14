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
* \addtogroup shape
* @{
*/
/**
* @brief Point cloud.
*
* Points are represented using goVector<T> objects.
* 
* @note The only currently supported template parameters are 
*       goFloat and goDouble. 
*
* @todo Add function to create a configuration matrix goMatrix (N,getDim()) and/or a configuration vector
* from a point cloud.
**/
template <class T>
class goPointCloud : public goObjectBase
{
    public:
        goPointCloud (goSize_t dim = 2);
        goPointCloud (const goPointCloud<T>&);
        goPointCloud (const goList<goVector<T> >&);
        virtual ~goPointCloud ();
        goPointCloud<T>& operator= (const goPointCloud<T>&);

        bool operator!= (const goPointCloud<T>& other) const;
        bool operator== (const goPointCloud<T>& other) const;

        goIndex_t                   getPointCount () const;
        goList<goVector<T> >&       getPoints ();
        const goList<goVector<T> >& getPoints () const;
        virtual bool                setPoints (const goList<goVector<T> >&);
        void                        addPoint (const goVector<T>& p);

        void              setChanged         ();

        goSize_t          getDim             () const;

        bool              getMean            (goVector<T>& mean) const;
        bool              getCenterOfMass    (goVector<T>& comRet) const;
        bool              translate          (const goVector<T>& d);
        bool              getPrincipalAxes2D (goVectorf& a1, goVectorf& a2, const goArray<goFloat>* weights = 0) const;
        bool              unitScale          (goFloat factor = 1.0f);

        template<class matrixT> inline bool transform (const matrixT& m)
        {
            typename goList<goVector<T> >::Element* el = this->getPoints().getFrontElement();
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

        void    affineTransform (const goMatrix<T>& m);

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
