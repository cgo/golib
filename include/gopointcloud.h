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

template<class T> class goPointCloudPrivate;

/**
* \addtogroup shape
* @{
*/
/**
* @brief Point cloud.
*
* @note The only currently supported template parameters are 
*       goPoint<...>. Add more general vectors when needed.
*       Same holds for goCurve.
*
* @todo See note.
**/
template <class pointT>
class goPointCloud : public goObjectBase
{
    public:
        goPointCloud ();
        goPointCloud (const goPointCloud<pointT>&);
        goPointCloud (const goList<pointT>&);
        virtual ~goPointCloud ();
        goPointCloud& operator= (const goPointCloud&);

        bool operator!= (const goPointCloud& other) const;
        bool operator== (const goPointCloud& other) const;

        goIndex_t             getPointCount () const;
        goList<pointT>&       getPoints ();
        const goList<pointT>& getPoints () const;
        virtual bool          setPoints (const goList<pointT>&);
        void              addPoint (const pointT& p);

        void              setChanged         ();

        bool              getMean            (pointT& mean) const;
        bool              getCenterOfMass    (pointT& comRet) const;
        bool              translate          (const pointT& d);
        bool              getPrincipalAxes2D (go4Vectorf& a1, go4Vectorf& a2, const goArray<goFloat>* weights = 0) const;
        bool              unitScale          (goFloat factor = 1.0f);

        template<class matrixT> inline bool transform (const matrixT& m)
        {
            typename goList<pointT>::Element* el = this->getPoints().getFrontElement();
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

        virtual bool writeObjectFile (FILE*) const;
        virtual bool readObjectFile  (FILE*);
        
        virtual bool callObjectMethod (int methodID, goObjectMethodParameters* param = NULL);
        
    protected:
        virtual void receiveObjectMessage (const goObjectMessage& message);

    private:
        goPointCloudPrivate<pointT>* myPrivate;
};

typedef goPointCloud<goPointf> goPointCloudf;

/** @} */
#endif
