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

class goPointCloudPrivate;

class goPointCloud : public goObjectBase
{
    public:
        goPointCloud ();
        goPointCloud (const goPointCloud&);
        virtual ~goPointCloud ();
        goPointCloud& operator= (const goPointCloud&);

        bool operator!= (const goPointCloud& other);
        
        goList<goPointf>&       getPoints ();
        const goList<goPointf>& getPoints () const;
        void              setPoints (goList<goPointf>&);

        bool              getCenterOfMass    (goPointf& comRet) const;
        bool              translate          (const goPointf& d);
        bool              transform          (const go44Matrixf& m);
        bool              getPrincipalAxes2D (go4Vectorf& a1, go4Vectorf& a2, const goArray<goFloat>* weights = 0) const;
        bool              unitScale          (goFloat factor = 1.0f);
        
        virtual bool writeObjectFile (FILE*) const;
        virtual bool readObjectFile  (FILE*);
        
        virtual bool callObjectMethod (int methodID, goObjectMethodParameters* param = NULL);
        
    protected:
        virtual void receiveObjectMessage (const goObjectMessage& message);

    private:
        goPointCloudPrivate* myPrivate;
};

#endif
