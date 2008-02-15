#ifndef GOGL_MESHOBJECT_H
#define GOGL_MESHOBJECT_H

#include <gogl/drawableobject.h>
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif
#ifndef GOVECTOR_H
# include <govector.h>
#endif
#ifndef GOMATRIX_H
# include <gomatrix.h>
#endif
#ifndef GOLIST_H
# include <golist.h>
#endif

namespace goGL
{
    class MeshObjectPrivate;

/** @addtogroup gl
 * @{
 */
    /** 
     * @brief A mesh object for OpenGL.
     */
    class MeshObject : public DrawableObject
    {
        public:
            MeshObject ();
            virtual ~MeshObject ();

            MeshObject (const MeshObject& o);
            MeshObject& operator= (const MeshObject& o);

            virtual bool operator () () const;

            // bool align ();
            // bool toList (int listName);
            virtual bool draw () const;
            virtual bool init ();

            goAutoPtr<goVectorf>          getMin () const;
            goAutoPtr<goVectorf>          getMax () const;
            goMatrixf&                    getVertices ();
            goFixedArray<goVector<int> >& getFaces ();
            const goMatrixf&              getVertices () const;
            const goFixedArray<goVector<int> >& getFaces () const;
            void                          getAdjacencyLists (goFixedArray<goList<int> >& ret) const;
            void                          getAdjacentFaces (goFixedArray<goList<int> >& ret) const;
            goMatrixf&                    getNormals ();
            goMatrixf&                    getFaceNormals ();
            const goMatrixf&              getNormals () const;
            const goMatrixf&              getFaceNormals () const;
           
            void calculateNormals ();
            void calculateNormals (goMatrixf& normals, goMatrixf& face_normals) const;
            void calculateFaceNormals (goMatrixf& face_normals) const;

            bool removeDoubles ();

            void setFilename (const char* filename);
            const char* getFilename () const;

            virtual bool writeASCII (FILE* f) const;
            virtual bool readASCII (FILE* f);

        private:
            MeshObjectPrivate* myPrivate;
    };
/** 
 * @}
 */
};

#endif
