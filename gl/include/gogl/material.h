#ifndef GOGL_MATERIAL_H
#define GOGL_MATERIAL_H

#include <GL/gl.h>
#include <goobjectbase.h>
#ifndef GOVECTOR_H
# include <govector.h>
#endif

namespace goGL
{
/** @addtogroup gl
 * @{
 */
    /** 
     * @brief Material object for OpenGL.
     */
    class Material : public goObjectBase
    {
        public:
            Material ();
            virtual ~Material ();

            virtual bool operator() () const;

            bool writeASCII (FILE* f) const;
            bool writeASCII (const char* filename) const;
            bool readASCII (FILE* f);
            bool readASCII (const char* filename);

            goVectorf myAmbient;
            goVectorf mySpecular;
            goVectorf myDiffuse;
            goFloat   myShininess;
            goVectorf myEmission;
            GLenum    myFace;
    };
/** 
 * @}
 */
};

#endif
