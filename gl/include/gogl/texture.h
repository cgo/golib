#ifndef GOGL_TEXTURE_H
#define GOGL_TEXTURE_H

#include <GL/gl.h>
#include <gosignal3d.h>

namespace goGL
{
/** @addtogroup gl
 * @{
 */
    /** 
     * @brief Class representing a texture object in OpenGL.
     */
    class Texture
    {
        public:
            Texture (const goSignal3DBase<void>& image);
            virtual ~Texture ();

            GLuint getName () const;

        protected:
            void init (const goSignal3DBase<void>& image);

        private:
            GLuint myName;
    };
/** 
 * @}
 */
};

#endif
