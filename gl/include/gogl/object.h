#ifndef GOGL_OBJECT_H
#define GOGL_OBJECT_H

#include <goobjectbase.h>
#ifndef GOVECTOR_H
# include <govector.h>
#endif

namespace goGL
{
    class ObjectPrivate;

    class Object : public goObjectBase
    {
        public:
            Object ();
            virtual ~Object ();

            Object (const Object& o);
            Object& operator= (const Object& o);

            void setRotation (goFloat angle, goFloat x, goFloat y, goFloat z);
            void setRotation (const goVectorf& r);
            void setRotation (goFloat angle, const goVectorf& n);

            void setScale (goFloat x, goFloat y, goFloat z);
            void setScale (const goVectorf& t);

            void setTranslation (goFloat x, goFloat y, goFloat z);
            void setTranslation (const goVectorf& t);

            const goVectorf& getRotation () const;
            const goVectorf& getScale () const;
            const goVectorf& getTranslation () const;

            virtual bool writeASCII (FILE* f) const;
            virtual bool writeASCII (const char* filename) const;

            virtual bool readASCII (FILE* f);
            virtual bool readASCII (const char* filename);

        private:
            ObjectPrivate* myPrivate;
    };
};

#endif
