#include <gogl/object.h>

namespace goGL
{
    class ObjectPrivate
    {
        public:
            ObjectPrivate () 
                : rotation(0),
                  translation(0),
                  scale(0)
            {
                rotation.setData (_rotation, 4, 1);
                translation.setData (_translation, 3, 1);
                scale.setData (_scale, 3, 1);
                rotation.fill (0.0f);
                rotation[1] = 1.0f;
                translation.fill (0.0f);
                scale.fill (1.0f);
            };
            ~ObjectPrivate () {};

            goFloat _rotation[4];
            goFloat _translation[3];
            goFloat _scale[3];

            goVectorf rotation;  // 4: angle, x, y, z
            goVectorf translation; // 3
            goVectorf scale; // 3
    };
};

goGL::Object::Object ()
    : goObjectBase (),
      myPrivate (0)
{
    myPrivate = new ObjectPrivate;
}

goGL::Object::~Object ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

goGL::Object::Object (const Object& o)
    : goObjectBase (),
      myPrivate (0)
{
    myPrivate = new ObjectPrivate;
    *this = o;
}

goGL::Object& goGL::Object::operator= (const Object& o)
{
    this->setTranslation (o.getTranslation());
    this->setScale (o.getScale());
    this->setRotation (o.getRotation());

    return *this;
}

void goGL::Object::setRotation (goFloat angle, goFloat x, goFloat y, goFloat z)
{
    myPrivate->rotation[0] = angle;
    myPrivate->rotation[1] = x;
    myPrivate->rotation[2] = y;
    myPrivate->rotation[3] = z;
}

void goGL::Object::setRotation (const goVectorf& r)
{
    if (r.getSize() == 3)
    {
        goFloat s = r.norm2 ();
        if (s <= 0.0f)
        {
            myPrivate->rotation.fill (0.0f);
            myPrivate->rotation[1] = 1.0f;
            return;
        }
        myPrivate->rotation[0] = s;
        myPrivate->rotation[1] = r[0] / s;
        myPrivate->rotation[2] = r[1] / s;
        myPrivate->rotation[3] = r[2] / s;
    }
    else
    {
        if (r.getSize() == 4)
        {
            myPrivate->rotation = r;
        }
        else
        {
            goLog::error ("goGL::Object::setRotation(): Vector must be size 3 or 4.");
            return;
        }
    }
}

void goGL::Object::setRotation (goFloat angle, const goVectorf& n)
{
    if (n.getSize() != 3)
    {
        goLog::error ("goGL::Object::setRotation(): Axis must be size 3.");
        return;
    }
    myPrivate->rotation[0] = angle;
    myPrivate->rotation[1] = n[0];
    myPrivate->rotation[2] = n[1];
    myPrivate->rotation[3] = n[2];
}

void goGL::Object::setScale (const goVectorf& s)
{
    if (s.getSize() != 3)
    {
        goLog::error ("goGL::Object::setScale(): must be size 3.");
        return;
    }
    myPrivate->scale = s;
}

void goGL::Object::setScale (goFloat x, goFloat y, goFloat z)
{
    myPrivate->scale[0] = x;
    myPrivate->scale[1] = y;
    myPrivate->scale[2] = z;
}

void goGL::Object::setTranslation (goFloat x, goFloat y, goFloat z)
{
    myPrivate->translation[0] = x;
    myPrivate->translation[1] = y;
    myPrivate->translation[2] = z;
}

void goGL::Object::setTranslation (const goVectorf& t)
{
    if (t.getSize() != 3)
    {
        goLog::error ("goGL::Object::setTranslation(): Axis must be size 3.");
        return;
    }
    myPrivate->translation = t;
}

const goVectorf& goGL::Object::getRotation () const
{
    return myPrivate->rotation;
}

const goVectorf& goGL::Object::getScale () const
{
    return myPrivate->scale;
}

const goVectorf& goGL::Object::getTranslation () const
{
    return myPrivate->translation;
}

bool goGL::Object::writeASCII (FILE* f) const
{
    bool ok = this->getRotation().writeASCII (f);
    ok = ok && this->getScale().writeASCII (f);
    ok = ok && this->getTranslation().writeASCII (f);

    return ok;
}

bool goGL::Object::writeASCII (const char* filename) const
{
    FILE* f = ::fopen (filename, "w");
    if (!f)
        return false;
    bool ok = this->writeASCII (f);
    ::fclose (f);
    return ok;
}

bool goGL::Object::readASCII (FILE* f)
{
    bool ok = myPrivate->rotation.readASCII (f);
    if (!ok) return false;
    ok = myPrivate->scale.readASCII (f);
    if (!ok) return false;
    ok = myPrivate->translation.readASCII (f);
    if (!ok) return false;

    return ok;
}

bool goGL::Object::readASCII (const char* filename)
{
    FILE* f = ::fopen (filename, "r");
    if (!f)
        return false;
    bool ok = this->readASCII (f);
    ::fclose (f);
    return ok;
}
