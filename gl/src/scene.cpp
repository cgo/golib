#include <gogl/scene.h>
#include <gogl/meshobject.h>
#include <gofixedarray.h>
#include <gofileio.h>

namespace goGL
{
    class ScenePrivate
    {
        public:
            ScenePrivate ()
                : drawables (),
                  lights (7),
                  camera (),
                  ambient (0)
            {
                ambient.setData (_ambient, 4, 1);
                ambient.fill (0.4f);
                ambient[3] = 1.0f;

                GLenum lightsenums[] = {
                    GL_LIGHT0,
                    GL_LIGHT1,
                    GL_LIGHT2,
                    GL_LIGHT3,
                    GL_LIGHT4,
                    GL_LIGHT5,
                    GL_LIGHT6};

                goSize_t sz = lights.getSize();
                for (goSize_t i = 0; i < sz; ++i)
                {
                    lights[i] = goAutoPtr<goGL::Light> (new goGL::Light (lightsenums[i]));
                }
                camera = goAutoPtr<goGL::Camera> (new goGL::Camera);
            };

            ~ScenePrivate ()
            {};

            goList<goAutoPtr<goGL::DrawableObject> > drawables;
            goFixedArray<goAutoPtr<goGL::Light> >    lights;
            goAutoPtr<goGL::Camera>                  camera;

            goFloat _ambient[4];
            goVectorf ambient;
    };
};

goGL::Scene::Scene ()
    : Object (),
      myPrivate (0)
{
    myPrivate = new ScenePrivate;
}

goGL::Scene::~Scene ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

bool goGL::Scene::render ()
{
    const goSize_t N_lights = this->getLightCount();

    glMatrixMode (GL_MODELVIEW);
    glLoadIdentity ();

    //=
    //= Set background colour, clear depth buffer
    //= 
    glClearColor (0.0f, 0.0f, 0.0f, 0.0f);
    glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glDisable (GL_CULL_FACE);
    glEnable (GL_DEPTH_TEST);

    //=
    //= Set global ambient light
    //=
    //= Two-sided lighting
    glLightModeli (GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
    glLightModelfv (GL_LIGHT_MODEL_AMBIENT, myPrivate->ambient.getPtr());

    //= Set projection and viewing transformation, and viewport
    (*this->getCamera())();
    printf ("camera: \n");
    this->getCamera()->myPosition.print();
    this->getCamera()->myLookat.print();
    this->getCamera()->myUp.print();

    //= FIXME: Calling the lights each render is not necessary --
    //=        add an extra lighting method later and
    //=        set here only the positions.
    for (goSize_t i = 0; i < N_lights; ++i)
    {
        if (this->getLight(i)->myEnabled)
        {
            glEnable (this->getLight(i)->myLightEnum);
            (*this->getLight(i))();
        }
        else
        {
            glDisable (this->getLight(i)->myLightEnum);
        }
    }

    //= Draw all objects
    goList<goAutoPtr<goGL::DrawableObject> >::Element* el = myPrivate->drawables.getFrontElement();
    while (el)
    {
        (*el->elem)();
        el = el->next;
    }
    
    printf ("Render done.\n");

    //= Done!
    return true;
}

bool goGL::Scene::add (goAutoPtr<goGL::DrawableObject> o)
{
    return myPrivate->drawables.append (o);
}

goSize_t goGL::Scene::getObjectCount () const
{
    return myPrivate->drawables.getSize ();
}

goAutoPtr<goGL::DrawableObject> goGL::Scene::getObject (goSize_t i)
{
    if (i < this->getObjectCount())
    {
        return myPrivate->drawables(i)->elem;
    }
    return goAutoPtr<goGL::DrawableObject> (0);
}

goSize_t goGL::Scene::getLightCount () const
{
    return myPrivate->lights.getSize ();
}

goAutoPtr<goGL::Light> goGL::Scene::getLight (goSize_t i)
{
    if (i < this->getLightCount())
    {
        return myPrivate->lights[i];
    }
    return goAutoPtr<goGL::Light> (0);
}

goAutoPtr<goGL::Camera> goGL::Scene::getCamera ()
{
    return myPrivate->camera;
}

bool goGL::Scene::writeASCII (FILE* f) const
{
    goString s = "goGL::Scene\n";
    bool ok = goFileIO::writeASCII (f, s);
    s = "objects\n";
    ok = ok && goFileIO::writeASCII (f, s);
    s = "";
    s += (int) this->getObjectCount ();
    s += "\n";
    ok = ok && goFileIO::writeASCII (f, s);
    for (goSize_t i = 0; i < this->getObjectCount(); ++i)
    {
        ok = ok && const_cast<goGL::Scene*>(this)->getObject (i)->writeASCII (f);
    }
    
    s = "lights\n";
    ok = ok && goFileIO::writeASCII (f, s);
    s = "";
    s += (int) this->getLightCount ();
    s += "\n";
    ok = ok && goFileIO::writeASCII (f, s);
    for (goSize_t i = 0; i < this->getLightCount(); ++i)
    {
        ok = ok && const_cast<goGL::Scene*>(this)->getLight (i)->writeASCII (f);
    }

    s = "camera\n";
    ok = ok && goFileIO::writeASCII (f, s);
    ok = ok && const_cast<goGL::Scene*>(this)->getCamera ()->writeASCII (f);

    return ok;
}

static inline bool CHECK_LINE (FILE* f, goString& s, const char* sought) {
    bool ok = goFileIO::readASCIILine (f, s);
    if (s != sought) 
    {
        goString t = "goGL::Scene::readASCII(): expected ";
        t += sought;
        t += ", got ";
        t += s;
        goLog::warning (t);
        return false;
    }
    return ok;
}

bool goGL::Scene::readASCII (FILE* f)
{
    bool ok = true;
    goString s;
    if (!CHECK_LINE (f, s, "goGL::Scene"))
        return false;
    if (!CHECK_LINE (f, s, "objects"))
        return false;
    goFileIO::readASCIILine (f, s);
    goSize_t N_obj = s.toInt();
    
    myPrivate->drawables.erase ();
    // myPrivate->drawables.resize (N_obj);

    for (goSize_t i = 0; i < N_obj; ++i)
    {
        // FIXME: When there are more different objects, 
        //        change this.
        goAutoPtr<goGL::DrawableObject> obj (new goGL::MeshObject);
        myPrivate->drawables.append (obj);
        ok = ok && obj->readASCII (f);
        if (!ok)
        {
            goString temp = "goGL::Scene::readASCII(): reading object ";
            temp += (int)i;
            temp += " failed.";
            goLog::warning (temp);
        }
    }

    if (!CHECK_LINE (f, s, "lights"))
        return false;
    goFileIO::readASCIILine (f, s);
    goSize_t N_lights = s.toInt();

    myPrivate->lights.setSize (0);
    myPrivate->lights.setSize (N_lights);

    for (goSize_t i = 0; i < N_lights; ++i)
    {
        myPrivate->lights[i] = goAutoPtr<goGL::Light> (new goGL::Light);
        ok = ok && myPrivate->lights[i]->readASCII (f);
        if (!ok)
        {
            goString temp = "goGL::Scene::readASCII(): reading light ";
            temp += (int)i;
            temp += " failed.";
            goLog::warning (temp);
        }
    }

    if (!CHECK_LINE (f, s, "camera"))
        return false;

    ok = ok && this->getCamera()->readASCII (f);

    return ok;
}

bool goGL::Scene::writeASCII (const char* filename) const
{
    return goGL::Object::writeASCII (filename);
}

bool goGL::Scene::readASCII (const char* filename)
{
    return goGL::Object::readASCII (filename);
}
