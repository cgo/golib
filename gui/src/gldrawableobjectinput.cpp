#include <gogui/gldrawableobjectinput.h>
#include <gogl/material.h>
#include <gogui/glmaterialinput.h>

namespace goGUI
{
    class GLDrawableObjectInputPrivate
    {
        public:
            GLDrawableObjectInputPrivate () 
                : materialInput (), material (), 
                callerChanged (), object (0) {};
            ~GLDrawableObjectInputPrivate () {};

            goGUI::GLMaterialInput materialInput;
            goGL::Material         material;

            // sigc::signal<void> signalChanged;
            goCaller0<int> callerChanged;

            goAutoPtr<goGL::DrawableObject> object;
    };
};

goGUI::GLDrawableObjectInput::GLDrawableObjectInput ()
    : GLObjectInput (),
      myPrivate (0)
{
    myPrivate = new GLDrawableObjectInputPrivate;
    this->getBox()->pack_start (myPrivate->materialInput);
    myPrivate->materialInput.set (myPrivate->material);
    // this->signalObjectInputChanged().connect (sigc::mem_fun (*this, &GLDrawableObjectInput::inputChangedSlotDrawableObject));
    this->callerObjectInputChanged().connect (goMemberFunction<GLDrawableObjectInput,int> (this, &GLDrawableObjectInput::inputChangedSlotDrawableObject));
    // myPrivate->materialInput.signalChanged().connect (sigc::mem_fun (*this, &GLDrawableObjectInput::inputChangedSlotDrawableObject));
    myPrivate->materialInput.callerChanged().connect (goMemberFunction<GLDrawableObjectInput,int> (this, &GLDrawableObjectInput::inputChangedSlotDrawableObject));
}

goGUI::GLDrawableObjectInput::~GLDrawableObjectInput ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

/** 
 * @brief Set the drawable.
 * 
 * Changes in the input will be written into the object \c o.
 *
 * @param o Object to which the input is bound.
 */
void goGUI::GLDrawableObjectInput::setDrawableObject (goAutoPtr<goGL::DrawableObject> o)
{
    myPrivate->object = o;
    goAutoPtr<goGL::Object> o2;
    o2 = o;
    GLObjectInput::setObject (o2);
    if (!o.isNull())
        this->setDrawable (*o);
}

/** 
 * @brief Get the drawable.
 * 
 * @return Autopointer to the drawable object. May be a null pointer.
 */
goAutoPtr<goGL::DrawableObject> goGUI::GLDrawableObjectInput::getDrawableObject ()
{
    return myPrivate->object;
}

/** 
 * @brief Put properties from the object set with \c setDrawableObject() into the inputs.
 */
void goGUI::GLDrawableObjectInput::updateInput ()
{
    if (!myPrivate->object.isNull())
        this->setDrawable (*myPrivate->object);
}

/** 
 * @brief Put properties from object \c o into the inputs.
 * 
 * @param o Object to read from.
 */
void goGUI::GLDrawableObjectInput::setDrawable (const goGL::DrawableObject& o)
{
    GLObjectInput::set (o);
    myPrivate->materialInput.set (o.getMaterial ());
}

/** 
 * @brief Read out inputs and put them into \c o.
 * 
 * @param o Object to read into.
 */
void goGUI::GLDrawableObjectInput::getDrawable (goGL::DrawableObject& o)
{
    GLObjectInput::get (o);
    myPrivate->materialInput.get (o.getMaterial ());
}

/** 
 * @brief Connected to signalObjectInputChanged() and to the material input's signalChanged().
 *
 * Updates the object and emits \c signalDrawableObjectInputChanged().
 */
int goGUI::GLDrawableObjectInput::inputChangedSlotDrawableObject ()
{
    if (!myPrivate->object.isNull())
        this->getDrawable (*myPrivate->object);

    myPrivate->callerChanged ();
}

/** 
 * @brief Emitted whenever an input changes.
 * 
 * @return Signal.
 */
goCaller0<int>& goGUI::GLDrawableObjectInput::callerDrawableObjectInputChanged ()
{
    return myPrivate->callerChanged;
}

//sigc::signal<void>& goGUI::GLDrawableObjectInput::signalDrawableObjectInputChanged ()
//{
//    return myPrivate->signalChanged;
//}

