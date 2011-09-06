/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogui/gldrawableobjectinput.h>
#include <gogl/material.h>
#include <gogui/glmaterialinput.h>
#include <gtkmm.h>

namespace goGUI
{
    class GLDrawableObjectInputPrivate
    {
        public:
            GLDrawableObjectInputPrivate () 
                : materialInput (), material (), 
                shadeModelInput (),
                shadeModelConnection (),
                callerChanged (), 
                object (0)
            {
            };

            ~GLDrawableObjectInputPrivate () {};

            goGUI::GLMaterialInput materialInput;
            goGL::Material         material;
            Gtk::ComboBoxText      shadeModelInput;
            sigc::connection       shadeModelConnection;

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
    {
        myPrivate->shadeModelInput.append_text ("Smooth");
        myPrivate->shadeModelInput.append_text ("Flat");
        myPrivate->shadeModelInput.set_active (0);
        Gtk::HBox* hbox = Gtk::manage (new Gtk::HBox);
        Gtk::Label* l = Gtk::manage (new Gtk::Label);
        l->set_text ("Shade model: ");
        hbox->pack_start (*l, Gtk::PACK_SHRINK);
        hbox->pack_start (myPrivate->shadeModelInput, Gtk::PACK_SHRINK);
        this->getBox()->pack_start (*hbox);
        hbox->show_all ();
    }
    myPrivate->materialInput.set (myPrivate->material);
    // this->signalObjectInputChanged().connect (sigc::mem_fun (*this, &GLDrawableObjectInput::inputChangedSlotDrawableObject));
    this->callerObjectInputChanged().connect (goMemberFunction<int, GLDrawableObjectInput> (this, &GLDrawableObjectInput::inputChangedSlotDrawableObject));
    // myPrivate->materialInput.signalChanged().connect (sigc::mem_fun (*this, &GLDrawableObjectInput::inputChangedSlotDrawableObject));
    myPrivate->materialInput.callerChanged().connect (goMemberFunction<int, GLDrawableObjectInput> (this, &GLDrawableObjectInput::inputChangedSlotDrawableObject));
    myPrivate->shadeModelConnection = myPrivate->shadeModelInput.signal_changed().connect (sigc::mem_fun (*this, &GLDrawableObjectInput::inputChangedShadeModel));
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
    //= If we do not disconnect here, an application may crash.
    myPrivate->shadeModelConnection.disconnect ();
    GLObjectInput::set (o);
    myPrivate->materialInput.set (o.getMaterial ());
    switch (o.getShadeModel())
    {
        case GL_SMOOTH: myPrivate->shadeModelInput.set_active (0); break;
        case GL_FLAT: myPrivate->shadeModelInput.set_active (1); break;
        default: goLog::warning ("goGUI::GLDrawableObjectInput::setDrawable (): unknown shade model.");
    }
    myPrivate->shadeModelConnection = myPrivate->shadeModelInput.signal_changed().connect (sigc::mem_fun (*this, &GLDrawableObjectInput::inputChangedShadeModel));
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
    switch (myPrivate->shadeModelInput.get_active_row_number ())
    {
        case 0: o.setShadeModel (GL_SMOOTH); break;
        case 1: o.setShadeModel (GL_FLAT); break;
        default: goLog::warning ("goGUI::GLDrawableObjectInput::getDrawable (): unknown shade model.");
    }
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
    return 0;
}

void goGUI::GLDrawableObjectInput::inputChangedShadeModel ()
{
    //= Just relay
    this->inputChangedSlotDrawableObject ();
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

