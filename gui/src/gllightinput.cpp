/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogui/gllightinput.h>

namespace goGUI
{
    class GLLightInputPrivate
    {
        public:
            GLLightInputPrivate () 
                : light (), 
                  callerChangedLight ()
                  // signalChangedLight () 
            {};
            ~GLLightInputPrivate () {};

            goGL::Light light;
            // sigc::signal<void, goGL::Light> signalChangedLight;
            goCaller0<int> callerChangedLight;
    };

    static int GLLightInputV_[] = {4, 4, 4, 4};
};


goGUI::GLLightInput::GLLightInput ()
    : MultiVectorInput (GLLightInputV_, 4)
{
    myPrivate = new GLLightInputPrivate;

    this->getInput (0).set_label ("Position");
    this->getInput (1).set_label ("Ambient");
    this->getInput (2).set_label ("Specular");
    this->getInput (3).set_label ("Diffuse");

    this->set (myPrivate->light);

    // this->signalChanged().connect (sigc::mem_fun (*this, &GLLightInput::inputChangedSlotLight));
    this->callerChanged().connect (goMemberFunction<int, GLLightInput> (this, &GLLightInput::inputChangedSlotLight));
    this->show_all ();
}

goGUI::GLLightInput::~GLLightInput ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }

}

void goGUI::GLLightInput::set (const goGL::Light& light)
{
    this->getInput (0).setVector (light.myPosition);
    this->getInput (1).setVector (light.myAmbient);
    this->getInput (2).setVector (light.mySpecular);
    this->getInput (3).setVector (light.myDiffuse);
}

void goGUI::GLLightInput::get (goGL::Light& light)
{
    this->getInput (0).getVector (light.myPosition);
    this->getInput (1).getVector (light.myAmbient);
    this->getInput (2).getVector (light.mySpecular);
    this->getInput (3).getVector (light.myDiffuse);
}

int goGUI::GLLightInput::inputChangedSlotLight ()
{
    // this->get (myPrivate->light);
    // myPrivate->signalChangedLight (myPrivate->light);
    myPrivate->callerChangedLight ();
}

//sigc::signal<void, goGL::Light>& goGUI::GLLightInput::signalChangedLight ()
//{
//    return myPrivate->signalChangedLight;
//}

goCaller0<int>& goGUI::GLLightInput::callerChangedLight ()
{
    return myPrivate->callerChangedLight;
}
