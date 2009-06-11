#include <gogui/glmaterialinput.h>

namespace goGUI
{
    class GLMaterialInputPrivate
    {
        public:
            GLMaterialInputPrivate () 
                : material (), 
                  callerChangedMaterial () {};
            ~GLMaterialInputPrivate () {};

            goGL::Material material;
            // sigc::signal<void> signalChangedMaterial;
            goCaller0<int> callerChangedMaterial;
    };

    static int GLMaterialInputV_[] = {4, 4, 4, 1, 4};
};


goGUI::GLMaterialInput::GLMaterialInput ()
    : MultiVectorInput (GLMaterialInputV_, 5)
{
    myPrivate = new GLMaterialInputPrivate;

    this->getInput (0).set_label ("Ambient");
    this->getInput (1).set_label ("Diffuse");
    this->getInput (2).set_label ("Specular");
    this->getInput (3).set_label ("Shininess");
    this->getInput (4).set_label ("Emission");

    this->set (myPrivate->material);

    // this->signalChanged().connect (sigc::mem_fun (*this, &GLMaterialInput::inputChangedSlotMaterial));
    this->show_all ();
}

goGUI::GLMaterialInput::~GLMaterialInput ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }

}

void goGUI::GLMaterialInput::set (const goGL::Material& m)
{
    this->getInput (0).setVector (m.myAmbient);
    this->getInput (1).setVector (m.myDiffuse);
    this->getInput (2).setVector (m.mySpecular);
    goVectorf temp(1);
    temp[0] = m.myShininess;
    this->getInput (3).setVector (temp);
    this->getInput (4).setVector (m.myEmission);
}

void goGUI::GLMaterialInput::get (goGL::Material& m)
{
    printf ("GLMaterialInput::get ()\n");
    this->getInput (0).getVector (m.myAmbient);
    this->getInput (1).getVector (m.myDiffuse);
    this->getInput (2).getVector (m.mySpecular);
    goVectorf temp(1);
    this->getInput (3).getVector (temp);
    m.myShininess = temp[0];
    this->getInput (4).getVector (m.myEmission);
}

int goGUI::GLMaterialInput::inputChangedSlotMaterial ()
{
    this->get (myPrivate->material);
    myPrivate->callerChangedMaterial ();
    return 0;
}

goCaller0<int>& goGUI::GLMaterialInput::callerChangedMaterial ()
{
    return myPrivate->callerChangedMaterial;
}
//sigc::signal<void>& goGUI::GLMaterialInput::signalChangedMaterial ()
//{
//    return myPrivate->signalChangedMaterial;
//}
