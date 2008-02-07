#include <gogui/scenecontrol.h>
#include <gogui/helper.h>
#include <gogui/gldrawableobjectinput.h>
#include <gogui/glanimation.h>
#include <gtkmm.h>

#include <gogl/scene.h>

namespace goGUI
{
    class SceneControlPrivate
    {
        public:
            SceneControlPrivate ()
                : objectBox (),
                  lightBox (),
                  scene (0),
                  view (),
                  viewWindow (),
                  objectPropButton ("Object Properties"),
                  loadOFFButton ("Load OFF"),
                  saveSceneButton ("Save Scene"),
                  loadSceneButton ("Load Scene"),
                  addWaypointButton ("Add Waypoint"),
                  editWaypointButton ("Edit Selected Waypoint"),
                  editWaypointConnection1 (),
                  editWaypointConnection2 (),
                  objectInput (),
                  objectInputWindow (),
                  glanimation ()
            {
                scene.set (new goGL::Scene);
            };
            ~SceneControlPrivate () {};

            void updateBoxes ()
            {
                objectBox.clear ();
                objectBox.append_text ("None");
                for (goSize_t i = 0; i < scene->getObjectCount(); ++i)
                {
                    goString s = "Object ";
                    s += (int)i;
                    objectBox.append_text (s.toCharPtr());
                }
            };

            Gtk::ComboBoxText objectBox;
            Gtk::ComboBoxText lightBox;

            goAutoPtr<goGL::Scene> scene;
            goGUI::SceneView view;
            Gtk::Window viewWindow;

            Gtk::Button objectPropButton;
            Gtk::Button loadOFFButton;
            Gtk::Button saveSceneButton;
            Gtk::Button loadSceneButton;

            //= For animation
            Gtk::Button addWaypointButton;
            Gtk::CheckButton editWaypointButton;
            sigc::connection editWaypointConnection1;
            sigc::connection editWaypointConnection2;

            goGUI::GLDrawableObjectInput objectInput;
            Gtk::Window                  objectInputWindow;

            goGUI::GLAnimation glanimation;
    };
};

goGUI::SceneControl::SceneControl ()
    : Control ("Scene Control"),
      myPrivate (0)
{
    myPrivate = new SceneControlPrivate;

    myPrivate->updateBoxes ();
    myPrivate->objectBox.set_active (0);

    myPrivate->objectInputWindow.hide ();
    myPrivate->objectInputWindow.add (myPrivate->objectInput);
    myPrivate->objectInputWindow.set_title ("Object Properties");
    myPrivate->objectInput.show ();

    Gtk::VBox* myVBox = Gtk::manage (new Gtk::VBox);
    Gtk::HBox* myHBox = Gtk::manage (new Gtk::HBox);
    {
        Gtk::VBox* vbox = Gtk::manage (new Gtk::VBox);
        vbox->pack_start (myPrivate->objectBox, Gtk::PACK_SHRINK);
        vbox->pack_start (myPrivate->lightBox, Gtk::PACK_SHRINK);
        myHBox->pack_start (*vbox, Gtk::PACK_SHRINK);
    }
    {
        Gtk::VBox* vbox = Gtk::manage (new Gtk::VBox);
        Gtk::HBox* hbox = Gtk::manage (new Gtk::HBox);
        hbox->pack_start (myPrivate->objectPropButton, Gtk::PACK_SHRINK);
        hbox->pack_start (myPrivate->loadOFFButton, Gtk::PACK_SHRINK);
        vbox->pack_start (*hbox);

        hbox = Gtk::manage (new Gtk::HBox);
        hbox->pack_start (myPrivate->saveSceneButton, Gtk::PACK_SHRINK);
        hbox->pack_start (myPrivate->loadSceneButton, Gtk::PACK_SHRINK);
        vbox->pack_start (*hbox);

        hbox = Gtk::manage (new Gtk::HBox);
        hbox->pack_start (myPrivate->addWaypointButton, Gtk::PACK_SHRINK);
        hbox->pack_start (myPrivate->editWaypointButton, Gtk::PACK_SHRINK);
        vbox->pack_start (*hbox);

        myHBox->pack_start (*vbox, Gtk::PACK_SHRINK);
    }
    {
        myPrivate->lightBox.append_text ("GL_LIGHT0");
        myPrivate->lightBox.append_text ("GL_LIGHT1");
        myPrivate->lightBox.append_text ("GL_LIGHT2");
    }

    {
        myPrivate->glanimation.signalWaypointSelected().connect (sigc::mem_fun (*this, &SceneControl::transformToSelectedWaypoint));
        // myPrivate->view.signalChanged().connect (sigc::mem_fun (*this, &SceneControl::editWaypoint));
    }

    {
        myPrivate->objectBox.signal_changed().connect (sigc::mem_fun (*this, &SceneControl::objectBoxChangedSlot));
        myPrivate->lightBox.signal_changed().connect (sigc::mem_fun (*this, &SceneControl::lightBoxChangedSlot));
    }
    {
        myPrivate->objectPropButton.signal_clicked().connect (sigc::mem_fun (*this, &SceneControl::objectProperties));
        myPrivate->objectInput.signalDrawableObjectInputChanged().connect (sigc::mem_fun (*this, &SceneControl::objectPropChanged));
        myPrivate->loadOFFButton.signal_clicked().connect (sigc::mem_fun (*this, &SceneControl::loadOFF));

        myPrivate->saveSceneButton.signal_clicked().connect (sigc::mem_fun (*this, &SceneControl::saveScene));
        myPrivate->loadSceneButton.signal_clicked().connect (sigc::mem_fun (*this, &SceneControl::loadScene));
        myPrivate->addWaypointButton.signal_clicked().connect (sigc::mem_fun (*this, &SceneControl::addWaypoint));
        myPrivate->editWaypointButton.signal_toggled().connect (sigc::mem_fun (*this, &SceneControl::editWaypointToggled));

        myPrivate->glanimation.signalPositionChanged().connect (sigc::mem_fun (*this, &SceneControl::animationPositionChanged));
    }

    myPrivate->viewWindow.add (myPrivate->view);
    myPrivate->view.setScene (myPrivate->scene);
    myPrivate->view.show ();
    myPrivate->viewWindow.show ();

    myVBox->pack_start (*myHBox, Gtk::PACK_SHRINK);

    this->signal_hide().connect (sigc::mem_fun (*this, &SceneControl::onHide));
    this->signal_show().connect (sigc::mem_fun (*this, &SceneControl::onShow));

    myPrivate->view.signalChanged().connect (sigc::mem_fun (*this, &SceneControl::viewChanged));

    {
        myVBox->pack_start (myPrivate->glanimation);
        myPrivate->glanimation.setAnimation (goAutoPtr<goGL::Animation> (new goGL::Animation));
    }

    this->add (*myVBox);
    this->show_all ();
}

void goGUI::SceneControl::objectProperties ()
{
    // Show object dialog
    myPrivate->objectInputWindow.show ();
}

void goGUI::SceneControl::objectPropChanged ()
{
    myPrivate->view.queue_draw();
    return;
}

goGUI::SceneView& goGUI::SceneControl::getSceneView ()
{
    return myPrivate->view;
}

void goGUI::SceneControl::viewChanged ()
{
    //= set current object's properties
    int activeObject = myPrivate->objectBox.get_active_row_number() - 1;
    if (activeObject < 0)
        return;
    goAutoPtr<goGL::DrawableObject> o = myPrivate->scene->getObject (activeObject);
    if (!o.isNull())
    {
        printf ("Calling updateInput ()\n");
        // myPrivate->objectInput.setDrawable (*o);
        myPrivate->objectInput.updateInput ();
        printf ("...done.\n");
    }
}

goGUI::SceneControl::~SceneControl ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void goGUI::SceneControl::loadOFF ()
{
    static goString path = "./";
    goString fname;
    if (!goGUI::getFilenameOpen (fname, path, "Scene Control: Open OFF"))
    {
        return;
    }

    fname.getPathName (path);
    myPrivate->view.loadOFF (fname.toCharPtr());

    myPrivate->updateBoxes ();

    myPrivate->view.GLWidgetBegin();
    myPrivate->view.glDraw();
    myPrivate->view.GLWidgetEnd();
}

void goGUI::SceneControl::onHide ()
{
    myPrivate->viewWindow.hide ();
}

void goGUI::SceneControl::onShow ()
{
    myPrivate->viewWindow.show ();
}

void goGUI::SceneControl::objectBoxChangedSlot ()
{
    int active_row = myPrivate->objectBox.get_active_row_number ();
    if (active_row < 0)
    {
        myPrivate->view.setActiveObject (-1);
        return;
    }

    myPrivate->view.setActiveObject (active_row - 1);   //= -1: Object 0 is "None/Camera" and view uses camera if object < 0.

    //= Set object's properties to the objectInput widget
    if (active_row >= 1)
    {
        goAutoPtr<goGL::DrawableObject> o = myPrivate->scene->getObject (active_row - 1);
        myPrivate->objectInput.setDrawableObject (o);
//        if (!o.isNull())
//        {
//            myPrivate->objectInput.set (*o);
//        }
    }
}

void goGUI::SceneControl::lightBoxChangedSlot ()
{
}

void goGUI::SceneControl::saveScene ()
{
    static goString path = "./";
    goString fname;
    if (!goGUI::getFilenameSave (fname, path, "Save Scene"))
        return;
    
    fname.getPathName (path);
    if (!myPrivate->scene->writeASCII (fname.toCharPtr()))
    {
        this->warning ("Saving scene failed.");
    }
}

void goGUI::SceneControl::loadScene ()
{
    static goString path = "./";
    goString fname;
    if (!goGUI::getFilenameOpen (fname, path, "Load Scene"))
        return;
    
    fname.getPathName (path);
    if (!myPrivate->scene->readASCII (fname.toCharPtr()))
    {
        this->warning ("Loading scene failed.");
    }
    myPrivate->updateBoxes ();
    myPrivate->view.queue_draw ();
}

void goGUI::SceneControl::addWaypoint ()
{
    int active_object = myPrivate->objectBox.get_active_row_number () - 1;
    if (active_object < 0)
    {
        return;
    }

    goAutoPtr<goGL::DrawableObject> obj = myPrivate->scene->getObject (active_object);

    goGL::Waypoint wp;
    wp.setTranslation (obj->getTranslation());
    wp.setRotation (obj->getRotation());
    wp.setScale (obj->getScale());
    
    myPrivate->glanimation.addWaypoint (wp);
}

void goGUI::SceneControl::animationPositionChanged ()
{
    int active_object = myPrivate->objectBox.get_active_row_number () - 1;
    if (active_object < 0)
    {
        return;
    }

    if (myPrivate->editWaypointButton.get_active())
        myPrivate->editWaypointButton.set_active (false);

    goAutoPtr<goGL::DrawableObject> obj = myPrivate->scene->getObject (active_object);

    goAutoPtr<goGL::Waypoint> wp = myPrivate->glanimation.getWaypoint();
    
    obj->setTranslation (wp->getTranslation());
    myPrivate->view.GLWidgetBegin ();
    myPrivate->view.glDraw ();
    myPrivate->view.swapBuffers ();
    myPrivate->view.GLWidgetEnd ();
}

void goGUI::SceneControl::editWaypointToggled ()
{
    bool is_editing = myPrivate->editWaypointButton.get_active();
    myPrivate->addWaypointButton.set_sensitive (!is_editing);
    myPrivate->glanimation.set_sensitive (!is_editing);
    if (is_editing)
    {
        this->transformToSelectedWaypoint ();
        myPrivate->view.queue_draw ();
//        myPrivate->editWaypointConnection1 = myPrivate->glanimation.signalWaypointSelected().connect (sigc::mem_fun (*this, &SceneControl::transformToSelectedWaypoint));
        myPrivate->editWaypointConnection2 = myPrivate->view.signalChanged().connect (sigc::mem_fun (*this, &SceneControl::editWaypoint));
    }
    else
    {
//        myPrivate->editWaypointConnection1.disconnect ();
        myPrivate->editWaypointConnection2.disconnect ();
    }
}

void goGUI::SceneControl::editWaypoint ()
{
    int active_object = myPrivate->objectBox.get_active_row_number () - 1;
    if (active_object < 0)
    {
        return;
    }

    goAutoPtr<goGL::DrawableObject> obj = myPrivate->scene->getObject (active_object);
    goGL::Waypoint wp;
    wp.setTranslation (obj->getTranslation());
    wp.setRotation (obj->getRotation());
    wp.setScale (obj->getScale());
    
    int i = myPrivate->glanimation.selectedWaypoint ();
    myPrivate->glanimation.getAnimation()->setWaypoint (i, wp);
}

void goGUI::SceneControl::transformToSelectedWaypoint ()
{
    int active_object = myPrivate->objectBox.get_active_row_number () - 1;
    if (active_object < 0)
    {
        return;
    }

    goAutoPtr<goGL::DrawableObject> obj = myPrivate->scene->getObject (active_object);

    goAutoPtr<goGL::Waypoint> wp = myPrivate->glanimation.getSelectedWaypoint();
    if (wp.isNull())
        return;

    obj->setTranslation (wp->getTranslation());
    myPrivate->view.queue_draw ();
    // obj->setRotation (wp->getRotation());
    // obj->setScale (wp->getScale());
}
