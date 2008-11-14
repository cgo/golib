#include <gogui/scenecontrol.h>
#include <gogui/helper.h>
#include <gogui/gldrawableobjectinput.h>
#include <gogui/glanimation.h>
#include <gogui/vectorinput.h>
#include <gtkmm.h>

#include <gogl/scene.h>
#include <gogl/helper.h>
#include <gogl/textureimage.h>
#include <gofileio.h>

#include <limits>

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
                  deleteObjectButton ("Delete Object"),
                  loadImageButton ("Load Image As Plane"),
                  saveSceneButton ("Save Scene"),
                  loadSceneButton ("Load Scene"),
                  saveImageButton ("Save image"),
                  clearColourInput ("Clear colour", 4),
                  ambientInput ("Global light", 4),
                  waypointButtonsBox (),
                  addWaypointButton ("Add Waypoint"),
                  removeWaypointButton ("Remove WP"),
                  prependWaypointButton ("Prepend WP"),
                  appendWaypointButton ("Append WP"),
                  editWaypointButton ("Edit Selected Waypoint"),
                  createMovieButton ("Create Movie"),
                  constantSpeedButton ("Speed from translation"),
                  movieStepsButton (),
                  tooltips (),
                  editWaypointConnection1 (),
                  editWaypointConnection2 (),
                  objectInput (),
                  objectInputWindow (),
                  glanimation ()
            {
                scene.set (new goGL::Scene);

                movieStepsButton.set_digits (0);
                movieStepsButton.set_range (0, std::numeric_limits<int>::max ());
                movieStepsButton.set_increments (1, 10);
                movieStepsButton.set_value (25);

                tooltips.enable ();
                tooltips.set_tip (objectBox, "Select the currently active object here.");
                tooltips.set_tip (lightBox, "Select the currently active light here.\nThis currently has no function.");
                tooltips.set_tip (objectPropButton, "Translation, Rotation, Scale, Material of active object.");
                tooltips.set_tip (deleteObjectButton, "Delete the currently active object.");
                tooltips.set_tip (loadOFFButton, "Load an object in OFF file format.");
                tooltips.set_tip (loadImageButton, "Load an image and create a plane object.");
                tooltips.set_tip (saveSceneButton, "Save scene (OFF file names, transformations, materials).\nNo lights and images yet.");
                tooltips.set_tip (loadSceneButton, "Load a saved scene.");
                tooltips.set_tip (saveImageButton, "Save OpenGL image.");
                tooltips.set_tip (addWaypointButton, "Add a new waypoint to the animation.");
                tooltips.set_tip (removeWaypointButton, "Remove the current waypoint from the animation.");
                tooltips.set_tip (prependWaypointButton, "Prepend new waypoint before current waypoint.");
                tooltips.set_tip (appendWaypointButton, "Append new waypoint after current waypoint.");
                tooltips.set_tip (editWaypointButton, "Edit the current waypoint.");
                tooltips.set_tip (createMovieButton, "Create image sequence from current animation.");
                tooltips.set_tip (constantSpeedButton, "If set, time steps for waypoints are calculated from\nthe translation.\nIf not set, time is set incrementally.\nUnset if translation does not change\n between two consecutive frames!");
                tooltips.set_tip (movieStepsButton, "Number of images to save for the movie.");
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
            Gtk::Button deleteObjectButton;
            Gtk::Button loadImageButton;
            Gtk::Button saveSceneButton;
            Gtk::Button loadSceneButton;
            Gtk::Button saveImageButton;

            goGUI::VectorInput clearColourInput;
            goGUI::VectorInput ambientInput;

            //= For animation
            Gtk::VBox   waypointButtonsBox;
            Gtk::Button addWaypointButton;
            Gtk::Button removeWaypointButton;
            Gtk::Button prependWaypointButton;
            Gtk::Button appendWaypointButton;
            Gtk::CheckButton editWaypointButton;

            Gtk::Button      createMovieButton;
            Gtk::CheckButton constantSpeedButton;
            Gtk::SpinButton  movieStepsButton;

            Gtk::Tooltips tooltips;

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

    myPrivate->constantSpeedButton.set_active (true);

    if (!myPrivate->scene.isNull())
    {
        myPrivate->ambientInput.setVector (myPrivate->scene->getAmbient());
        myPrivate->clearColourInput.setVector (myPrivate->scene->getClearColour());
    }

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
        hbox->pack_start (myPrivate->deleteObjectButton, Gtk::PACK_SHRINK);
        vbox->pack_start (*hbox);

        hbox = Gtk::manage (new Gtk::HBox);
        hbox->pack_start (myPrivate->loadOFFButton, Gtk::PACK_SHRINK);
        hbox->pack_start (myPrivate->loadImageButton, Gtk::PACK_SHRINK);
        vbox->pack_start (*hbox);

        hbox = Gtk::manage (new Gtk::HBox);
        hbox->pack_start (myPrivate->saveSceneButton, Gtk::PACK_SHRINK);
        hbox->pack_start (myPrivate->loadSceneButton, Gtk::PACK_SHRINK);
        vbox->pack_start (*hbox);

        hbox = Gtk::manage (new Gtk::HBox);
        hbox->pack_start (myPrivate->saveImageButton, Gtk::PACK_SHRINK);
        vbox->pack_start (*hbox);

        hbox = Gtk::manage (new Gtk::HBox);
        hbox->pack_start (myPrivate->clearColourInput, Gtk::PACK_SHRINK);
        vbox->pack_start (*hbox);
        hbox = Gtk::manage (new Gtk::HBox);
        hbox->pack_start (myPrivate->ambientInput, Gtk::PACK_SHRINK);
        vbox->pack_start (*hbox);

        hbox = Gtk::manage (new Gtk::HBox);
        hbox->pack_start (myPrivate->addWaypointButton, Gtk::PACK_SHRINK);
        hbox->pack_start (myPrivate->removeWaypointButton, Gtk::PACK_SHRINK);
        myPrivate->waypointButtonsBox.pack_start (*hbox, Gtk::PACK_SHRINK);
        // vbox->pack_start (*hbox);
        hbox = Gtk::manage (new Gtk::HBox);
        hbox->pack_start (myPrivate->prependWaypointButton, Gtk::PACK_SHRINK);
        hbox->pack_start (myPrivate->appendWaypointButton, Gtk::PACK_SHRINK);
        myPrivate->waypointButtonsBox.pack_start (*hbox, Gtk::PACK_SHRINK);

        hbox = Gtk::manage (new Gtk::HBox);
        hbox->pack_start (myPrivate->createMovieButton, Gtk::PACK_SHRINK);
        hbox->pack_start (myPrivate->movieStepsButton, Gtk::PACK_SHRINK);
        hbox->pack_start (myPrivate->constantSpeedButton, Gtk::PACK_SHRINK);
        myPrivate->waypointButtonsBox.pack_start (*hbox, Gtk::PACK_SHRINK);

        vbox->pack_start (myPrivate->waypointButtonsBox, Gtk::PACK_SHRINK);

        hbox = Gtk::manage (new Gtk::HBox);
        hbox->pack_start (myPrivate->editWaypointButton, Gtk::PACK_SHRINK);
        vbox->pack_start (*hbox, Gtk::PACK_SHRINK);


        myHBox->pack_start (*vbox, Gtk::PACK_SHRINK);
    }
    {
        myPrivate->lightBox.append_text ("GL_LIGHT0");
        myPrivate->lightBox.append_text ("GL_LIGHT1");
        myPrivate->lightBox.append_text ("GL_LIGHT2");
    }

    {
        //= THIS CRASHES ONLY AT UNI -- SOMETHING WRONG WITH GTKMM/SIGC ????
        // myPrivate->glanimation.signalWaypointSelected().connect (sigc::mem_fun (*this, &SceneControl::transformToSelectedWaypoint));
        //= Workaround for the crash mentioned above.
        myPrivate->glanimation.waypointSelectedCaller().connect (goMemberFunction<SceneControl,int> (this, &SceneControl::transformToSelectedWaypoint));
        // myPrivate->view.signalChanged().connect (sigc::mem_fun (*this, &SceneControl::editWaypoint));
    }

    {
        myPrivate->objectBox.signal_changed().connect (sigc::mem_fun (*this, &SceneControl::objectBoxChangedSlot));
        myPrivate->lightBox.signal_changed().connect (sigc::mem_fun (*this, &SceneControl::lightBoxChangedSlot));
    }
    {
        myPrivate->objectPropButton.signal_clicked().connect (sigc::mem_fun (*this, &SceneControl::objectProperties));
        // myPrivate->objectInput.signalDrawableObjectInputChanged().connect (sigc::mem_fun (*this, &SceneControl::objectPropChanged));
        myPrivate->objectInput.callerDrawableObjectInputChanged().connect (goMemberFunction<SceneControl,int> (this, &SceneControl::objectPropChanged));
        myPrivate->loadOFFButton.signal_clicked().connect (sigc::mem_fun (*this, &SceneControl::loadOFF));
        myPrivate->deleteObjectButton.signal_clicked().connect (sigc::mem_fun (*this, &SceneControl::deleteObject));
        myPrivate->loadImageButton.signal_clicked().connect (sigc::mem_fun (*this, &SceneControl::loadImage));

        myPrivate->saveSceneButton.signal_clicked().connect (sigc::mem_fun (*this, &SceneControl::saveScene));
        myPrivate->loadSceneButton.signal_clicked().connect (sigc::mem_fun (*this, &SceneControl::loadScene));
        myPrivate->saveImageButton.signal_clicked().connect (sigc::mem_fun (*this, &SceneControl::saveImage));

        myPrivate->clearColourInput.signalChanged().connect (sigc::mem_fun (*this, &SceneControl::clearColourChanged));
        myPrivate->ambientInput.signalChanged().connect (sigc::mem_fun (*this, &SceneControl::ambientChanged));

        myPrivate->addWaypointButton.signal_clicked().connect (sigc::mem_fun (*this, &SceneControl::addWaypoint));
        myPrivate->removeWaypointButton.signal_clicked().connect (sigc::mem_fun (*this, &SceneControl::removeWaypoint));
        myPrivate->prependWaypointButton.signal_clicked().connect (sigc::mem_fun (*this, &SceneControl::prependWaypoint));
        myPrivate->appendWaypointButton.signal_clicked().connect (sigc::mem_fun (*this, &SceneControl::appendWaypoint));

        myPrivate->createMovieButton.signal_clicked().connect (sigc::mem_fun (*this, &SceneControl::createMovie));
        myPrivate->constantSpeedButton.signal_toggled().connect (sigc::mem_fun (*this, &SceneControl::constantSpeedToggled));

        myPrivate->editWaypointButton.signal_toggled().connect (sigc::mem_fun (*this, &SceneControl::editWaypointToggled));

        //= BUG IN UNIVERSITY COMPUTER'S SIGC/GTKMM???
        // myPrivate->glanimation.signalPositionChanged().connect (sigc::mem_fun (*this, &SceneControl::animationPositionChanged));
        myPrivate->glanimation.positionChangedCaller().connect (goMemberFunction<SceneControl,int> (this, &SceneControl::animationPositionChanged));
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

int goGUI::SceneControl::objectPropChanged ()
{
    myPrivate->view.queue_draw();
    return 0;
}

/** 
 * @brief Get the \c goGUI::SceneView widget used by this
 * control.
 * 
 * @return The \c SceneView widget.
 */
goGUI::SceneView& goGUI::SceneControl::getSceneView ()
{
    return myPrivate->view;
}

/** 
 * @brief Slot called whenever the view changes (by user input).
 */
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

/** 
 * @brief Load an object in Object File Format (OFF).
 */
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

/** 
 * @brief Deletes the currently active object.
 */
void goGUI::SceneControl::deleteObject ()
{
    //= set current object's properties
    int activeObject = myPrivate->objectBox.get_active_row_number() - 1;
    if (activeObject < 0)
        return;

    myPrivate->scene->removeObject (activeObject);
    myPrivate->objectInput.updateInput ();
    myPrivate->updateBoxes ();
    myPrivate->view.GLWidgetBegin ();
    myPrivate->view.glDraw ();
    myPrivate->view.swapBuffers ();
    myPrivate->view.GLWidgetEnd ();
}

/** 
 * @brief Loads an image as a plane in OpenGL (goGL::TextureImage).
 */
void goGUI::SceneControl::loadImage ()
{
    static goString path = "./";
    goString fname;
    if (!goGUI::getFilenameOpen (fname, path, "Scene Control: Open Image As Plane"))
    {
        return;
    }
    fname.getPathName (path);

    goSignal3D<void> image;
    try
    {
        goFileIO::readImage (fname.toCharPtr(), &image, false);
    }
    catch (goFileIOException ex)
    {
        this->warning ("Could not load image.");
        return;
    }

    myPrivate->view.GLWidgetBegin ();

    goGL::TextureImage* img = new goGL::TextureImage;
    img->setImage (image);
    goAutoPtr<goGL::DrawableObject> obj (img);
    myPrivate->scene->add (obj);
    myPrivate->view.GLWidgetEnd ();
    myPrivate->updateBoxes ();
    // myPrivate->view.queue_draw ();
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

void goGUI::SceneControl::saveImage ()
{
    static goString path = "./";
    goString fname;
    if (!goGUI::getFilenameSave (fname, path, "Save Image"))
        return;

    fname.getPathName (path);


    goSignal3D<void> image2;
    goSignal3D<void> image;
    image.setDataType (GO_UINT8);
    image2.setDataType (GO_UINT8);
    image2.make (32,32,1,32,32,1,4,4,0,3);
    myPrivate->view.GLWidgetBegin ();
    myPrivate->view.glDraw ();
    myPrivate->view.swapBuffers ();
    goGL::getGLBuffer (image2);
    myPrivate->view.GLWidgetEnd ();
    //= Flip Y
    if (image2.getSize() != image.getSize())
    {
        image.make (image2.getSize(), image2.getBlockSize(), image2.getBorderSize(), image2.getChannelCount());
    }
    goSignalFlipY (image2, image);

    try
    {
        goFileIO::writeImage (fname.toCharPtr(), &image);
    }
    catch (goFileIOException ex)
    {
        if (ex.code == goFileIOException::EXISTS)
        {
            goFileIO::remove (fname.toCharPtr());
        }
        try
        {
            goFileIO::writeImage (fname.toCharPtr(), &image);
        }
        catch (goFileIOException ex2)
        {
            this->warning ("Writing image failed.");
            return;
        }
    }
}

void goGUI::SceneControl::clearColourChanged ()
{
    if (!myPrivate->scene.isNull ())
    {
        goVectorf temp;
        myPrivate->clearColourInput.getVector (temp);
        myPrivate->scene->setClearColour (temp);
        myPrivate->view.queue_draw ();
    }
}

void goGUI::SceneControl::ambientChanged ()
{
    if (!myPrivate->scene.isNull ())
    {
        goVectorf temp;
        myPrivate->ambientInput.getVector (temp);
        myPrivate->scene->setAmbient (temp);
        myPrivate->view.queue_draw ();
    }
}

/** 
 * @brief Adds a waypoint to the current animation.
 */
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

void goGUI::SceneControl::removeWaypoint ()
{
    myPrivate->glanimation.removeWaypoint (myPrivate->glanimation.selectedWaypoint());
}

void goGUI::SceneControl::prependWaypoint ()
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
    
    myPrivate->glanimation.prependWaypoint (wp, myPrivate->glanimation.selectedWaypoint());
}

void goGUI::SceneControl::appendWaypoint ()
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
    
    myPrivate->glanimation.appendWaypoint (wp, myPrivate->glanimation.selectedWaypoint());
}

int goGUI::SceneControl::animationPositionChanged ()
{
    int active_object = myPrivate->objectBox.get_active_row_number () - 1;
    if (active_object < 0)
    {
        return 0;
    }

    if (myPrivate->editWaypointButton.get_active())
        myPrivate->editWaypointButton.set_active (false);

    goAutoPtr<goGL::DrawableObject> obj = myPrivate->scene->getObject (active_object);

    goAutoPtr<goGL::Waypoint> wp = myPrivate->glanimation.getWaypoint();
    
    obj->setTranslation (wp->getTranslation());
    obj->setRotation (wp->getRotation());
    myPrivate->view.GLWidgetBegin ();
    myPrivate->view.glDraw ();
    myPrivate->view.swapBuffers ();
    myPrivate->view.GLWidgetEnd ();

    return 0;
}

void goGUI::SceneControl::createMovie ()
{
    int active_object = myPrivate->objectBox.get_active_row_number () - 1;
    if (active_object < 0)
    {
        this->warning ("Activate the appropriate object first.");
        return;
    }

    goAutoPtr<goGL::DrawableObject> obj = myPrivate->scene->getObject (active_object);

    static goString path = "./";
    goString fname;
    if (!goGUI::getFilenameSave (fname, path, "Choose Movie Images Base Name"))
    {
        return;
    }

    fname.getPathName (path);
   
    goSize_t steps = myPrivate->movieStepsButton.get_value_as_int();

    goDouble dt = 1.0 / (float)(steps - 1);
    goDouble t = 0.0;
    goGL::Waypoint wp;
    goAutoPtr<goGL::Animation> A = myPrivate->glanimation.getAnimation();
    if (A.isNull())
    {
        this->warning ("Animation object is null.");
        return;
    }

    goString cfname;
    char n[16];
    goSignal3D<void> image2;
    goSignal3D<void> image;
    image.setDataType (GO_UINT8);
    image2.setDataType (GO_UINT8);
    image2.make (32,32,1,32,32,1,4,4,0,3);
    for (goSize_t i = 0; i < steps; ++i)
    {
        A->interpolate (t, wp);
        obj->setTranslation (wp.getTranslation());
        obj->setRotation (wp.getRotation());

        cfname = fname;
        cfname += "_";
        sprintf (n, "%.5d", i);
        cfname += n;
        cfname += ".jpg";
        myPrivate->view.GLWidgetBegin ();
        myPrivate->view.glDraw ();
        myPrivate->view.swapBuffers ();
        goGL::getGLBuffer (image2);
        myPrivate->view.GLWidgetEnd ();
        //= Flip Y
        if (image2.getSize() != image.getSize())
        {
            image.make (image2.getSize(), image2.getBlockSize(), image2.getBorderSize(), image2.getChannelCount());
        }
        goSignalFlipY (image2, image);

        try
        {
            goFileIO::writeImage (cfname.toCharPtr(), &image);
        }
        catch (goFileIOException ex)
        {
            if (ex.code == goFileIOException::EXISTS)
            {
                goFileIO::remove (cfname.toCharPtr());
            }
            try
            {
                goFileIO::writeImage (cfname.toCharPtr(), &image);
            }
            catch (goFileIOException ex2)
            {
                this->warning ("Writing image failed!");
                return;
            }
        }

        t = goMath::min<goDouble> (1.0, t + dt);
    }
}

void goGUI::SceneControl::constantSpeedToggled ()
{
    if (!myPrivate->glanimation.getAnimation().isNull ())
    {
        myPrivate->glanimation.getAnimation()->setConstantSpeedFromPosition (myPrivate->constantSpeedButton.get_active());
    }
}

void goGUI::SceneControl::editWaypointToggled ()
{
    bool is_editing = myPrivate->editWaypointButton.get_active();
    //myPrivate->addWaypointButton.set_sensitive (!is_editing);
    myPrivate->waypointButtonsBox.set_sensitive (!is_editing);
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

int goGUI::SceneControl::transformToSelectedWaypoint ()
{
    int active_object = myPrivate->objectBox.get_active_row_number () - 1;
    if (active_object < 0)
    {
        return 0;
    }

    goAutoPtr<goGL::DrawableObject> obj = myPrivate->scene->getObject (active_object);

    goAutoPtr<goGL::Waypoint> wp = myPrivate->glanimation.getSelectedWaypoint();
    if (wp.isNull())
        return 0;

    obj->setTranslation (wp->getTranslation());
    myPrivate->view.queue_draw ();
    obj->setRotation (wp->getRotation());
    // obj->setScale (wp->getScale());

    return 0;
}
