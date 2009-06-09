#include <gogui/imageview.h>
#include <gogui/mainwindow.h>
#include <gogui/helper.h>
#include <gogui/cannycontrol.h>
#include <gogui/controldialog.h>

#include <gofileio.h>
#include <gosignal3d.h>
#include <gosignal.h>
#include <gosignalhelper.h>
#include <gosignalmacros.h>

#include <gtkmm.h>

class ImageViewer : public goGUI::MainWindow
{
    public:
        ImageViewer ();
        virtual ~ImageViewer ();

        void loadImage ();
        void loadImageName (const char* fname);
        void loadCurve ();
        void sobel ();
        void canny ();

        goCaller1<int, goAutoPtr<goSignal3DBase<void> > >& imageCaller () { return myImageCaller; }
        goCaller1<int, goAutoPtr<goSignal3DBase<void> > >& edgeMapCaller () { return myEdgeMapCaller; }

    private:
        goGUI::ImageView view;
        goGUI::CannyControl cannyControl;

        goCaller1<int, goAutoPtr<goSignal3DBase<void> > > myImageCaller;   //= called when the image is set
        goCaller1<int, goAutoPtr<goSignal3DBase<void> > > myEdgeMapCaller; //= called when the target (edge map) is set
};

ImageViewer::ImageViewer ()
    : goGUI::MainWindow (),
      view (),
      cannyControl (),
      myImageCaller (),
      myEdgeMapCaller ()
{
    //Gtk::ScrolledWindow *sw = Gtk::manage (new Gtk::ScrolledWindow);
    //sw->add (this->view);
    //this->view.show ();
    //this->getPaned().add1 (*sw);
    this->getPaned().add1 (this->view);
    this->addControl (this->cannyControl);

    Gtk::MenuItem* loadItem = this->addMenuItem (this->getFileMenu (), "Load image");
    loadItem->signal_activate().connect (sigc::mem_fun (this, &ImageViewer::loadImage));

    Gtk::MenuItem* loadItem2 = this->addMenuItem (this->getFileMenu (), "Load curve");
    loadItem2->signal_activate().connect (sigc::mem_fun (this, &ImageViewer::loadCurve));
   
    {
        Gtk::MenuItem* i = this->addMenuItem (this->getFileMenu (), "Sobel");
        i->signal_activate().connect (sigc::mem_fun (this, &ImageViewer::sobel));
    }
    {
        Gtk::MenuItem* i = this->addMenuItem (this->getFileMenu (), "Canny");
        i->signal_activate().connect (sigc::mem_fun (this, &ImageViewer::canny));
    }

    this->addFileQuit ();

    this->show_all_children ();

    //this->getPaned().get_child1()->hide ();
}

ImageViewer::~ImageViewer ()
{
}

void ImageViewer::loadCurve ()
{
    goString fname = "";
    static goString start = "./";
    if (goGUI::getFilenameOpen (fname, start, "Load Curve from Matrix File"))
    {
        fname.getPathName (start);
        goMatrixd curve;
        
        if (!curve.readASCII (fname.toCharPtr ()))
        {
            goGUI::warning ("Could not load curve matrix.");
            return;
        }

        
        goVectord x (0), y (0);
        curve.refColumn (0, x);
        curve.refColumn (1, y);

        goVectord mean;
        goMath::centerOfMass (curve, mean);
        goMath::translate (curve, mean * -1.0);

        goFloat x0 = goMath::min (x);
        goFloat x1 = goMath::max (x);
        goFloat y0 = goMath::min (y);
        goFloat y1 = goMath::max (y);

        curve.print ();

        goAutoPtr<goPlot::Object2DPoints> o = this->view.graph()->addCurve (curve);
        goPlot::Trafo2D t = o->transform ();
        t *= goPlot::Trafo2D (1.0 / (x1-x0), 0.0, 0.0, 1.0 / (y1-y0), -x0 / (x1-x0), -y0 / (y1-y0));
        o->setTransform (t);
        this->view.graph()->axis(0)->setLower (0.0);
        this->view.graph()->axis(0)->setUpper (1.0);
        this->view.graph()->axis(1)->setLower (0.0);
        this->view.graph()->axis(1)->setUpper (1.0);

        o->lineTraits().setWidth (3.0);
        o->lineTraits().setColour (goPlot::RGBA (1.0, 0.0, 0.0, 0.5));

//        this->view.graph()->axis(0)->setVisible (true);
//        this->view.graph()->axis(0)->setTics (10);
//        this->view.graph()->axis(1)->setVisible (true);
//        this->view.graph()->axis(1)->setTics (10);
//
        this->view.queue_draw ();
    }
}

void ImageViewer::loadImage ()
{
    goString fname = "";
    static goString start = "./";
    if (goGUI::getFilenameOpen (fname, start, "Load Image"))
    {
        fname.getPathName (start);
        goSignal3D<void> image;
        try
        {
            goFileIO::readImage (fname.toCharPtr (), &image, true);
//            goSignal3D<void> image2;
//            image2.setDataType (image.getDataType ().getID ());
//            image2.make (image.getSize(), image.getSize(), image.getBorderSize(), 3);
//            switch (image.getChannelCount())
//            {
//                case 1: 
//                    {
//                        const int s_i[] = {0, 0, 0};
//                        const int t_i[] = 
//                    }
//                goCopySignal (&image, &image2);
//            }
//            this->view.setImage (image2);
            
            //= Let imageview worry about the image format.            
            this->view.setImage (image);
            this->view.queue_draw ();
            // this->control.setImage (image);
        }
        catch (goFileIOException& ex)
        {
            Gtk::MessageDialog dlg ("Reading image failed.");
            dlg.run ();
            return;
        }
    }
}

void ImageViewer::loadImageName (const char* fname)
{
    goSignal3D<void> image;
    try
    {
        goFileIO::readImage (fname, &image, true);
        goSignal3D<void> image2;
        image2.setDataType (image.getDataType ().getID ());
        image2.make (image.getSize(), image.getSize(), image.getBorderSize(), 3);
        goCopySignal (&image, &image2);
        this->view.setImage (image2);
        this->view.queue_draw ();
    }
    catch (goFileIOException& ex)
    {
        Gtk::MessageDialog dlg ("Reading image failed.");
        dlg.run ();
        return;
    }
}

void ImageViewer::sobel ()
{
    goAutoPtr<goSignal3DBase<void> > image = this->view.getImage ();
    
    if (image.isNull ())
        return;
    
    goSignal3D<void> sob, abs_sob;
    sob.setDataType (GO_FLOAT);

    if (image->getChannelCount() == 1)
    {
        goSignal::sobel2D (*image, sob);
    }
    else
    {
        goSignal3D<void> temp;
        temp.setDataType (GO_UINT8);
        temp.make (image->getSize(), image->getBlockSize(), image->getBorderSize(), 1);
        goRGBAtoScalar (image, &temp);
        goSignal::sobel2D (temp, sob);
    }
    abs_sob.setDataType (sob.getDataType ().getID ());
    abs_sob.make (sob.getSize(), sob.getSize(), sob.getBorderSize(), 1);

    GO_SIGNAL3D_EACHELEMENT_2_GENERIC (goFloat xx = *(goFloat*)__ptr; 
            goFloat yy = *(((goFloat*)__ptr) + 1); *(goFloat*)__ptr_target = sqrt (xx * xx + yy * yy);,
            sob, abs_sob);

    goNormalizeSignal (&abs_sob);
    abs_sob *= 255.0f;

    goSignal3D<void> image2;
    image2.make (image);
    image2.setChannel (0);
    goCopySignalChannel (&abs_sob, &image2);
    image2.setChannel (1);
    goCopySignalChannel (&abs_sob, &image2);
    image2.setChannel (2);
    goCopySignalChannel (&abs_sob, &image2);
    image2.setChannel (0);
    this->view.setImage (image2);
    this->view.queue_draw ();
}

void ImageViewer::canny ()
{
    goAutoPtr<goSignal3DBase<void> > image = this->view.getImage ();
    
    if (image.isNull ())
        return;

    // this->queue_draw ();
    // return;

    int result = Gtk::RESPONSE_CANCEL;
    goAutoPtr<goSignal3D<void> > cimg = new goSignal3D<void>;
    cimg->setDataType (GO_UINT8);
    if (image->getChannelCount() == 1)
    {
        image->setBorderFlags (GO_X|GO_Y, GO_CONSTANT_BORDER);
        image->applyBorderFlags (GO_X|GO_Y);
        // goSignal::smooth (*image);
        goGUI::CannyControl ctrl;
        goGUI::ControlDialog dlg (ctrl);
        ctrl.setImage (image);
        ctrl.setEdgeMap (cimg);
        result = dlg.run ();
        // goSignal::canny (*image, cimg);
    }
    else
    {
        goAutoPtr<goSignal3D<void> > temp = new goSignal3D<void>;
        temp->setDataType (GO_UINT8);
        temp->setBorderFlags (GO_X|GO_Y, GO_CONSTANT_BORDER);
        // temp.make (image->getSize(), image->getBlockSize(), image->getBorderSize(), 1);
        temp->make (image->getSize(), image->getBlockSize(), goSize3D (3, 3, 0), 1);
        goRGBAtoScalar (image, temp);
        goSignal::smooth (*temp);
        //cimg.make (&temp);
        //goCopySignal (&temp, &cimg);
        goGUI::CannyControl ctrl;
        goGUI::ControlDialog dlg (ctrl);
        ctrl.setImage (temp);
        ctrl.setEdgeMap (cimg);
        result = dlg.run ();
        // goSignal::canny (temp, cimg, 80.0, 40.0);
    }
    // cimg *= 255.0f;

    if (result != Gtk::RESPONSE_CANCEL)
    {
        goSignal3D<void> image2;
        image2.make (image);
        image2.setChannel (0);
        goCopySignalChannel (cimg, &image2);
        image2.setChannel (1);
        goCopySignalChannel (cimg, &image2);
        image2.setChannel (2);
        goCopySignalChannel (cimg, &image2);
        image2.setChannel (0);
        this->view.setImage (image2);
        this->view.queue_draw ();
    }
}


int main (int argc, char* argv[])
{
    Gtk::Main kit (argc, argv);
    ImageViewer iv;
    Gtk::Main::run (iv);
    exit (0);
}
