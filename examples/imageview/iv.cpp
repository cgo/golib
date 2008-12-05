#include <gogui/imageview.h>
#include <gogui/mainwindow.h>
#include <gogui/helper.h>

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
        void sobel ();
        void canny ();

    private:
        goGUI::ImageView view;
};

ImageViewer::ImageViewer ()
    : goGUI::MainWindow (),
      view ()
{
    Gtk::ScrolledWindow *sw = Gtk::manage (new Gtk::ScrolledWindow);
    sw->add (this->view);
    this->view.show ();
    this->getPaned().add1 (*sw);

    Gtk::MenuItem* loadItem = this->addMenuItem (this->getFileMenu (), "Load image");
    loadItem->signal_activate().connect (sigc::mem_fun (this, &ImageViewer::loadImage));
   
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
    goAutoPtr<goSignal3D<void> > image = this->view.getImage ();
    
    if (image.isNull ())
        return;
    
    goSignal3D<void> sob, abs_sob;
    sob.setDataType (GO_FLOAT);

    goSignal::sobel2D (*image, sob);
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
    goAutoPtr<goSignal3D<void> > image = this->view.getImage ();
    
    if (image.isNull ())
        return;
  
    goSignal3D<void> cimg;
    cimg.setDataType (GO_UINT8);
    goSignal::canny (*image, cimg);
    cimg *= 255.0f;

    goSignal3D<void> image2;
    image2.make (image);
    image2.setChannel (0);
    goCopySignalChannel (&cimg, &image2);
    image2.setChannel (1);
    goCopySignalChannel (&cimg, &image2);
    image2.setChannel (2);
    goCopySignalChannel (&cimg, &image2);
    image2.setChannel (0);
    this->view.setImage (image2);
    this->view.queue_draw ();
}

    template <class T>
    static int _cannyRoundAngle (T angle)
    {
        //= Erzwinge Winkel zw. 90 u. -90 Grad
        if (angle < T(-90))
        {
            angle += T(90);
        }
        else if (angle > T(90))
        {
            angle -= T(90);
        }

        if (angle < T(67.5) || angle < T(-67.5))
            return 90;

        if (angle > T(67.5) && angle < T(22.5))
            return 45;

        if (angle < T(22.5) && angle > T(-22.5))
            return 0;

        return -45;

        //if (angle < T(-22.5) && angle > T(-67.5))
        //    return -45;
    }

int main (int argc, char* argv[])
{
    Gtk::Main kit (argc, argv);
    ImageViewer iv;
    Gtk::Main::run (iv);
    exit (0);
}
