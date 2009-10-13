#include "gaussimagecontrol.h"
#include <gogui/imageview.h>
#include <gogui/vectorinput.h>
#include <gogaussimage.h>
#include <gosignal3dgenericiterator.h>
#include <gtkmm.h>

class GaussImageControlPrivate
{
    public:
        GaussImageControlPrivate ()
            : meanImageView (),
              varianceImageView (),
              meanWindow (),
              varianceWindow (),
              gaussImage (),
              framesCountInput ("Frames Count for Gaussian", 1)
        {
            meanWindow.add (meanImageView);
            varianceWindow.add (varianceImageView);
            meanWindow.show_all_children ();
            varianceWindow.show_all_children ();
            meanWindow.show ();
            varianceWindow.show ();
        }

        goGUI::ImageView meanImageView;
        goGUI::ImageView varianceImageView;
        Gtk::Window      meanWindow;
        Gtk::Window      varianceWindow;

        goGaussImage     gaussImage;  //= Computes parameters for one Gaussian per pixel 

        goGUI::VectorInput framesCountInput;  //= Number of frames over which to calculate mu and sigma
};

GaussImageControl::GaussImageControl ()
    : goGUI::Control ("Gauss Image"),
      myPrivate (0)
{
    myPrivate = new GaussImageControlPrivate;

    {
        Gtk::VBox* vbox = Gtk::manage (new Gtk::VBox);
        vbox->pack_start (myPrivate->framesCountInput, Gtk::PACK_SHRINK);
        this->add (*vbox);
    }

    goVectorf v (1);
    v[0] = 20;
    myPrivate->framesCountInput.setVector (v);
    myPrivate->framesCountInput.setDigits (0);
    myPrivate->framesCountInput.setRange (-1.0, 10000.0, 1.0, 10.0);
    myPrivate->framesCountInput.set_tooltip_text ("Number of frames used\nto calculate Gaussian");    
}

GaussImageControl::~GaussImageControl ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void GaussImageControl::update (goAutoPtr<goSignal3DBase<void> >  image)
{
    goGUI::ImageView& view1 = myPrivate->meanImageView;
    goGUI::ImageView& view2 = myPrivate->varianceImageView;

    goSize3D sz = image->getSize ();

    if (view1.getImage (0).isNull () || view2.getImage (0).isNull () ||
            view1.getImage (0)->getSize () != sz || view2.getImage (0)->getSize() != sz)
    {
        view1.setImage (sz.x, sz.y, goPlot::Object2DImage::RGB24, 0);
        view2.setImage (sz.x, sz.y, goPlot::Object2DImage::RGB24, 0);
    }

    static goSignal3D<void> mono_image;
    if (image->getChannelCount () != 1 && (mono_image.getSize() != image->getSize()))
    {
        mono_image.setDataType (image->getDataType().getID());
        mono_image.make (image->getSize(), image->getSize(), goSize3D (8, 8, 0), 1);
    }
    if (image->getChannelCount () != 1)
    {
        goRGBAtoScalar (image, &mono_image);
        image = &mono_image;
        image.getRRefPtr()->incRef (); //= Prevent the autoptr from deleting its contents.
    }

    goVectorf v (1);
    myPrivate->framesCountInput.getVector (v);

    myPrivate->gaussImage.update (*image, v[0]);
    const int source_chan[] = {0, 0, 0};
    const int target_chan[] = {0, 1, 2};
    // goSignal::convert (myPrivate->gaussImage.getMean(), *view1.getImage(0), source_chan, target_chan, 3);
    goSignal::convert (myPrivate->gaussImage.getVariance(), *view2.getImage(0), source_chan, target_chan, 3);

    // myPrivate->meanWindow.queue_draw ();
    myPrivate->varianceWindow.queue_draw ();
}

void GaussImageControl::difference (goAutoPtr<goSignal3DBase<void> >  image)
{
    goGUI::ImageView& view1 = myPrivate->meanImageView;

    goSize3D sz = image->getSize ();

    if (view1.getImage (0).isNull () || 
            view1.getImage (0)->getSize () != sz)
    {
        view1.setImage (sz.x, sz.y, goPlot::Object2DImage::RGB24, 0);
    }

    goSignal3DGenericConstIterator mean_it (&myPrivate->gaussImage.getMean()), var_it (&myPrivate->gaussImage.getVariance());
    goSignal3DGenericIterator image_it (image), target_it (view1.getImage (0));

    //= Assuming target is uchar 3 channel and image is uchar 1 channel.
    while (!mean_it.endY())
    {
        mean_it.resetX(); var_it.resetX(); image_it.resetX(); target_it.resetX();
        while (!mean_it.endX())
        {
            goFloat mu = *(goFloat*)*mean_it;
            goFloat x = (float)*(goUInt8*)*image_it;
            goFloat d = 0.5 * (mu - x) * (mu - x) / sqrt(*(goFloat*)*var_it);
            *(goUInt8*)*target_it = static_cast<goUInt8> (d);
            *((goUInt8*)*target_it + 1) = d;
            *((goUInt8*)*target_it + 2) = d;

            mean_it.incrementX(); var_it.incrementX(); image_it.incrementX(); target_it.incrementX();
        }
        mean_it.incrementY(); var_it.incrementY(); image_it.incrementY(); target_it.incrementY();
    }

    myPrivate->meanWindow.queue_draw ();
}
