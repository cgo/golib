#include "gaussimagecontrol.h"
#include <gogui/imageview.h>
#include <gogaussimage.h>
#include <gtkmm.h>

class GaussImageControlPrivate
{
    public:
        GaussImageControlPrivate ()
            : meanImageView (),
              varianceImageView (),
              meanWindow (),
              varianceWindow (),
              gaussImage ()
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

        goGaussImage     gaussImage;
};

GaussImageControl::GaussImageControl ()
    : goGUI::Control ("Gauss Image"),
      myPrivate (0)
{
    myPrivate = new GaussImageControlPrivate;
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

    myPrivate->gaussImage.update (*image, 5);
    const int source_chan[] = {0, 0, 0};
    const int target_chan[] = {0, 1, 2};
    goSignal::convert (myPrivate->gaussImage.getMean(), *view1.getImage(0), source_chan, target_chan, 3);
    goSignal::convert (myPrivate->gaussImage.getVariance(), *view2.getImage(0), source_chan, target_chan, 3);

    myPrivate->meanWindow.queue_draw ();
    myPrivate->varianceWindow.queue_draw ();
}
