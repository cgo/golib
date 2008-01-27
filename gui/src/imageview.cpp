#include <gtkmm.h>
#include <gogui/imageview.h>
#include <golist.h>

namespace goGUI
{
    class ImageViewPrivate
    {
        public:
            ImageViewPrivate() : curves(), image() {};
            ~ImageViewPrivate () {};

            goList<goMatrixd> curves;
            goSignal3D<void>  image;
    };
};

goGUI::ImageView::ImageView ()
    : Gtk::DrawingArea (), myPrivate(0)
{
    myPrivate = new ImageViewPrivate;
    this->add_events (Gdk::EXPOSURE_MASK | Gdk::POINTER_MOTION_MASK | Gdk::BUTTON_PRESS_MASK | Gdk::BUTTON_RELEASE_MASK);
}

goGUI::ImageView::~ImageView ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void goGUI::ImageView::drawCurve (const goMatrixd& curve)
{
    goGUI::Draw draw (this->get_window());
    Gdk::Color c;
    c.set_rgb_p (1.0, 0.0, 0.0);
    if (!(bool)(draw.getGC()->get_colormap()))
    {
        printf ("ImageView::drawCurve(): Do not have a colormap.");
    }
    //draw.getGC()->set_foreground (c);
    draw.getGC()->set_rgb_fg_color (c);
    draw.getGC()->set_line_attributes (3, Gdk::LINE_SOLID, Gdk::CAP_ROUND, Gdk::JOIN_ROUND);
    draw.curve (curve);
}

void goGUI::ImageView::drawCurves ()
{
    goList<goMatrixd>::Element* el = myPrivate->curves.getFrontElement();
    while (el)
    {
        this->drawCurve (el->elem);
        el = el->next;
    }
}

void goGUI::ImageView::drawImage ()
{
    goGUI::Draw draw (this->get_window());
    draw.image (myPrivate->image);
}

void goGUI::ImageView::setImage (const goSignal3DBase<void>& image)
{
    myPrivate->image.copy (image);
    this->set_size_request (image.getSizeX(), image.getSizeY());
}

goSignal3D<void>& goGUI::ImageView::getImage ()
{
    return myPrivate->image;
}

void goGUI::ImageView::addCurve (const goMatrixd& M)
{
    myPrivate->curves.append(M);
}

goList<goMatrixd>& goGUI::ImageView::getCurves ()
{
    return myPrivate->curves;
}

bool goGUI::ImageView::on_expose_event (GdkEventExpose* event)
{
    this->drawImage ();
    this->drawCurves ();
    return Gtk::Widget::on_expose_event (event);
}
