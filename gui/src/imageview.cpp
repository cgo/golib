#include <gtkmm.h>
#include <gogui/imageview.h>
#include <golist.h>
#include <goautoptr.h>

namespace goGUI
{
    class ImageViewCurveAttr
    {
        public:
            ImageViewCurveAttr ()
                : thickness (1),
                  linestyle (Gdk::LINE_SOLID),
                  capstyle (Gdk::CAP_ROUND),
                  joinstyle (Gdk::JOIN_ROUND),
                  colour (3)
            {
                colour[0] = 1.0f;
                colour[1] = 0.0f;
                colour[2] = 0.0f;
            };

            ~ImageViewCurveAttr () {};

            int thickness;
            Gdk::LineStyle linestyle;
            Gdk::CapStyle capstyle;
            Gdk::JoinStyle joinstyle;
            goVectorf colour;
    };

    class ImageViewPrivate
    {
        public:
            ImageViewPrivate() : curves(), image(0) 
            {
                image.set (new goSignal3D<void>);
                image->setDataType (GO_UINT8);
                image->make (1,1,1,1,1,1,0,0,0,1);
                image->fill (0.0f);
            };
            ~ImageViewPrivate () {};

            goList<goMatrixd>          curves;
            goList<ImageViewCurveAttr> curveAttributes;  //= These are not used yet. Add them.
            goAutoPtr<goSignal3D<void> >  image;
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

/** 
 * @brief Draws the curve given by point configuration matrix \c curve.
 * 
 * @param curve N x 2 matrix containing points in its rows.
 */
void goGUI::ImageView::drawCurve (const goMatrixd& curve)
{
    goGUI::Draw draw (this->get_window());
    ImageViewCurveAttr attr;
    Gdk::Color c;
    c.set_rgb_p (attr.colour[0], attr.colour[1], attr.colour[2]);
    if (!(bool)(draw.getGC()->get_colormap()))
    {
        printf ("ImageView::drawCurve(): Do not have a colormap.");
    }
    //draw.getGC()->set_foreground (c);
    draw.getGC()->set_rgb_fg_color (c);
    draw.getGC()->set_line_attributes (attr.thickness, attr.linestyle, attr.capstyle, attr.joinstyle);
    draw.curve (curve);
}

/** 
 * @brief Draws the curves stored in this object.
 */
void goGUI::ImageView::drawCurves ()
{
    goList<goMatrixd>::Element* el = myPrivate->curves.getFrontElement();
    while (el)
    {
        this->drawCurve (el->elem);
        el = el->next;
    }
}

/** 
 * @brief Draws the image stored in this object.
 */
void goGUI::ImageView::drawImage ()
{
    goGUI::Draw draw (this->get_window());
    draw.image (*myPrivate->image);
}

/** 
 * @brief Sets the image to draw.
 *
 * The image gets deep copied.
 * 
 * @param image Image to draw.
 */
void goGUI::ImageView::setImage (const goSignal3DBase<void>& image)
{
    if (!myPrivate->image.isNull())
    {
        myPrivate->image->copy (image);
        this->set_size_request (image.getSizeX(), image.getSizeY());
    }
}

/** 
 * @brief Get the image from this object.
 * 
 * @return Reference to the stored image.
 */
goAutoPtr<goSignal3D<void> > goGUI::ImageView::getImage ()
{
    return myPrivate->image;
}

/** 
 * @brief Adds a curve to the list of curves in this object.
 * 
 * @param M Point configuration matrix, N x 2, one point per row.
 */
void goGUI::ImageView::addCurve (const goMatrixd& M)
{
    myPrivate->curves.append(M);
}

/** 
 * @brief Get list of curve point configuration matrices.
 * 
 * @return List of matrices.
 */
const goList<goMatrixd>& goGUI::ImageView::getCurves () const
{
    return myPrivate->curves;
}

/** 
 * @brief Remove curve \c i.
 * 
 * @param i Index of curve to remove.
 */
void goGUI::ImageView::removeCurve (goIndex_t i)
{
    myPrivate->curves.remove (myPrivate->curves (i));
}

/** 
 * @brief Set the curve points of curve \c i.
 * 
 * @param i Index of curve.
 * @param M N x 2 Matrix of curve points.
 */
void goGUI::ImageView::setCurve (goIndex_t i, const goMatrixd& M)
{
    goList<goMatrixd>::Element* el = myPrivate->curves (i);
    if (!el)
        return;
    el->elem = M;
}

/** 
 * @brief Draws everything.
 * 
 * @param event The event.
 * 
 * @return True if successful, false otherwise.
 */
bool goGUI::ImageView::on_expose_event (GdkEventExpose* event)
{
    this->drawImage ();
    this->drawCurves ();
    return Gtk::Widget::on_expose_event (event);
}
