/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gtkmm.h>
#include <gotypes.h>
#include <gogui/draw.h>

goGUI::Draw::Draw (Glib::RefPtr<Gdk::Drawable> drawable)
    : myDrawable (drawable), myGC()
{
    this->myGC = Gdk::GC::create (this->myDrawable);
}

goGUI::Draw::~Draw ()
{
}

void goGUI::Draw::line (goDouble x0, goDouble y0, goDouble x1, goDouble y1)
{
//    int w,h;
//    this->myDrawable->get_size (w,h);
//    this->myDrawable->draw_line (this->myGC, int(::round(x0 * (float)w)), int(::round(y0 * (float)h)), 
//                                           int(::round(x1 * (float)w)), int(::round(y1 * (float)h)));
    this->myDrawable->draw_line (this->myGC, int(::round(x0)), int(::round(y0)), 
                                           int(::round(x1)), int(::round(y1)));
}

void goGUI::Draw::point (goDouble x0, goDouble y0)
{
//    int w,h;
//    this->myDrawable->get_size (w,h);
//    this->myDrawable->draw_point (this->myGC, int(::round(x0 * (float)w)), int(::round(y0 * (float)h)));
    this->myDrawable->draw_point (this->myGC, int(::round(x0)), int(::round(y0)));
}

void goGUI::Draw::curve (const goMatrixd& M)
{
    if (M.getRows() < 1)
    {
        return;
    }
    goSize_t sz = M.getRows();
    const goVectord row1, row2;
    for (goSize_t i = 0; i < sz - 1; ++i)
    {
        M.refRow (i, row1);
        M.refRow (i + 1, row2);
        this->line (row1[0], row1[1], row2[0], row2[1]);
    }
}

void goGUI::Draw::image (const goSignal3D<void>& image)
{
    if (image.getBlockSize() != image.getSize())
    {
        goLog::warning ("goGUI::Draw::image(): image must be linear.");
        return;
    }
    if (image.getChannelCount() != 3)
    {
        goLog::warning ("goGUI::Draw::image(): image must currently be rgb.");
        return;
    }
    if (image.getDataType().getID() != GO_UINT8)
    {
        goLog::warning ("goGUI::Draw::image(): image must currently be uint8.");
        return;
    }
    
    this->myDrawable->draw_rgb_image (this->myGC, 0, 0, image.getSizeX(), image.getSizeY(), Gdk::RGB_DITHER_NONE, (const goUInt8*)image.getPtr(), image.getSizeX() * image.getChannelCount() * image.getDataType().getSize());
}

Glib::RefPtr<Gdk::GC> goGUI::Draw::getGC ()
{
    return this->myGC;
}
