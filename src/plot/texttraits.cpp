/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goplot/texttraits.h>

#include <goplot/plot.h>
#include <string>

#include <pango/pangocairo.h>

namespace goPlot
{
    TextTraits::TextTraits () 
        : myColour (0.0, 0.0, 0.0, 1.0),
        myFont ("sans normal 12"),
        myCairoContext (0),
        myLayout (0)
          // myFontDesc (0)
    { 
    }

    TextTraits::~TextTraits ()
    {
        if (myLayout)
        {
            g_object_unref (myLayout);
            myLayout = 0;
        }
    }

    TextTraits::TextTraits (const TextTraits& other)
        : myColour (other.colour ()),
        myFont (other.font ().c_str ()),
        myCairoContext (0),
        myLayout (0)
    {
        this->setContext (const_cast<cairo_t*> (other.myCairoContext));
    }

    TextTraits& TextTraits::operator= (const TextTraits& other)
    {
        this->setColour (other.colour ());
        this->setFont (other.font ().c_str ());
        this->setContext (const_cast<cairo_t*> (other.myCairoContext));
        return *this;
    }

    void TextTraits::setContext (cairo_t* cr)
    {
        myCairoContext = cr;

        if (myLayout)
        {
            g_object_unref (myLayout);
            myLayout = 0;
        }

        if (cr)
        {
            myLayout = pango_cairo_create_layout (cr);
        }
        //if (myFontDesc)
        //{
        //    pango_font_description_free (myFontDesc);
        //    myFontDesc = 0;
        //}
    }

    /** 
     * @brief Applies the properties to the current context.
     * Sets colour and creates and sets a current font description.
     */
    void TextTraits::apply ()
    {
        cairo_t* cr = myCairoContext;
        if (!cr || !this->myLayout)
            return;

        cairo_set_source_rgba (cr, myColour.r, myColour.g, myColour.b, myColour.a);
        // pango_layout_set_text (pl, "Text", -1);
        PangoFontDescription *desc = pango_font_description_from_string (myFont.c_str ());
        // pango_font_description_set_absolute_size (desc, 12 * PANGO_SCALE);  //= Absolute size, 12 pixel font 
        pango_layout_set_font_description (myLayout, desc);
        pango_font_description_free (desc);
    }

    /** 
     * @brief Gets the colour.
     * 
     * @return Font colour.
     */
    const RGBA& TextTraits::colour () const { return myColour; }
    /** 
     * @brief Sets the colour.
     * 
     * @param c Font colour.
     */
    void        TextTraits::setColour (const RGBA& c) { myColour = c; }

    /** 
     * @brief Gets the textual font description.
     * 
     * @return Font description text.
     */
    const std::string TextTraits::font () const { return myFont; }
    /** 
     * @brief Sets the textual font description.
     *
     * The description is passed to pango. See the documentation
     * of \c pango_font_description_from_string().
     * 
     * @param f Textual font description.
     */
    void              TextTraits::setFont (const char* f) { myFont = f; }

    /** 
     * @brief Gets the pango layout for this text object.
     * 
     * Probably only needed in the drawing code.
     *
     * @return Pointer to PangoLayout
     */
    PangoLayout* TextTraits::layout () { return myLayout; }
    /** 
     * @brief Gets the pango layout for this text object.
     * 
     * Probably only needed in the drawing code.
     *
     * @return Pointer to PangoLayout
     */
    const PangoLayout* TextTraits::layout () const { return myLayout; }
};
