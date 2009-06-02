#ifndef GOPLOT_TEXTTRAITS_H
#define GOPLOT_TEXTTRAITS_H

#include <goplot/plot.h>
#include <string>

 #include <pango/pangocairo.h>

//struct cairo_t;
//struct PangoLayout;

namespace goPlot
{
    /** @addtogroup cairoplot
     * @{
     */
    /** 
     * @brief Text properties.
     * Contains colour and font.
     */
    class TextTraits
    {
        public:
            TextTraits ();

            ~TextTraits ();

            TextTraits (const TextTraits& other);

            TextTraits& operator= (const TextTraits& other);
            
            void setContext (cairo_t* cr);

            void apply ();

            const RGBA& colour () const;
            void        setColour (const RGBA& c);

            const std::string font () const;
            void              setFont (const char* f);

            PangoLayout* layout ();
            const PangoLayout* layout () const;

        private:
            RGBA myColour;
            std::string myFont;

            cairo_t     *myCairoContext;
            PangoLayout *myLayout;
            // PangoFontDescription *myFontDesc;
    };
    /** @} */
};
#endif
