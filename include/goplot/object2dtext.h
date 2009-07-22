#ifndef GOPLOT_OBJECT2DTEXT_H
#define GOPLOT_OBJECT2DTEXT_H

#include <goplot/plot.h>
#include <goplot/texttraits.h>
#include <string>

// #include <pango/pangocairo.h>

namespace goPlot
{
    /** @addtogroup cairoplot
     * @{
     */
    /** 
     * @brief 2D text drawing class. Uses pango with cairo.
     *
     * A position can be set using setPosition() (see there).
     * The text is then drawn with the properties in TextTraits accessible by traits().
     *
     * @bug FIXME: rel_x|y funktioniert nicht mit markup Text!
     */
    class Object2DText : public Object2D
    {
        public:
            Object2DText ();
            Object2DText (const char* c);

            Object2DText (const goPlot::Object2DText& other);

            virtual ~Object2DText ();
            
            operator const char* () const;
            goPlot::Object2DText& operator= (const char* t);
            goPlot::Object2DText& operator= (const goPlot::Object2DText& other);

            std::string& string (); 
            const std::string& string () const;

            goPlot::TextTraits& traits ();
            const goPlot::TextTraits& traits () const;

            goPlot::real x () const;
            goPlot::real y () const;
            goPlot::real relDx () const;
            goPlot::real relDy () const;

            void setPosition (goPlot::real x, goPlot::real y, goPlot::real rel_dx = 0.0, goPlot::real rel_dy = 0.0);

            virtual void setContext (cairo_t *context);

            virtual void draw ();

            void addToPath ();

        private:
            std::string myString;
            TextTraits  myTraits;
            real        myX;
            real        myY;
            real        myRelDx;
            real        myRelDy;
    };
    /** @} */
};

#endif
