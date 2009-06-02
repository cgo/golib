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

            Object2DText (const Object2DText& other);

            virtual ~Object2DText ();
            
            operator const char* () const;
            Object2DText& operator= (const char* t);
            Object2DText& operator= (const Object2DText& other);

            std::string& string (); 
            const std::string& string () const;

            TextTraits& traits ();
            const TextTraits& traits () const;

            real x () const;
            real y () const;
            real relDx () const;
            real relDy () const;

            void setPosition (real x, real y, real rel_dx = 0.0, real rel_dy = 0.0);

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
