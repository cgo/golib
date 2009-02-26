#ifndef GOPLOT_LAYOUT_H
#define GOPLOT_LAYOUT_H

#include <goplot/plot.h>

namespace NSPACE
{
    class LayoutGraph : public Graph
    {
        public:
            LayoutGraph (int dim = 2)
                : Graph (dim)
            {
            }

        private:
            real myPosX;
            real myPosY;

    };
    /*
     * @brief 
     * Hier ist noch nicht das letzte Wort gesprochen .. vielleicht doch etwas freier gestalten.
     * Graph + Position + Titel + ...
     */
    class Layout : public Object2D
    {
        public:
            Layout ()
                : Object2D (),
                  myGraphs (),
                  myWidth (1),
                  myHeight (1)
            {
            }

            virtual ~Layout ()
            {
            }

            void setSize (size_t w, size_t h)
            {
                myWidth = w;
                mySize = h;
                myGraphs.resize (w * h);
            }

            AutoPtr<Graph> graph (size_t x, size_t y)
            {
                size_t i = x + y * myWidth;
                if (i < myGraphs.size ())
                {
                    return myGraphs[i];
                }

                throw std::exception ();
            }

            void setGraph (size_t x, size_t y, AutoPtr<Graph> g)
            {
            }

            virtual void draw ()
            {
            }

        private:
            std::vector <AutoPtr<Graph> >  myGraphs;
            size_t myWidth; //= Width in graphs
            size_t myHeight; //= Height in graphs
    };
};

#endif
