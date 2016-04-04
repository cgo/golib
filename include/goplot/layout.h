/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOPLOT_LAYOUT_H
#define GOPLOT_LAYOUT_H

#include <sys/_types/_size_t.h>
#include <algorithm>
#include <cstdio>
#include <exception>
#include <list>
#include <vector>

// #include "/usr/local/Cellar/cairo/1.14.6_1/include/cairo/cairo.h"
#include "goautoptr.h"
#include "graph.h"
#include "plot.h"

namespace NSPACE
{
    class LayoutGraph 
    {
        public:
            LayoutGraph (size_t gridX = 0, size_t gridY = 0, size_t extX = 1, size_t extY = 1)
                : 
                  myGridPosX (gridX),
                  myGridPosY (gridY),
                  myExtentX (extX),
                  myExtentY (extY)
            {
            }

            size_t posX () const { return myGridPosX; }
            size_t posY () const { return myGridPosY; }
            size_t extX () const { return myExtentX; }
            size_t extY () const { return myExtentY; }

            void set (size_t gridX = 0, size_t gridY = 0, size_t extX = 1, size_t extY = 1)
            {
                myGridPosX = gridX;
                myGridPosY = gridY;
                myExtentX = extX;
                myExtentY = extY;
            }

        private:
            size_t myGridPosX;
            size_t myGridPosY;
            size_t myExtentX;
            size_t myExtentY;
    };
    /*
     * @brief 
     * Hier ist noch nicht das letzte Wort gesprochen .. vielleicht doch etwas freier gestalten.
     * Graph + Position + Titel + ...
     */
    class Layout : public Graph
    {
        public:
            Layout (int dim = 2)
                : Graph (dim),
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
                myHeight = h;
                myGraphs.resize (w * h);
                myLayouts.resize (w * h);
            }

            void updateLayout ()
            {
                this->objects2D().clear ();
                const size_t sz = myGraphs.size ();
                for (size_t i = 0; i < sz; ++i)
                {
                    goAutoPtr<Graph>& g = myGraphs[i];
                    if (!g.isNull ())
                    {
                        LayoutGraph& l = myLayouts[i];

                        //g->setTransform (Trafo2D<real> (1.0 / float (myWidth) * float (l.extX ()), 
                        //                                0.0, 
                        //                                0.0, 
                        //                                1.0 / float (myHeight) * float (l.extY ()),
                        //                                0.0, 1.0));


                        g->setTransform (Trafo2D (1.0 / float (myWidth) * float (l.extX ()),
                                     0.0, 
                                     0.0, 
                                     1.0 / float (myHeight) * float (l.extY ()),
                                     float (l.posX()) / float (myWidth), 
                                     1.0 - float (l.posY() + l.extY()) / float (myHeight)));

#if 1
                        double space_x_0 = 0.0, space_y_0 = 0.0, space_x_1 = 0.0, space_y_1 = 0.0;

                        g->getBorderHint (1.0, 1.0, space_x_0, space_y_0, space_x_1, space_y_1);
                        
                        if (this->context ())
                        {
                            double dummy = 0.0;
                            cairo_save (this->context ());
                            this->applyTransform (this->context ());
                            g->applyTransform (this->context ());
                            cairo_device_to_user_distance (this->context (), &space_x_0, &dummy);
                            dummy = 0.0;
                            cairo_device_to_user_distance (this->context (), &space_x_1, &dummy);
                            dummy = 0.0;
                            cairo_device_to_user_distance (this->context (), &dummy, &space_y_0);
                            dummy = 0.0;
                            cairo_device_to_user_distance (this->context (), &dummy, &space_y_1);
                            cairo_restore (this->context ());
                        }

                        printf ("Borderhint: %f %f %f %f\n", space_x_0, space_x_1, space_y_0, space_y_1);

                        g->setTransform (Trafo2D (1.0 / float (myWidth) * float (l.extX ()) - space_x_0 - space_x_1,
                                    0.0, 
                                    0.0, 
                                    1.0 / float (myHeight) * float (l.extY ()) - space_y_0 - space_y_1,
                                    float (l.posX()) / float (myWidth) + space_x_0, 
                                    1.0 - float (l.posY() + l.extY()) / float (myHeight) - space_y_0 ));
#endif
                        this->add (g);
                    }
                }
            }

            goAutoPtr<Graph> graph (size_t x, size_t y)
            {
                size_t i = x + y * myWidth;
                if (i < myGraphs.size ())
                {
                    return myGraphs[i];
                }

                throw std::exception ();
            }

            void setGraph (goAutoPtr<Graph> g, size_t x, size_t y, size_t extent_x = 1, size_t extent_y = 1)
            {
                size_t i = x + y * myWidth;
                if (i >= myGraphs.size ())
                {
                    this->setSize (std::max (myWidth, x + 1), std::max (myHeight, y + 1));
                }

                myGraphs[i] = g;
                myLayouts[i].set (x, y, extent_x, extent_y);
            }

            virtual void draw ()
            {
                this->updateLayout ();
                Graph::draw ();
            }
#if 0
            virtual void draw ()
            {
                cairo_t* cr = this->context ();

                if (!cr)
                    return;

                cairo_save (cr);

                this->applyTransform (cr);

                std::vector<AutoPtr<Graph> >::iterator it = myGraphs.begin ();
                for (; it != myGraphs.end (); ++it)
                {
                    if (!it->isNull ())
                    {
                        (*it)->draw ();
                    }
                }

                cairo_restore (cr);
            }
#endif

        private:
            std::vector <goAutoPtr<Graph> >  myGraphs;
            std::vector <LayoutGraph>        myLayouts;
            size_t myWidth; //= Width in graphs
            size_t myHeight; //= Height in graphs
    };

};

#endif
