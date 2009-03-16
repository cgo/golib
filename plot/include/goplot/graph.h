#ifndef GOPLOT_GRAPH_H
#define GOPLOT_GRAPH_H

#include <goplot/plot.h>
#include <goplot/graphaxis.h>
#include <goplot/autoptr.h>

#include <exception>
#include <list>
#include <vector>

#include <assert.h>

namespace NSPACE
{
    /** 
     * @brief Graph. Contains multiple Object2D and some axes to be drawn.
     * Can also contain other Graphs.
     */
    class Graph : public Object2D
    {
        public:
            /** 
             * @brief Construct a graph (axes).
             * 
             * @param dim Dimension -- currently, only 2 is supported.
             */
            Graph (int dim = 2)
                : Object2D  (), 
                  myAxes    (), 
                  myObjects (), 
                  myDim     (dim), 
                  myTitle   ("")
            {
                if (dim < 1)
                    return;

                myAxes.resize (0);
                int axes = 0;
                if (dim == 2)
                {
                    axes = 4;
                }
                else
                {
                    throw std::exception ();
                }

                myAxes.reserve (axes);

                for (int i = 0; i < axes; ++i)
                {
                    myAxes.push_back (AutoPtr<GraphAxis> (new GraphAxis));
                    myAxes[i]->setIndex (i);
                    //if (axes <= 4)
                    //    myAxes[i]->configure (configs[i]);
                }

                this->axis (0)->setVisible (true);
                this->axis (1)->setVisible (true);
                this->axis (2)->setVisible (false);
                this->axis (3)->setVisible (false);
                //this->axis (0)->configure (this->axis(0)->configuration() | GraphAxis::MIDDLE);
                //this->axis (1)->configure (this->axis(1)->configuration() | GraphAxis::MIDDLE);

                this->axis (2)->enableTics (true);
                this->axis (2)->enableTicsText (true);
                this->axis (3)->enableTics (true);
                this->axis (3)->enableTicsText (true);
            }

            Graph (const Graph& other)
            {
                *this = other;
            }

            Graph& operator= (const Graph& other)
            {
                int dim = other.dim ();
                myDim = dim;
                myAxes.resize (0);
                myAxes.reserve (other.myAxes.size ());
                for ( size_t i = 0; i < other.myAxes.size (); ++i)
                {
                    myAxes.push_back (AutoPtr<GraphAxis> (new GraphAxis (*other.axis(i))));
                }

                //= FIXME === Pointers are copied, not objects. Is that wanted?
                myObjects = other.myObjects;

                //= Also use the same transformation
                this->setTransform (other.transform ());

                return *this;
            }

            virtual ~Graph ()
            {
            }

            void setDimensions (real xmin, real xmax, real ymin, real ymax)
            {
                this->axis(0)->setLower (xmin);
                this->axis(0)->setUpper (xmax);
                this->axis(1)->setLower (ymin);
                this->axis(1)->setUpper (ymax);
                this->axis(2)->setLower (xmin);
                this->axis(2)->setUpper (xmax);
                this->axis(3)->setLower (ymin);
                this->axis(3)->setUpper (ymax);
                //graph.axis(0)->lineTraits().setWidth (0.01);
                //graph.axis(1)->lineTraits().setWidth (0.01);
                this->axis(0)->setTics (10);
                this->axis(1)->setTics (10);
                this->axis(2)->setTics (15);
                this->axis(3)->setTics (15);
            }


            /** 
             * @brief Set the font for the tics text.
             *
             * Convenience function that sets the font of all axes to \c f.
             * @see GraphAxis::setTicsFont().
             *
             * @param f Font description text.
             */
            void setTicsFont (const char* f)
            {
                for (size_t i = 0; i < this->myAxes.size (); ++i)
                {
                    this->axis (i)->setTicsFont (f);
                }
            }

            int              dim () const { return myDim; }
            AutoPtr<GraphAxis>  axis (int i) 
            { 
                if (i >= 0 && i < int (this->myAxes.size ()))
                    return myAxes[i];

                throw std::exception (); // ("Graph::axis(): Index out of range");
            }

            const AutoPtr<GraphAxis> axis (int i) const 
            { 
                if (i >= 0 && i < this->dim ())
                    return myAxes[i];

                throw std::exception (); // ("Graph::axis(): Index out of range");
            }

            /** 
             * @brief Adds a Object2D to this graph.
             * 
             * @param o The object pointer (managed by a AutoPtr)
             */
            void add (AutoPtr<Object2D> o)
            {
                if (!o.isNull ())
                    o->setContext (this->context ());

                myObjects.push_back (o);
            }

            /** 
             * @brief Sets the title of the graph.
             *
             * The title is not drawn by the Graph::draw() method. 
             * It is only stored here and should be drawn by the class arranging and plotting the graphs.
             * 
             * @param t Title
             */
            void setTitle (const char* t)
            {
                myTitle = t;
            }

            /** 
             * @brief Get the title text.
             * @see setTitle()
             * 
             * @return The title text
             */
            const char* title () const { return myTitle.c_str (); }

            /** 
             * @brief Adds a text label.
             *
             * Convenience function that creates a Object2DText and
             * calls add().
             *
             * \c rel_x and \c rel_y can be used to align the label (see Object2DText::setPosition()).
             *
             * @param x X position
             * @param y Y position
             * @param label Label text
             * @param rel_x Relative x movement in units of text width
             * @param rel_y Relative y movement in units of text height
             *
             * @return Reference to the new Object2DText
             */
            Object2DText& addLabel (double x, double y, const char* label, double rel_x = 0.0, double rel_y = 0.0)
            {
                Object2DText* txt = new Object2DText (label);
                
                if (!txt)
                    throw std::exception ();

                txt->setPosition (x, y, rel_x, rel_y);
                this->add (AutoPtr<Object2D> (txt));
                return *txt;
            }

            /** 
             * @brief Sets the context and hands it on to all axes and drawable objects.
             * 
             * @param c The context
             */
            virtual void setContext (cairo_t* c)
            {
                Object2D::setContext (c);

                //= Draw axes
                for (int i = 0; i < int (myAxes.size ()); ++i)
                {
                    assert (!myAxes[i].isNull ());
                    myAxes[i]->setContext (c);
                }
                for (std::list<AutoPtr<Object2D> >::iterator it = myObjects.begin (); it != myObjects.end (); ++it)
                {
                    (*it)->setContext (c);
                }
            }

            void applyAxesTransform (cairo_t* c)
            {
                cairo_matrix_t M;
                cairo_matrix_init (&M, 1.0 / myAxes[0]->length (), 0, 0, 1.0 / myAxes[1]->length (), -myAxes[0]->lower() / myAxes[0]->length(), -myAxes[1]->lower() / myAxes[1]->length());
                cairo_transform (c, &M);
            }

            /** 
             * @brief Draws axes (according to their visibilities) and all objects.
             */
            virtual void draw ()
            {
                cairo_t* cr = this->context ();
                if (!cr)
                    return;

                cairo_save (cr);

                this->applyTransform (cr);
                
                //= Draw axes
                for (int i = 0; i < int (myAxes.size ()); ++i)
                {
                    assert (!myAxes[i].isNull ());
                    myAxes[i]->draw ();
                }

                this->applyAxesTransform (cr);

                //cairo_matrix_t Mo;
                //cairo_get_matrix (cr, &Mo);
                //cairo_matrix_multiply (&Mo, &Mo, &M);
                //cairo_set_matrix (cr, &Mo);
                //= Draw other objects
                for (std::list<AutoPtr<Object2D> >::iterator it = myObjects.begin (); it != myObjects.end (); ++it)
                {
                    (*it)->draw ();
                }
                cairo_restore (cr);
            };

            void getBorderHint (double w, double h, double& x1, double& y1, double& x2, double& y2)
            {
                cairo_t* cr = this->context ();
                if (!cr)
                    return;

                cairo_save (cr);
                
                cairo_new_path (cr);

                this->applyTransform (cr);

                x1 = 0.0;
                x2 = 0.0;
                y1 = 0.0;
                y2 = 0.0;
                //= Draw axes
                int sz = int (myAxes.size ());
                // int sz = 2;
                for (int i = 0; i < sz; ++i)
                {
                    double xx1, xx2, yy1, yy2;

                    assert (!myAxes[i].isNull ());
                    myAxes[i]->getBorderHint (w, h, xx1, yy1, xx2, yy2);
                    x1 = std::max (xx1, x1);
                    x2 = std::max (xx2, x2);
                    y1 = std::max (yy1, y1);
                    y2 = std::max (yy2, y2);
                }
                
                cairo_new_path (cr);

                cairo_restore (cr);
            }

        protected:
            std::list<AutoPtr<Object2D> > objects2D () { return myObjects; }

        private:
            std::vector<AutoPtr<GraphAxis> > myAxes;
            std::list<AutoPtr<Object2D> >    myObjects;
            int                              myDim;
            std::string                      myTitle;
    };
};

#endif
