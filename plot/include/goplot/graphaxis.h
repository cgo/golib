#ifndef GOPLOT_GRAPHAXIS_H
#define GOPLOT_GRAPHAXIS_H

#include <goplot/plot.h>
#include <goplot/object2dpoints.h>
#include <goplot/object2dtext.h>

#include <vector>
#include <algorithm>

namespace NSPACE
{
    // template <class Points, class Real> class Object2DPoints;

    /** 
     * @brief Axis.
     */
    class GraphAxis : public Object2DPoints <Points2DSimple <real>, real>
    {
        public:
            enum Conf
            {
                POS_MASK = 7,
                //= Positions:
                BOTTOM = 1,
                LEFT = 2,
                TOP = 3,
                RIGHT = 4,
                //= Flags:
                MIDDLE = 1 << 4,
                TICS = 1 << 5,
                TICS_TEXT = 1 << 6,
                VISIBLE = 1 << 7
            };

        public:
            /** 
             * @brief Constructor.
             * 
             * Makes an axis extending from \c l to \u.
             * The \c index indicates which axis it is --- 
             * in 2D, 0 indicates the x axis, 1 indicates the y axis.
             * If, for example, you want to make a graph for a sine function in the
             * range from 0 to 2 pi, you can set axis 0 to \c l = 0 and \c u = 2 * M_PI,
             * and axis 1 to \c l = -1 and \c u = 1.
             *
             * @param index This axis' index
             * @param l Lower end of range
             * @param u Upper end of range
             */
            GraphAxis (int index = 0, double l = 0.0, double u = 1.0)
                : Object2DPoints <Points2DSimple <real>, real> (2),
                  myLower (l), 
                  myUpper (u),
                  myIndex (index),
                  myTics (),
                  myTicsLineTraits (),
                  myTicsText (),
                  myTicsFont ("sans normal light 7"),
                  myPosX (0.0),
                  myPosY (0.0),
                  myConf (LEFT | TICS | TICS_TEXT | VISIBLE)
            {
                //= Set some defaults
                this->lineTraits().setColour (RGBA (0.0, 0.0, 0.0, 0.5));
                myTicsLineTraits.setColour (RGBA (1.0, 0.0, 0.0, 0.5));
                myTicsLineTraits.setWidth (1.0);
                this->setIndex (myIndex);
            }

            virtual ~GraphAxis ()
            {
            }

            /** 
             * @brief Returns the length of the axis (upper end - lower end).
             * 
             * @return Length of this axis
             */
            double length () 
            {
                return ::fabs (myUpper - myLower);
            }

            /** 
             * @brief x position of this axis.
             * Only used by configure () so far, will be removed.
             * @deprecate
             * 
             * @return x position of this axis
             */
            real x () const { return myPosX; }
            /** 
             * @brief y position of this axis.
             * Only used by configure () so far, will be removed.
             * @deprecate
             * 
             * @return y position of this axis
             */
            real y () const { return myPosY; }
            /** 
             * @brief Sets position of axis.
             * Only used by configure () so far, will be removed.
             * @deprecate
             * @param x X position in (0,1)
             * @param y Y position in (0,1)
             */
            void setPosition (real x, real y) { myPosX = x; myPosY = y; }

            /** 
             * @brief Get the tics font (as a descriptive string).
             * @see setTicsFont()
             * 
             * @return const char* to the font description string.
             */
            const char* ticsFont () const { return myTicsFont.c_str (); }
            /** 
             * @brief Set font for tics text.
             * 
             * The font description is a string given to pango.
             * The default is "sans normal light 7".
             * See description of pango_font_description_from_string () 
             * <a href="http://library.gnome.org/devel/pango/unstable/pango-Fonts.html#pango-font-description-from-string">here</a>.
             *
             * @param f String describing the font.
             */
            void setTicsFont (const char* f) 
            {
                myTicsFont = f; 
                this->updateTicsText ();
            }

            /** 
             * @brief Check visibility.
             * 
             * @return True if axis is visible, false otherwise.
             */
            bool visible () const { return (myConf & VISIBLE) != 0; }

            /** 
             * @brief Set visibility.
             * 
             * @param v If true, axis is visible, if false, axis is invisible.
             */
            void setVisible (bool v) 
            {
                if (v)
                    myConf = myConf | VISIBLE;
                else
                    myConf = myConf & ~VISIBLE;
            }

            /** 
             * @brief Set the axis configuration.
             *
             * @note It is better to use setVisible(), enableTics(), enableTicsText().
             * 4 axes are configured when a Graph is constructed; fiddling should not be necessary.
             * @todo Replace this or leave it?
             *
             * <ul>
             *  <li> TICS: Sets if tics are drawn
             *  <li> TICS_TEXT: Sets if tic texts are drawn
             *  <li> MIDDLE: Axis will be positioned in the middle (this may be removed in the future)
             *  <li> VISIBLE: Switches visibility
             * </ul>
             *
             * @param c  One position out of {BOTTOM, LEFT, TOP, RIGHT} or'ed with {MIDDLE, TICS, TICS_TEXT}.
             */
            void configure (int c)
            {
                switch (c & POS_MASK)
                {
                    case RIGHT: 
                        {
                            if (c & MIDDLE)
                                this->setPosition (0.5, 0.0); 
                            else
                                this->setPosition (1.0, 0.0); 
                            Trafo2D<real> M (0.0, 1.0, -1.0, 0.0, myPosX, myPosY);
                            this->setTransform (M);
                        } break;
                    case BOTTOM: 
                        {
                            if (c & MIDDLE)
                                this->setPosition (0.0, 0.5); 
                            else
                                this->setPosition (0.0, 0.0); 
                            Trafo2D<real> M (1.0, 0.0, 0.0, 1.0, myPosX, myPosY);
                            this->setTransform (M);
                        } break;
                    case LEFT: 
                        {
                            if (c & MIDDLE)
                                this->setPosition (0.5, 0.0);
                            else
                                this->setPosition (0.0, 0.0);
                            Trafo2D<real> M (0.0, 1.0, 1.0, 0.0, myPosX, myPosY);
                            this->setTransform (M);
                        } break;
                    case TOP: 
                        {
                            if (c & MIDDLE)
                                this->setPosition (0.0, 0.5);
                            else
                                this->setPosition (0.0, 1.0);
                            Trafo2D<real> M (1.0, 0.0, 0.0, -1.0, myPosX, myPosY);
                            this->setTransform (M);
                        } break;
                }
                this->enableTics ((c & TICS) != 0);
                this->enableTicsText ((c & TICS_TEXT) != 0);
                myConf = c;
            }
            
            /** 
             * @brief Get the configuration.
             * @see configure()
             * 
             * @return Configuration
             */
            int configuration () const { return myConf; }

            /** 
             * @brief Sets or blanks tics flag.
             * 
             * @param f If true, tics flag will be set and tics will be drawn.
             * If false, the opposite will happen.
             */
            void enableTics (bool f) 
            {
                if (f)
                    myConf = myConf | TICS;
                else
                    myConf = myConf & ~TICS;
            }

            /** 
             * @brief Sets or blanks the tics text flag.
             * 
             * @param f If true, the flag will be set and tics texts will be drawn.
             * If false, the opposite will happen.
             */
            void enableTicsText (bool f) 
            {
                if (f)
                    myConf = myConf | TICS_TEXT;
                else
                    myConf = myConf & ~TICS_TEXT;
            }

            /** 
             * @brief Draws the axis and tics and tics texts according to flags.
             * @see enableTicsText(), enableTics()
             */
            virtual void draw ()
            {
                if (!this->visible ())
                {
                    return;
                }
                cairo_t* cr = this->context ();
                if (!cr)
                    return;

                cairo_save (cr);

                this->points().set (0, 0.0, 0.0);
                this->points().set (1, 1.0, 0.0);

                Object2DPoints <Points2DSimple <real>, real>::draw ();

                if ( (this->configuration () & TICS) != 0)
                {
                    this->drawTics (cr);
                }
                // this->updateTicsText ();
                if ( (this->configuration () & TICS_TEXT) != 0)
                {
                    this->drawTicsText ();
                }

                cairo_restore (cr);
            };

            /** 
             * @brief Draws tics.
             *
             * @param cr Cairo context
             */
            void drawTics (cairo_t* cr)
            {
                cairo_save (cr);
               
                myTicsLineTraits.apply (cr);
                this->applyTransform (cr);

                //cairo_set_line_width (cr, 0.001);
                //cairo_set_source_rgba (cr, 1.0, 0.0, 0.0, 1.0);

                real tic_length = 0.02;
                real f = 1.0 / (myUpper - myLower);
                real tic;
                real tic_outer = 0.0;
                real tic_inner = 1.0 - tic_outer;
                for (std::vector<real>::iterator it = myTics.begin (); it != myTics.end (); ++it)
                {
                    tic = (*it - myLower) * f;
                    cairo_move_to (cr, tic, -tic_outer * tic_length);
                    cairo_line_to (cr, tic, tic_inner * tic_length);
                }

                //= Debugging: draw last tic fat.
                //cairo_set_line_width (cr, 0.1);
                //cairo_set_source_rgba (cr, 0.0, 1.0, 0.0, 1.0);
                //cairo_move_to (cr, tic, -1.0 * tic_length);
                //cairo_line_to (cr, tic, 1.0 * tic_length);
                //cairo_stroke (cr);
                
                //= Set to identity matrix before stroke, so that line width is in device coordinates (e.g. pixels)
                cairo_identity_matrix (cr);
                cairo_stroke (cr);
                cairo_restore (cr);
            }

            /** 
             * @brief Get the lower end of range of this axis.
             * 
             * @return Lower end of range of this axis
             */
            double lower () const { return myLower; };
            /** 
             * @brief Get the upper end of range of this axis.
             * 
             * @return Upper end of range of this axis
             */
            double upper () const { return myUpper; };
            /** 
             * @brief Set the lower end of range of this axis.
             * 
             * @param l Lower end of range
             */
            void   setLower (double l) { myLower = l; };
            /** 
             * @brief Set the upper end of range of this axis.
             * 
             * @param l Upper end of range
             */
            void   setUpper (double u) { myUpper = u; };
            /** 
             * @brief Get the index of this axis.
             *
             * 0 corresponds to x axis, 1 to y axis, 2 to the upper x axis, 3 to the right y axis.
             * 
             * @return Axis index
             */
            int    index () const { return myIndex; };

            /** 
             * @brief Set the index of this axis.
             * @see index()
             *
             * Sets the index and configures this axis accordingly by calling configure().
             *
             * @param i This axis' index.
             */
            void   setIndex (int i) 
            { 
                myIndex = i; 
                switch (myIndex)
                {
                    case 0:
                        this->configure (BOTTOM | TICS | TICS_TEXT | VISIBLE);
                        break;
                    case 1:
                        this->configure (LEFT | TICS | TICS_TEXT | VISIBLE);
                        break;
                    case 2:
                        this->configure (TOP | TICS);
                        break;
                    case 3:
                        this->configure (RIGHT | TICS);
                        break;
                }
            }

            /** 
             * @brief Set a vector of tics.
             * 
             * The tics must be in the range of [this->lower(), this->upper()]
             * and indicate the positions at which to draw tics if the the tics flag
             * is enabled with enableTics().
             *
             * @param t Vector of tics positions
             */
            void   setTics (const std::vector<real>& t) 
            { 
                myTics = t; 
                this->updateTicsText ();
            }

            /** 
             * @brief Set tics using the given range.
             *
             * @see setTics(const std::vector<real>& t)
             *
             * Sets tics from start, using step, up to end (inclusive, if end is reached exactly).
             * Calls setTics(const std::vector<real>& t).
             *
             * @param start Start of range
             * @param step Step width
             * @param end End of range
             */
            void setTics (real start, real step, real end) 
            {
                if (start > end)
                    return;

                myTics.resize (0);
                myTics.reserve (int ((end - start) / step) + 1);
                for (; start <= end; start += step)
                {
                    myTics.push_back (start);
                }
                this->updateTicsText ();
            }

            /** 
             * @brief Set a certain amounf of equally spaced tics.
             * 
             * Calls setTics(real start, real step, real end) to set \c count
             * tics on this axis.
             *
             * @param count Number of tics to set
             */
            void setTics (int count = 10)
            {
                real l = this->lower ();
                real u = this->upper ();
                this->setTics (l, (u - l) / float (count), u);
            }

            /** 
             * @brief Updates the texts for all tics of this axis.
             *
             * @todo Add variable precision
             *
             * If the tics have been set newly, this is called by the setTics() methods
             * in order to get the tics texts right.
             */
            void updateTicsText ()
            {
                myTicsText.resize (0);
                for (std::vector<real>::iterator it = myTics.begin (); it != myTics.end (); ++it)
                {
                    char temp [255];
                    ::sprintf (temp, "%.2f", (float)*it);
                    Object2DText tt (temp);
                    // tt.traits ().setFont ("Purisa normal light 7");
                    tt.traits ().setFont (this->ticsFont ());
                    tt.setContext (this->context ());
                    myTicsText.push_back (tt);
                }
            }

            /** 
             * @brief Draws the tics texts.
             */
            void drawTicsText ()
            {
                cairo_t *cr = this->context ();
                if (!cr)
                    return;

                cairo_save (cr);

                this->applyTransform (cr);

                int i = 0;
                real f = 1.0 / (myUpper - myLower);
                int sz = int (myTicsText.size ());
                for (std::vector<Object2DText>::iterator it = myTicsText.begin (); it != myTicsText.end (); ++it, ++i)
                {
                    it->setContext (this->context ());
                    real dx = 0.0, dy = 0.0;
                    //= Depending on the axis, align the tic texts accordingly.
                    //= Adjust for the last and first tics, so that they don't get in each other's way.
                    //= The units of dx and dy are in text width and height.
                    switch (myIndex)
                    {
                        //= Bottom axis
                        case 0:
                            if (i > 0)
                                dx = -0.5;
                            if (i == sz - 1)
                                dx = -1.0;
                            dy = 0.0;
                            break;
                        //= Top axis
                        case 2:
                            if (i > 0)
                                dx = -0.5;
                            if (i == sz - 1)
                                dx = -1.0;
                            dy = -1.0;
                            break;
                        //= Left axis
                        case 1:
                            if (i > 0)
                            {
                                if (i == sz - 1)
                                    dx = 0.0;
                                else
                                    dx = -0.5;
                            }
                            else
                                dx = -1.0;
                            dy = -1.0;
                            break;
                        //= Right axis
                        case 3:
                            if (i > 0)
                            {
                                if (i == sz - 1)
                                    dx = 0.0;
                                else
                                    dx = -0.5;
                            }
                            else
                                dx = -1.0;
                            dy = 0.0;
                            break;
                        default:
                            break;
                    }
                    it->setPosition ((myTics[i] - myLower) * f, 0.0, dx, dy);
                    // printf ("Tic: %f\n", myTics[i]);
                    it->draw ();
                }

                cairo_restore (cr);
            }

            /** 
             * @brief Finds out the extents of the tics.
             *
             * Used by getBorderHint() to find out how much space should be
             * left around the axis.
             * 
             * <b>If I am not mistaken</b>, the extents should be returned in device coordinates.
             *
             * @param x1 Border at left
             * @param y1 Border at top
             * @param x2 Border at right
             * @param y2 Border at bottom
             */
            void getTicsTextPathExtents (double& x1, double& y1, double& x2, double& y2)
            {
                cairo_t *cr = this->context ();
                if (!cr)
                    return;

                cairo_save (cr);

                this->applyTransform (cr);

                int i = 0;
                real f = 1.0 / (myUpper - myLower);
                int sz = int (myTicsText.size ());
                for (std::vector<Object2DText>::iterator it = myTicsText.begin (); it != myTicsText.end (); ++it, ++i)
                {
                    it->setContext (this->context ());
                    real dx = 0.0, dy = 0.0;
                    //= Depending on the axis, align the tic texts accordingly.
                    switch (myIndex)
                    {
                        //= Bottom axis
                        case 0:
                            if (i > 0)
                                dx = -0.5;
                            if (i == sz - 1)
                                dx = -1.0;
                            dy = 0.0;
                            break;
                        //= Top axis
                        case 2:
                            if (i > 0)
                                dx = -0.5;
                            if (i == sz - 1)
                                dx = -1.0;
                            dy = -1.0;
                            break;
                        //= Left axis
                        case 1:
                            if (i > 0)
                            {
                                if (i == sz - 1)
                                    dx = 0.0;
                                else
                                    dx = -0.5;
                            }
                            else
                                dx = -1.0;
                            dy = -1.0;
                            break;
                        //= Right axis
                        case 3:
                            if (i > 0)
                            {
                                if (i == sz - 1)
                                    dx = 0.0;
                                else
                                    dx = -0.5;
                            }
                            else
                                dx = -1.0;
                            dy = 0.0;
                            break;
                        default:
                            break;
                    }
                    it->setPosition ((myTics[i] - myLower) * f, 0.0, dx, dy);
                    it->addToPath ();
                }

                cairo_stroke_extents (cr, &x1, &y1, &x2, &y2);
                cairo_restore (cr);
            }

            /** 
             * @brief Tries to figure out how much border is needed to display the text at the tics 
             * of the axes. Assuming currently that the axes are drawn at left and bottom.
             * 
             * @param w Width of window
             * @param h Height of window
             * @param x1 Border at left
             * @param y1 Border at top
             * @param x2 Border at right
             * @param y2 Border at bottom
             */
            void getBorderHint (double w, double h, double& x1, double& y1, double& x2, double& y2)
            {
                if (!this->visible ())
                {
                    x1 = 0.0;
                    y1 = 0.0;
                    x2 = 0.0;
                    y2 = 0.0;
                    return;
                }
                double xx1, xx2, yy1, yy2;
                this->getTicsTextPathExtents (xx1, yy1, xx2, yy2);
                //printf ("w, h: %lf, %lf\n", w, h);
                //printf ("Path extents: %lf, %lf, %lf, %lf\n", xx1, yy1, xx2, yy2);
                x1 = std::max (-xx1, 0.0);
                x2 = std::max ((xx2 - w) + x1, 0.0);
                y1 = std::max (-yy1, 0.0);
                y2 = std::max ((yy2 - h) + y1, 0.0);
                //printf ("x1, y1, x2, y2: %lf, %lf, %lf, %lf\n", x1, y1, x2, y2);
            }

        private:
            double            myLower;
            double            myUpper;
            int               myIndex;
            std::vector<real> myTics;
            LineTraits        myTicsLineTraits;
            std::vector<Object2DText> myTicsText;
            std::string       myTicsFont;

            real              myPosX;
            real              myPosY;
            int               myConf;
    };
};
#endif
