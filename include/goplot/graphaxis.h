/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOPLOT_GRAPHAXIS_H
#define GOPLOT_GRAPHAXIS_H

#include <goplot/plot.h>
#include <goplot/object2dpoints.h>
#include <goplot/object2dtext.h>

#include <vector>
#include <algorithm>

namespace goPlot
{
    // template <class Points, class Real> class Object2DPoints;

    /** @addtogroup cairoplot
     * @{
     */
    /** 
     * @brief Axis.
     */
    class GraphAxis : public Object2DPoints
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
            GraphAxis (int index = 0, double l = 0.0, double u = 1.0);

            virtual ~GraphAxis ();

            double length ();

            real x () const;
            real y () const;
            void setPosition (real x, real y);

            const char* ticsFont () const;
            void setTicsFont (const char* f);

            bool visible () const;

            void setVisible (bool v);

            void configure (int c);
            
            int configuration () const;

            void enableTics (bool f);

            void enableTicsText (bool f);

            virtual void draw ();

            void drawTics (cairo_t* cr);

            double lower () const;
            double upper () const;
            void   setLower (double l);
            void   setUpper (double u);
            int    index () const;

            void   setIndex (int i);

            void   setTics (const std::vector<real>& t);

            void setTics (real start, real step, real end);

            void setTics (int count = 10);

            void updateTicsText ();

            void drawTicsText ();

            void getTicsTextPathExtents (double& x1, double& y1, double& x2, double& y2);

            void getBorderHint (double w, double h, double& x1, double& y1, double& x2, double& y2);

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

    /** @} */
};
#endif
