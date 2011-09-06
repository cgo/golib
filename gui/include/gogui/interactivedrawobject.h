/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGUI_INTERACTIVEDRAWOBJECT_H
#define GOGUI_INTERACTIVEDRAWOBJECT_H

#include <list>
#include <gomath.h>

namespace goGUI
{

    class InteractiveDrawObject
    {
        public:
            virtual void apply () = 0; //= Apply properties of this object to the T type object 

            std::list<goVectord>&       getEditPoints () { return myEditPoints; }
            const std::list<goVectord>& getEditPoints () const { return myEditPoints; }

        protected:
            InteractiveDrawObject () 
                : myEditPoints () {}

        private:
            std::list<goVectord> myEditPoints;
    };

    template <class T>
        class InteractiveDrawObjectT : public InteractiveDrawObject
        {
            public:
                InteractiveDrawObjectT (goAutoPtr<T> object_ptr = 0);
                // virtual ~InteractiveDrawObject ();

                void setObject (goAutoPtr<T> object_ptr);
                goAutoPtr<T> getObject ();

                virtual void apply (); //= Apply properties of this object to the T type object 

            private:
                goAutoPtr<T>         myDrawObject;  //= T should be something supporting draw(), 
                                                    //= like the Object2D* classes used in goPlot::Graph Cairo plots.
        };
};

#endif
