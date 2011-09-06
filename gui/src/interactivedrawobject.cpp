/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogui/interactivedrawobject.h>
#include <goplot/object2dbox.h>

template <class T>
goGUI::InteractiveDrawObjectT<T>::InteractiveDrawObjectT (goAutoPtr<T> object_ptr)
    : InteractiveDrawObject (),
      myDrawObject (0)
{
    this->setObject (object_ptr);
}

namespace goGUI
{
    template <>
        void InteractiveDrawObjectT < goPlot::Object2DBox > ::setObject (goAutoPtr<goPlot::Object2DBox> object_ptr)
        {
            if (object_ptr)
            {
                this->getEditPoints().clear ();
                goVectord p (2);
                object_ptr->getCorners().refRow (0, p);
                this->getEditPoints().push_back (p);
                object_ptr->getCorners().refRow (1, p);
                this->getEditPoints().push_back (p);
            }

            myDrawObject = object_ptr;
        }

    template <>
        void InteractiveDrawObjectT<goPlot::Object2DBox>::apply ()
        {
            if (myDrawObject.isNull())
                return;

            if (getEditPoints().size() < 2)
                return;

            goVectord& p1 = *this->getEditPoints().begin();
            goVectord& p2 = *(++this->getEditPoints().begin());

            myDrawObject->setCorners (p1[0], p1[1], p2[0], p2[1]);
        }
};

template <class T>
goAutoPtr<T> goGUI::InteractiveDrawObjectT<T>::getObject ()
{
    return this->myDrawObject;
}

template <class T>
void goGUI::InteractiveDrawObjectT<T>::setObject (goAutoPtr<T> object_ptr)
{
    goLog::warning ("goGUI::InteractiveDrawObject::setObject(): don't know how to set for this type.");
}

template <class T>
void goGUI::InteractiveDrawObjectT<T>::apply ()
{
    goLog::warning ("goGUI::InteractiveDrawObject::apply(): don't know how to apply for this type.");
}


template class goGUI::InteractiveDrawObjectT <goPlot::Object2DBox>;
