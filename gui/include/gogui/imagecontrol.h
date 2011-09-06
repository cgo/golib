/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGUI_IMAGECONTROL_H
#define GOGUI_IMAGECONTROL_H

#include <gogui/control.h>
#include <gogui/imageview.h>
#ifndef GOFUNCTOR_H
# include <gofunctor.h>
#endif

namespace goGUI
{
    class ImageControlPrivate;

/** @addtogroup gui
 * @{
 */
    /** 
     * @brief Control widget for images in an ImageView.
     *
     * This control is used with an ImageView object.
     * It displays a Gtk::TreeView showing the images in the ImageView
     * and lets the user do things like deleting images and re-ordering
     * them by drag and drop.
     */
    class ImageControl : public Control
    {
        public:
            ImageControl ();
            virtual ~ImageControl ();

            void loadImage ();
            void addImage (goAutoPtr<goSignal3DBase<void> > img);

            void setImageView (ImageView* iv);

            void imageViewChanged (int); //= "Slot" connected to ImageView::changedCaller()
            //void selectionChanged (); //= Slot for the Gtk widget
            void treeViewDragEnd (const Glib::RefPtr<Gdk::DragContext>& context);

            goCaller1 <void, goAutoPtr<goSignal3DBase<void> > >& getImageChangedCaller ();   //= Called when the current image changes

        protected:
            void treeRowActivated (const Gtk::TreeModel::Path& path, Gtk::TreeViewColumn* col);
            void treeViewButtonPressed (GdkEventButton*);
            void treeDeleteImage ();

        private:
            ImageControlPrivate* myPrivate;
    };
/** \} */
};

#endif
