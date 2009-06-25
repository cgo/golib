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

            goCaller1 <void, goAutoPtr<goSignal3DBase<void> > >& getImageChangedCaller ();   //= Called when the current image changes

        protected:
            void treeRowActivated (const Gtk::TreeModel::Path& path, Gtk::TreeViewColumn* col);
            void treeViewButtonPressed (GdkEventButton*);
            void treeDeleteImage ();

        private:
            ImageControlPrivate* myPrivate;
    };
};

#endif
