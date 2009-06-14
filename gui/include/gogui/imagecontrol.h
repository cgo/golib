#ifndef GOGUI_IMAGECONTROL_H
#define GOGUI_IMAGECONTROL_H

#include <gogui/control.h>
#include <gogui/imageview.h>

namespace goGUI
{
    class ImageControlPrivate;

    class ImageControl : public Control
    {
        public:
            ImageControl ();

            void loadImage ();
            void addImage (goAutoPtr<goSignal3DBase<void> > img);

            void setImageView (ImageView* iv);

            void imageViewChanged (int); //= "Slot" connected to ImageView::changedCaller()
            void selectionChanged (); //= Slot for the Gtk widget

        protected:
            void treeRowActivated (const Gtk::TreeModel::Path& path, Gtk::TreeViewColumn* col);

        private:
            ImageControlPrivate* myPrivate;
    };
};

#endif
