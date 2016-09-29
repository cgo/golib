/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogui/imagecontrol.h>
#include <gogui/helper.h>
#include <gtkmm.h>

namespace goGUI
{
    class ImageModelColumns : public Gtk::TreeModelColumnRecord
    {
        public:
            ImageModelColumns ()
                // : Gtk::TreeModelColumnRecord ()
            {
                add (myColNumber);
                add (myColName);
            }

            Gtk::TreeModelColumn <goIndex_t>     myColNumber;   //= Index of the row is stored here
            Gtk::TreeModelColumn <Glib::ustring> myColName;     //= Textual description (ObjectName)
    };

    class ImageModelInfoColumns : public Gtk::TreeModelColumnRecord
    {
        public:
            ImageModelInfoColumns ()
            {
                add (myColStr);
            }

            Gtk::TreeModelColumn <Glib::ustring> myColStr;
    };

    class ImageControlPrivate
    {
        public:
            ImageControlPrivate ()
                : imageList (1),
                  imageView (0),
                  myColumns (),
                  myRefStore (),
                  myTreeView (),
                  myImageChangedCaller (),
                  myTreeContextMenu (Gtk::manage(new Gtk::Menu))
            {
                myRefStore = Gtk::ListStore::create (myColumns);
                myTreeView.set_model (myRefStore);
                myTreeView.append_column ("Number", myColumns.myColNumber);
                myTreeView.append_column ("Name", myColumns.myColName);
                myTreeView.set_headers_visible (false);
                myTreeView.set_reorderable (true);
                myTreeView.set_grid_lines (Gtk::TREE_VIEW_GRID_LINES_HORIZONTAL);

                myTreeContextMenu->set_visible();
                // myTreeView.set_hover_selection (true);
            }

            Gtk::ListViewText imageList;
            goGUI::ImageView* imageView;

            ImageModelColumns               myColumns;
            // Glib::RefPtr<Gtk::TreeStore>    myRefTreeStore;
            Glib::RefPtr<Gtk::ListStore>    myRefStore;
            Gtk::TreeView                   myTreeView;
            goCaller1 <void, goAutoPtr<goSignal3DBase<void> > > myImageChangedCaller;

            Gtk::Menu*                      myTreeContextMenu;
    };


};

goGUI::ImageControl::ImageControl ()
    : Control ("Image View"),
      myPrivate (0)
{
    myPrivate = new ImageControlPrivate;
    // myPrivate->imageList.signal_columns_changed ().connect (sigc::mem_fun (*this, &ImageControl::selectionChanged));

    myPrivate->myTreeView.signal_row_activated ().connect (sigc::mem_fun (*this, &ImageControl::treeRowActivated));
    myPrivate->myTreeView.signal_button_press_event ().connect_notify (sigc::mem_fun (*this, &ImageControl::treeViewButtonPressed));

    //= Drag and drop
    {
    	std::vector<Gtk::TargetEntry> targets(1);
    	targets[0].set_target(Glib::ustring("Image"));
    	targets[0].set_flags(Gtk::TARGET_SAME_APP);
    	targets[0].set_info(0);

    	myPrivate->myTreeView.drag_source_set(targets, Gdk::SHIFT_MASK, Gdk::ACTION_MOVE);
    	Glib::RefPtr<Gtk::TargetList> targetList = myPrivate->myTreeView.drag_dest_get_target_list();
    	targetList->add(targets);

    	myPrivate->myTreeView.signal_drag_end ().connect (sigc::mem_fun (*this, &ImageControl::treeViewDragEnd));
    }

    //= Context menu in treeview
    {
//        Gtk::Menu::MenuList& menulist = myPrivate->myTreeContextMenu.items ();

        Gtk::MenuItem *menuItem = Gtk::manage(new Gtk::MenuItem("_Delete"));

        menuItem->signal_activate().connect(sigc::mem_fun(*this, &ImageControl::treeDeleteImage));

        myPrivate->myTreeContextMenu->append(*menuItem);
        menuItem->set_visible();

//        menulist.push_back( Gtk::Menu_Helpers::MenuElem("_Delete",
//                    sigc::mem_fun(*this, &ImageControl::treeDeleteImage) ) );
    }
    myPrivate->myTreeContextMenu->accelerate (*this);

    Gtk::ScrolledWindow* sw = Gtk::manage (new Gtk::ScrolledWindow);
    sw->set_size_request (-1, 200);
    sw->add (myPrivate->myTreeView);
    this->add (*sw);
}

goGUI::ImageControl::~ImageControl ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void goGUI::ImageControl::treeViewButtonPressed (GdkEventButton* event)
{
    // bool return_value = goGUI::Control::on_button_press_event (event);

    if( (event->type == GDK_BUTTON_PRESS) && (event->button == 3) )
    {
        myPrivate->myTreeContextMenu->popup (event->button, event->time);
    }

    // return return_value;
}

//===============================
//= Put all selected Gtk::TreeModel::iterator objects into a vector.
class SelectedTreeModelIterators
{
    public:
        SelectedTreeModelIterators (Glib::RefPtr<Gtk::TreeSelection> selection)
            : myIterators ()
        {
            selection->selected_foreach_iter (sigc::mem_fun (*this, &SelectedTreeModelIterators::selected_row_callback));
        }

        void selected_row_callback (const Gtk::TreeModel::iterator& iter)
        {
            myIterators.push_back (iter);
        }

        std::vector<Gtk::TreeModel::iterator> myIterators;
};

//= Put the numbers on myColNumber in a vector (myIndex)
class TreeModelEnumerate
{
    public:
        TreeModelEnumerate (Glib::RefPtr<Gtk::TreeModel> model, goGUI::ImageControlPrivate* p)
            : myIndex (), myPrivate (p)
        {
            model->foreach_iter (sigc::mem_fun (*this, &TreeModelEnumerate::row_callback));
        }

        bool row_callback (const Gtk::TreeModel::iterator& iter)
        {
            Gtk::TreeModel::Row row = *iter;
            myIndex.push_back ((*row) [myPrivate->myColumns.myColNumber]);
            return false; //= Returning true stops the tree walk
        }

        std::vector<int> myIndex;
        goGUI::ImageControlPrivate* myPrivate;
};

//= Renumber all rows from 0 to N-1
class TreeModelRenumber
{
    public:
        TreeModelRenumber (Glib::RefPtr<Gtk::TreeModel> model, goGUI::ImageControlPrivate* p)
            : myI (0), myPrivate (p)
        {
            model->foreach_iter (sigc::mem_fun (*this, &TreeModelRenumber::row_callback));
        }

        bool row_callback (const Gtk::TreeModel::iterator& iter)
        {
            Gtk::TreeModel::Row row = *iter;
            (*row) [myPrivate->myColumns.myColNumber] = myI;
            ++myI;
            return false; //= Returning true stops the tree walk
        }

        int myI;
        goGUI::ImageControlPrivate* myPrivate;
};
//==============================

/** 
 * @brief Delete the current selection of images
 */
void goGUI::ImageControl::treeDeleteImage ()
{
    Glib::RefPtr<Gtk::TreeSelection> selection = myPrivate->myTreeView.get_selection ();
    // std::vector<Gtk::TreeModel::Path>::iterator it = selection->get_selected_rows ().begin ();

//    std::vector<Gtk::TreeModel::iterator> rm_iters;
//    for (; it != selection->get_selected_rows ().end (); ++it)
//    {
//        Gtk::TreeModel::iterator iter = myPrivate->myRefStore->get_iter (*it);
//        Gtk::TreeModel::Row row = *iter;
//        goIndex_t i = row[myPrivate->myColumns.myColNumber];
//        rm_iters.push_back (iter);
//        // myPrivate->myRefStore->erase (iter);
//    }


    SelectedTreeModelIterators sel_iters (myPrivate->myTreeView.get_selection ());

    myPrivate->myTreeView.set_model(Glib::RefPtr<Gtk::TreeModel>(NULL));
    for (size_t i = 0; i < sel_iters.myIterators.size (); ++i)
    {
        Gtk::TreeModel::Row row = *sel_iters.myIterators [i];
        myPrivate->imageView->removeImage (row [myPrivate->myColumns.myColNumber]);
        // FIXME: This crashes if we don't unset and then set the refstore again,
        // when sending a signal. Why?
        // We do also get a critical glib error GLib-CRITICAL **: g_sequence_get: assertion '!is_end (iter)' failed
        // when doing it like we do here. What is the correct way of removing an element from a refstore?
        myPrivate->myRefStore->erase (sel_iters.myIterators [i]);
        //= FIXME: Also remove from imageview!
    }
    myPrivate->myTreeView.set_model(myPrivate->myRefStore);

    TreeModelRenumber renumber (myPrivate->myRefStore, myPrivate);

    myPrivate->imageView->queue_draw ();
}

/** 
 * @brief Pop up a file open dialog and load an image.
 */
void goGUI::ImageControl::loadImage ()
{
    if (!myPrivate->imageView)
    {
        this->warning ("Image control: no ImageView set.\nCan not load images.");
    }

    goString fname = "";
    static goString start = "./";
    if (goGUI::getFilenameOpen (fname, start, "Load Image"))
    {
        fname.getPathName (start);
        goAutoPtr<goSignal3D<void> > image = new goSignal3D<void>;
        try
        {
            goFileIO::readImage (fname.toCharPtr (), image, true);
            goString fname_part;
            fname.getFileName (fname_part);
            image->setObjectName (fname_part);
            this->addImage (image);
            // myPrivate->imageVview->setImage (*image, 0);
            // this->imageView->setCurrentImage (0);
            myPrivate->imageView->queue_draw ();
            // return image;
            // this->control.setImage (image);
        }
        catch (goFileIOException& ex)
        {
            Gtk::MessageDialog dlg ("Reading image failed.");
            dlg.run ();
            // return nullptr;
        }
    }
}

/** 
 * @brief Add the image given as goAutoPtr
 * 
 * The image is just added to the ImageView, and is being deep copied.
 * The goAutoPtr is currently not copied.
 * The image is also automatically converted by ImageView to be displayable --- this means
 * the original data format is probably not retained in the copied version of the image.
 *
 * @param img Image to add.
 */
void goGUI::ImageControl::addImage (goAutoPtr<goSignal3DBase<void> > img)
{
    if (!myPrivate->imageView)
        return;

    //= FIXME: keep original images later and work with them -- now, just add it to the imageView

    //= Let imageview worry about the image format.
    myPrivate->imageView->setImage (*img, -1);
    myPrivate->imageView->setCurrentImage (myPrivate->imageView->getImageCount() - 1);
    myPrivate->imageView->getImage (myPrivate->imageView->getImageCount() - 1)->setObjectName (img->getObjectName());
    this->imageViewChanged (ImageView::IMAGE_SET); //= Force rebuilding the treeview to get the object name right.
}

/** 
 * @brief Set the ImageView object.
 * 
 * @param iv The ImageView object to be used with this ImageControl.
 */
void goGUI::ImageControl::setImageView (ImageView* iv)
{
    if (myPrivate->imageView)
    {
        myPrivate->imageView->changedCaller().disconnect (goMemberFunction<void, ImageControl, int> (this, &ImageControl::imageViewChanged));
    }

    myPrivate->imageView = iv;

    if (myPrivate->imageView)
    {
        myPrivate->imageView->changedCaller().connect (goMemberFunction<void, ImageControl, int> (this, &ImageControl::imageViewChanged));
    }
}

/** 
 * @brief Slot that gets called whenever the image view changes.
 * 
 * If the current image has changed, the caller returned by getImageChangedCaller() gets called
 * with the new current image as argument.
 * In the other cases, the treeview showing the images gets rebuilt.
 *
 * @param code One of ImageView::CURRENT_IMAGE_CHANGED, ImageView::IMAGE_SET, ImageView::IMAGE_REMOVED.
 */
void goGUI::ImageControl::imageViewChanged (int code)
{
    if (code == ImageView::CURRENT_IMAGE_CHANGED)
    {
        goAutoPtr<goSignal3DBase<void> > img = myPrivate->imageView->getImage (myPrivate->imageView->currentImageIndex());
        myPrivate->myImageChangedCaller (img);
        return;
    }

    if (code == ImageView::IMAGE_SET || code == ImageView::IMAGE_REMOVED)
    {
        myPrivate->myRefStore->clear ();

        if (!myPrivate->imageView)
            return;

        int sz = myPrivate->imageView->getImageCount ();
        for (int i = 0; i < sz; ++i)
        {
            goAutoPtr<goSignal3DBase<void> > img = myPrivate->imageView->getImage (i);

            if (!img.isNull())
            {
                goString s = "";
                s += (int)i;
                s += " "; s += img->getObjectName();
                myPrivate->imageList.append (Glib::ustring (s.toCharPtr ()));
                
                Gtk::TreeModel::iterator iter = myPrivate->myRefStore->append();
                Gtk::TreeModel::Row row = *iter;
                row[myPrivate->myColumns.myColNumber] = i;
                row[myPrivate->myColumns.myColName] = img->getObjectName().toCharPtr ();
                // Gtk::TreeModel::iterator iter_child = myPrivate->myRefTreeStore->append (row.children());
                // (*iter_child)[myPrivate->myColumns.myColNumber] = i;
                // (*iter_child)[myPrivate->myColumns.myColName] = "A child!";
            }
        }
    }
}

//========================================
//=======================================

void goGUI::ImageControl::treeViewDragEnd (const Glib::RefPtr<Gdk::DragContext>& context)
{
    TreeModelEnumerate enumerate (myPrivate->myRefStore, myPrivate);

    if (myPrivate->imageView)
    {
        myPrivate->imageView->reorderImages (enumerate.myIndex);
    }
    
    TreeModelRenumber renumber (myPrivate->myRefStore, myPrivate);
}

void goGUI::ImageControl::treeRowActivated (const Gtk::TreeModel::Path& path, Gtk::TreeViewColumn* col)
{
    if (path.size() < 1)
        return;

    //= First entry should mark the selected treemodel entry
    int entry = path[0];
    Gtk::TreePath tp;
    tp.push_back (entry);
    Gtk::TreeModel::iterator iter = myPrivate->myRefStore->get_iter (tp);
    //= Get the image index
    goIndex_t index = (*iter)[myPrivate->myColumns.myColNumber];
    
    if (myPrivate->imageView)
    {
        myPrivate->imageView->setCurrentImage (index);
        myPrivate->imageView->queue_draw ();
    }
}

/** 
 * @brief This goCaller is called whenever the current image changes.
 * 
 * The parameter of the caller is a pointer to the current image.
 *
 * @return The respective goCaller.
 */
goCaller1 <void, goAutoPtr<goSignal3DBase<void> > >& 
goGUI::ImageControl::getImageChangedCaller ()
{
    return myPrivate->myImageChangedCaller;
}

//void goGUI::ImageControl::selectionChanged ()
//{
//    Gtk::ListViewText::SelectionList l = myPrivate->imageList.get_selected ();
//    if (myPrivate->imageView && l.size() > 0)
//    {
//        myPrivate->imageView->setCurrentImage (l[0]);
//        myPrivate->imageView->queue_draw ();
//    }
//}
