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

            Gtk::TreeModelColumn <goIndex_t>     myColNumber;
            Gtk::TreeModelColumn <Glib::ustring> myColName;
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
                  myRefTreeStore (),
                  myTreeView (),
                  myImageChangedCaller ()
            {
                myRefTreeStore = Gtk::TreeStore::create (myColumns);
                myTreeView.set_model (myRefTreeStore);
                myTreeView.append_column ("Number", myColumns.myColNumber);
                myTreeView.append_column ("Name", myColumns.myColName);
                myTreeView.set_headers_visible (false);
                myTreeView.set_reorderable (true);
                myTreeView.set_grid_lines (Gtk::TREE_VIEW_GRID_LINES_HORIZONTAL);
                // myTreeView.set_hover_selection (true);
            }

            Gtk::ListViewText imageList;
            goGUI::ImageView* imageView;

            ImageModelColumns               myColumns;
            Glib::RefPtr<Gtk::TreeStore>    myRefTreeStore;
            Gtk::TreeView                   myTreeView;
            goCaller1 <void, goAutoPtr<goSignal3DBase<void> > > myImageChangedCaller;
    };


};

goGUI::ImageControl::ImageControl ()
    : Control ("Image View"),
      myPrivate (0)
{
    myPrivate = new ImageControlPrivate;
    // myPrivate->imageList.signal_columns_changed ().connect (sigc::mem_fun (*this, &ImageControl::selectionChanged));

    myPrivate->myTreeView.signal_row_activated ().connect (sigc::mem_fun (*this, &ImageControl::treeRowActivated));

    this->add (myPrivate->myTreeView);
}

goGUI::ImageControl::~ImageControl ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void goGUI::ImageControl::loadImage ()
{
    if (!myPrivate->imageView)
    {
        this->warning ("Image control: no ImageView set.\nCan not load images.");
        return;
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
            
            this->addImage (image);
            // myPrivate->imageVview->setImage (*image, 0);
            // this->imageView->setCurrentImage (0);
            myPrivate->imageView->queue_draw ();
            // this->control.setImage (image);
        }
        catch (goFileIOException& ex)
        {
            Gtk::MessageDialog dlg ("Reading image failed.");
            dlg.run ();
            return;
        }
    }
}

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
 * @param code One og ImageView::CURRENT_IMAGE_CHANGED, ImageView::IMAGE_SET, ImageView::IMAGE_REMOVED.
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
        myPrivate->myRefTreeStore->clear ();

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
                myPrivate->imageList.append_text (Glib::ustring (s.toCharPtr ()));
                
                Gtk::TreeModel::iterator iter = myPrivate->myRefTreeStore->append();
                Gtk::TreeModel::Row row = *iter;
                row[myPrivate->myColumns.myColNumber] = i;
                row[myPrivate->myColumns.myColName] = img->getObjectName().toCharPtr ();
                Gtk::TreeModel::iterator iter_child = myPrivate->myRefTreeStore->append (row.children());
                (*iter_child)[myPrivate->myColumns.myColNumber] = i;
                (*iter_child)[myPrivate->myColumns.myColName] = "A child!";
            }
        }
    }
}

void goGUI::ImageControl::treeRowActivated (const Gtk::TreeModel::Path& path, Gtk::TreeViewColumn* col)
{
    if (path.size() < 1)
        return;

    //= First entry should mark the selected treemodel entry
    int entry = path[0];
    Gtk::TreePath tp;
    tp.append_index (entry);
    Gtk::TreeModel::iterator iter = myPrivate->myRefTreeStore->get_iter (tp);
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
