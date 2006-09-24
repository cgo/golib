#include <gogui/helper.h>
#include <gtkmm.h>

bool goGUI::getFilenameOpen (goString& fname, const goString& start, const goString& title)
{
#ifdef HAVE_GTK_2
    Gtk::FileSelection dialog;
    dialog.set_title (title.toCharPtr());
    dialog.set_filename (start.toCharPtr());
    if (dialog.run () != Gtk::RESPONSE_OK)
    {
        return false;
    }
    std::string filename_ = dialog.get_filename ();
    fname = filename_.data();
#elif defined HAVE_GTK_2_4
    Gtk::FileChooserDialog dialog (title.toCharPtr(), Gtk::FILE_CHOOSER_ACTION_OPEN);
    dialog.add_button ("Ok", Gtk::RESPONSE_OK);
    dialog.add_button ("Cancel", Gtk::RESPONSE_CANCEL);
    dialog.set_current_folder (start.toCharPtr());
    if (dialog.run () != Gtk::RESPONSE_OK)
    {
        return false;
    }
    std::string filename_ = dialog.get_filename ();
    fname = filename_.data();
#endif
    return true;
}

bool goGUI::getFoldername (goString& fname, const goString& start, const goString& title)
{
#ifdef HAVE_GTK_2
    Gtk::FileSelection dialog;
    dialog.set_title (title.toCharPtr());
    dialog.set_filename (start.toCharPtr());
    if (dialog.run () != Gtk::RESPONSE_OK)
    {
        return false;
    }
    std::string filename_ = dialog.get_filename ();
    fname = filename_.data();
#elif defined HAVE_GTK_2_4
    Gtk::FileChooserDialog dialog (title.toCharPtr(), Gtk::FILE_CHOOSER_ACTION_SELECT_FOLDER);
    dialog.add_button ("Ok", Gtk::RESPONSE_OK);
    dialog.add_button ("Cancel", Gtk::RESPONSE_CANCEL);
    dialog.set_current_folder (start.toCharPtr());
    if (dialog.run () != Gtk::RESPONSE_OK)
    {
        return false;
    }
    std::string filename_ = dialog.get_filename ();
    fname = filename_.data();
#endif
    return true;
}

bool goGUI::getFilenameSave (goString& fname, const goString& start, const goString& title)
{
#ifdef HAVE_GTK_2
    Gtk::FileSelection dialog;
    dialog.set_title (title.toCharPtr());
    dialog.set_filename (start.toCharPtr());
    if (dialog.run () != Gtk::RESPONSE_OK)
    {
        return false;
    }
    std::string filename_ = dialog.get_filename ();
    fname = filename_.data();
#elif defined HAVE_GTK_2_4
    Gtk::FileChooserDialog dialog (title.toCharPtr(), Gtk::FILE_CHOOSER_ACTION_SAVE);
    dialog.add_button ("Ok", Gtk::RESPONSE_OK);
    dialog.add_button ("Cancel", Gtk::RESPONSE_CANCEL);
    dialog.set_current_folder (start.toCharPtr());
    if (dialog.run () != Gtk::RESPONSE_OK)
    {
        return false;
    }
    std::string filename_ = dialog.get_filename ();
    fname = filename_.data();
#endif
    return true;
}

bool goGUI::getFilenames (goFixedArray<goString>& filenames, const goString& start, const goString& title)
{
#ifdef HAVE_GTK_2
    Gtk::FileSelection dialog;
    dialog.set_title (title.toCharPtr());
    dialog.set_filename (start.toCharPtr());
    dialog.set_select_multiple (true);
    if (dialog.run () != Gtk::RESPONSE_OK)
    {
        return false;
    }

    Glib::ArrayHandle<std::string> sel = dialog.get_selections ();
    if (sel.size() == 0)
    {
        // Glib::strfreev (sel);
        // strfreev (sel);
        return false;
    }
    filenames.setSize (sel.size());
    Glib::ArrayHandle<std::string>::iterator it = sel.begin();
    goSize_t i = 0;
    goSize_t sz = sel.size();
    for (i = 0; i < sz; ++i)
    {
        filenames[i] = (*it).data();
        ++it;
    }
    // Glib::strfreev (sel);
    // strfreev (sel);
#elif defined HAVE_GTK_2_4
    Gtk::FileChooserDialog dialog (title.toCharPtr(), Gtk::FILE_CHOOSER_ACTION_OPEN);
    dialog.add_button ("Ok", Gtk::RESPONSE_OK);
    dialog.add_button ("Cancel", Gtk::RESPONSE_CANCEL);
    dialog.set_current_folder (start.toCharPtr());
    dialog.set_select_multiple (true);
    if (dialog.run () != Gtk::RESPONSE_OK)
    {
        return false;
    }
    Glib::SListHandle<Glib::ustring> fileList = dialog.get_filenames ();
    if (fileList.size() == 0)
    {
        //= FIXME in the documentation it says "delete handle with Glib::slist_free() ... but
        //= it doesn't seem to exist.
        // Glib::slist_free (fileList);
        // Glib::free (fileList);
        return false;
    }
    goSize_t sz = fileList.size ();
    Glib::SListHandle<Glib::ustring>::iterator it = fileList.begin ();
    filenames.setSize (sz);
    goSize_t i;
    for (i = 0; i < sz; ++i)
    {
        filenames[i] = Glib::locale_from_utf8(*it).data ();
        //= FIXME see fixme above
        // Glib::free (*it);
        ++it;
    }
#endif
    return true;
}
