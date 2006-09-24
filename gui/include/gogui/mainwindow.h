#ifndef GOGUI_MAINWINDOW_H
#define GOGUI_MAINWINDOW_H

#include <gtkmm.h>
#include <gogui/control.h>

namespace goGUI
{

    /** 
     * @brief Base class for a main application window.
     * Provides a menu bar, a standard File menu with about and quit items,
     * and a Gtk::HPaned, a horizontally divided widget, as main widget.
     * In the left area (Gtk: pack2()) a "controls box" is added,
     * where you can add goGUI::Control widgets using addControl().
     * The left area (Gtk: pack1()) is left blank and you can add stuff to it if you need it.
     * You can use something like getPaned().pack1(my_very_own_widget) to add you widget to the
     * left side of the HPaned.
     * To add new menus, you can do something like this:
     * @verbatim
        goGUI::MainWindow mw;
        ...
        Gtk::Menu* menu = mw.addMenu ("Help");
        Gtk::MenuItem* mi = mw.addMenuItem (menu, "Read documentation");
        mi->signal_activated().connect (sigc::mem_fun (my_widget_instance, &my_widget_class::my_void_callback));
        ...
       @endverbatim
     */
class MainWindow : public Gtk::Window
{
    public:
        MainWindow ();
        virtual ~MainWindow ();

        void addControl (goGUI::Control& c);

        Gtk::Menu*     addMenu     (const char* label);
        Gtk::MenuItem* addMenuItem (Gtk::Menu*, const char* label);

        void           addFileAbout (const char* aboutText);
        void           addFileQuit  ();

        void           setAboutText (const char*);

        virtual void fileAbout ();
        virtual void fileQuit ();

        Gtk::HPaned&  getPaned ();
        Gtk::MenuBar& getMenuBar ();

    protected:
        Gtk::MenuBar     myMenuBar;
        Gtk::VBox        myControlBox;
        Gtk::HPaned      myPaned;
        Gtk::Menu*       myFileMenu;
        goString         myAboutText;
};
};

#endif