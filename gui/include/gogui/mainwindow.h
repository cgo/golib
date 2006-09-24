#ifndef GOGUI_MAINWINDOW_H
#define GOGUI_MAINWINDOW_H

#include <gtkmm.h>
#include <gogui/control.h>

namespace goGUI
{

class MainWindow : public Gtk::Window
{
    public:
        MainWindow ();
        virtual ~MainWindow ();

        void addControl (goGUI::Control& c);

        Gtk::Menu*     addMenu     (const char* label);
        Gtk::MenuItem* addMenuItem (Gtk::Menu*, const char* label);

        virtual void fileAbout ();
        virtual void fileQuit ();

        Gtk::HPaned&  getPaned ();
        Gtk::MenuBar& getMenuBar ();

    protected:
        Gtk::MenuBar     myMenuBar;
        Gtk::VBox        myControlBox;
        Gtk::HPaned      myPaned;
};
};

#endif
