#include <gogui/mainwindow.h>
#include <gogui/helper.h>
#include <assert.h>

#include <gostring.h>
#include <golist.h>
#include "logo.xpm"

namespace goGUI
{

    class MainWindowPrivate
    {
        public:
            MainWindowPrivate () 
                : controls ()
            {
            };

            ~MainWindowPrivate () {};

            goList<void*> controls;
    };

MainWindow::MainWindow ()
    : Gtk::Window (), 
      myMenuBar (),
      myControlBox (),
      myPaned (),
      myFileMenu (0),
      myControlsMenu (0),
      myAboutText ("<b>Golib GUI</b>\n\n<i>by Christian Gosch\n</i>\n\nUsing Golib 0.5 and Gtkmm"),
      myPrivate (0)
{
    myPrivate = new MainWindowPrivate;

    Gtk::Tooltips* toolTips = Gtk::manage (new Gtk::Tooltips);
    toolTips->enable ();

    Gtk::VBox* vbox = Gtk::manage (new Gtk::VBox);
    assert (vbox);
    Gtk::MenuItem* menuItem = 0;

    myFileMenu = this->addMenu ("File");
    myControlsMenu = this->addMenu ("Controls");

    // myFileMenu->append (*Gtk::manage(new Gtk::SeparatorMenuItem));

    //= Align the menu to the top and add it.
    {
        Gtk::Alignment* alignment = Gtk::manage (new Gtk::Alignment(Gtk::ALIGN_LEFT, Gtk::ALIGN_TOP, 1.0, 0.0));
        alignment->add (myMenuBar);
        vbox->pack_start (*alignment, Gtk::PACK_SHRINK);
    }

    //= Add control box widget
    {
        // myPaned.pack1 (*vbox);
        Gtk::ScrolledWindow* scrollWindow = Gtk::manage (new Gtk::ScrolledWindow);
        scrollWindow->add (myControlBox);
        myPaned.pack2 (*scrollWindow);
    }

    {
        Gtk::Alignment* alignment = Gtk::manage (new Gtk::Alignment(Gtk::ALIGN_LEFT, Gtk::ALIGN_TOP, 1.0, 1.0));
        alignment->add (myPaned);
        vbox->pack_start (*alignment);
    }

    this->add (*vbox);
    this->show_all_children ();

    this->set_default_size (580, 380);
    myPaned.set_position (80);
}

MainWindow::~MainWindow ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void MainWindow::addControl (goGUI::Control& c, bool active)
{
    myControlBox.pack_start (c, Gtk::PACK_SHRINK);
    myPrivate->controls.append (&c);
    
    Gtk::CheckMenuItem* item = Gtk::manage (new Gtk::CheckMenuItem (c.get_label()));
    item->set_active (active);
    this->getControlsMenu()->append (*item);
#ifdef HAVE_GTK_2_4
    item->signal_toggled().connect (sigc::bind<goGUI::Control*, Gtk::CheckMenuItem*> (sigc::mem_fun (this, &MainWindow::controlsToggled), &c, item));
#elif HAVE_GTK_2
    item->signal_toggled().connect (SigC::bind<goGUI::Control*, Gtk::CheckMenuItem*> (SigC::slot (this, &MainWindow::controlsToggled), &c, item));
#endif

    // Add standard connections if any come up.
#ifdef HAVE_GTK_2
    //c.signal_().connect (SigC::slot (*this, &MainWindow::fileAbout));
#elif defined HAVE_GTK_2_4
    //menuItem->signal_activate().connect (sigc::mem_fun (*this, &MainWindow::fileAbout));
#endif
}

void MainWindow::addFileAbout (const char* aboutText)
{
    if (aboutText)
    {
        myAboutText = aboutText;
    }
    //= File menu
    Gtk::MenuItem* menuItem = this->addMenuItem (myFileMenu, "About");
#ifdef HAVE_GTK_2
    menuItem->signal_activate().connect (SigC::slot (*this, &MainWindow::fileAbout));
#elif defined HAVE_GTK_2_4
    menuItem->signal_activate().connect (sigc::mem_fun (*this, &MainWindow::fileAbout));
#endif
}

void MainWindow::addFileQuit ()
{
    //= File menu
    Gtk::MenuItem* menuItem = this->addMenuItem (myFileMenu, "Quit");
#ifdef HAVE_GTK_2
    menuItem->signal_activate().connect (SigC::slot (*this, &MainWindow::fileQuit));
#elif defined HAVE_GTK_2_4
    menuItem->signal_activate().connect (sigc::mem_fun (*this, &MainWindow::fileQuit));
#endif
}

Gtk::Menu* MainWindow::addMenu (const char* label)
{
    Gtk::Menu* menu = Gtk::manage (new Gtk::Menu);
    menu->set_title (label);
    Gtk::MenuItem* subMenuItem = Gtk::manage (new Gtk::MenuItem(label));
    subMenuItem->set_submenu (*menu);
    myMenuBar.append (*subMenuItem);

    return menu;
}

Gtk::MenuItem* MainWindow::addMenuItem (Gtk::Menu* menu, const char* label)
{
    Gtk::MenuItem* menuItem = Gtk::manage (new Gtk::MenuItem (label));
    menu->append (*menuItem);

    return menuItem;
}

void MainWindow::setAboutText (const char* t)
{
    myAboutText = t;
}

void MainWindow::fileAbout ()
{
    goGUI::about (myAboutText, logo_xpm);
}

void MainWindow::fileQuit ()
{
    Gtk::Main::quit ();
}

void MainWindow::controlsToggled (goGUI::Control* control, Gtk::CheckMenuItem* item)
{
    if (!control)
    {
        return;
    }
    assert (myPrivate->controls.getSize() == this->getControlsMenu()->items().size());
    if (!item)
    {
        //= This case should only be used when the corresponding check items
        //= are held in sync otherwise. This should be changed when
        //= there is need to hide/show the controls from other instances than
        //= the user himself.
        if (control->is_visible ())
        {
            control->hide ();
        }
        else
        {
            control->show ();
        }
    }
    else
    {
        if (item->get_active())
        {
            control->show ();
        }
        else
        {
            control->hide ();
        }
    }
}

/** 
 * @brief Get Gtk::HPaned that is supposed to be the main widget.
 *
 * The control box is added using pack2(), so you could add more widgets using
 * pack1() to get them into the left region of the window.
 * 
 * @return 
 */
Gtk::HPaned& MainWindow::getPaned ()
{
    return myPaned;
}

Gtk::MenuBar& MainWindow::getMenuBar ()
{
    return myMenuBar;
}

Gtk::Menu* MainWindow::getFileMenu ()
{
    return myFileMenu;
}

Gtk::Menu* MainWindow::getControlsMenu ()
{
    return myControlsMenu;
}

};
