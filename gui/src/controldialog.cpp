#include <gogui/controldialog.h>

goGUI::ControlDialog::ControlDialog (Control& ctrl)
    : Gtk::Dialog ()
//      myCancelButton ("Cancel"),
//      myCloseButton ("Close"),
//      myCancelled (false)
{
    this->set_title (ctrl.get_label ());
    //Gtk::Table* table = Gtk::manage (new Gtk::Table);
    //table->attach (ctrl, 0, 2, 0, 1, Gtk::SHRINK, Gtk::SHRINK);
    //table->attach (myCancelButton, 0, 1, 1, 2, Gtk::FILL, Gtk::FILL);
    //table->attach (myCloseButton, 1, 2, 1, 2, Gtk::FILL, Gtk::FILL);

    this->get_vbox()->pack_start (ctrl, Gtk::PACK_SHRINK);

    ctrl.show ();
    ctrl.show_all_children ();

    this->add_button ("Cancel", Gtk::RESPONSE_CANCEL);
    this->add_button ("Close", Gtk::RESPONSE_CLOSE);
    // myCancelButton.signal_clicked().connect (sigc::mem_fun (*this, &ControlDialog::cancel));
    // myCloseButton.signal_clicked().connect (sigc::mem_fun (*this, &ControlDialog::close));
    // this->add (*table);

    this->set_modal (true);
}

goGUI::ControlDialog::~ControlDialog ()
{
}

#if 0
bool goGUI::ControlDialog::run ()
{
    myCancelled = false;
    this->show_all_children ();
    return myCancelled;
}

void goGUI::ControlDialog::cancel ()
{
    myCancelled = true;
    this->hide ();
}

void goGUI::ControlDialog::close ()
{
    myCancelled = false;
    this->hide ();
}
#endif
