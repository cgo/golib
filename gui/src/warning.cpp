#include <gogui/helper.h>
#include <gtkmm.h>

void goGUI::warning (const char* text)
{
    Gtk::Dialog msg;
    Gtk::Label label;
    label.set_markup (text);
    //Gtk::Image image (Gdk::Pixbuf::create_from_xpm_data (logo));
    Gtk::HBox hbox;
    //Gtk::VSeparator sep;

    hbox.set_border_width (4);
    hbox.set_spacing (4);
    //hbox.pack_start (image, Gtk::PACK_SHRINK);
    //hbox.pack_start (sep,   Gtk::PACK_SHRINK);
    hbox.pack_end   (label, Gtk::PACK_SHRINK);
    msg.get_vbox()->pack_start (hbox);
    //label.show ();
    //image.show ();
    //sep.show ();
    hbox.show ();

    Gdk::Color white;
    white.set_rgb_p (1.0, 1.0, 1.0);
    msg.modify_bg (Gtk::STATE_NORMAL, white);

    msg.add_button ("Ok", Gtk::RESPONSE_OK);

    msg.set_title ("Warning");
    msg.run ();
}
