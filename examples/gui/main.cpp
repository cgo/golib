#include <gogui/mainwindow.h>
#include <gtkmm.h>

int main (int argc, char* argv[])
{
    Gtk::Main kit(argc, argv);
    goGUI::MainWindow window;
    Gtk::Main::run(window);
}
