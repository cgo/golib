#include <gogui/mainwindow.h>
#include <gogui/textoutput.h>
#include <gtkmm.h>
#include <stdio.h>
#include <fcntl.h>

int main (int argc, char* argv[])
{
    Gtk::Main mainloop (argc, argv);

    goGUI::MainWindow mainWindow;
    goGUI::TextOutput to;
    mainWindow.getPaned().add1 (to);

    //= Now open a file / use stdout and redirect it to the TextOutput.
    int fd = ::open ("/var/log/messages", O_RDONLY);
    std::cout << "fd == " << fd << "\n";
    to.setFile (fd);


    std::cout << "About to call run.\n";
    Gtk::Main::run (mainWindow);

    ::close (fd);
}
