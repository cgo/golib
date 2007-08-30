#include <gogui/mainwindow.h>
#include <gogui/draw.h>
#include <gtkmm.h>
#include <gothreadobject.h>
#include <gofileio.h>
#include <gosignalhelper.h>

class MyWindow : public goGUI::MainWindow
{
    public:
        MyWindow () : goGUI::MainWindow ()
        {
            Gtk::Menu* menu = this->getFileMenu();
            Gtk::MenuItem* item = this->addMenuItem (menu, "Draw test");
            item->signal_activate().connect (sigc::mem_fun (*this, &MyWindow::fileAbout));
            Gtk::Curve* da = Gtk::manage (new Gtk::Curve);
            this->getPaned().pack1 (*da, Gtk::EXPAND);
            this->show_all_children ();
            da->reset ();
        };

        virtual ~MyWindow ()
        {
        };

        virtual void fileAbout ()
        {
            goGUI::Draw draw (this->getPaned().get_child1()->get_window());
            draw.line (0.0, 0.0, 1.0, 1.0);
            draw.line (0.0, 1.0, 1.0, 0.0);

            goSignal3D<void> image;
            image.setDataType (GO_UINT8);
            goFileIO::readImage ("/home/gosch/Documents/images/Leeuw.JPG", &image, true);
            printf ("size: %d %d %d\n", image.getSizeX(), image.getSizeY(), image.getSizeZ());
            printf ("blocksize: %d %d %d\n", image.getBlockSizeX(), image.getBlockSizeY(), image.getBlockSizeZ());

            goSignal3D<void> image2;
            image2.setDataType (image.getDataType().getID());
            image2.make (image.getSize(), image.getBlockSize(), image.getBorderSize(), 3);
            goCopySignal (&image, &image2);

            draw.image (image2);
        };

};

template <class MW>
class MainWindowThread : public goThreadObject
{
    public:
        MainWindowThread () : goThreadObject(), kit(0), window(0)
        {
        };

        virtual void threadMethod ()
        {
            int argc = 1;
            char **argv = new char*[1];
            goString name = "MainWindowThread";
            argv[0] = new char [name.getSize() + 1];
            strcpy (argv[0], name.toCharPtr());
            argv[0][name.getSize()] = 0;
            Gtk::Main kit(argc, argv);
            window = new MW;
            // Gtk::Main::run (*this->window);
            kit.run (*window);
        };

        virtual ~MainWindowThread ()
        {
            if (this->kit)
            {
                delete this->kit;
                this->kit = 0;
            }
            if (this->window)
            {
                delete this->window;
                this->window = 0;
            }
        };

        Gtk::Main* kit;
        MW* window;
};

int main (int argc, char* argv[])
{
    //MainWindowThread<MyWindow> mwt;
    //mwt.run();
    //::sleep(3);
    //mwt.getThread().join();

    Gtk::Main kit(argc, argv);
    MyWindow window;
    // kit.run (window);
    Gtk::Main::run(window);
}
