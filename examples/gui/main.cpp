#include <gogui/mainwindow.h>
#include <gogui/draw.h>
#include <gtkmm.h>
#include <gothreadobject.h>
#include <gofileio.h>
#include <gosignalhelper.h>
#include <gomatrix.h>

#include <goplot.h>
#include <gogui/plotview.h>
#include <goplot/graph.h>

class MyWindow : public goGUI::MainWindow
{
    public:
        typedef goPlot::Object2DPoints<goPlot::Points2DMatrix<goFloat>, goFloat> PointsObject;
    public:
        MyWindow () : goGUI::MainWindow ()
        {
            Gtk::Menu* menu = this->getFileMenu();
            Gtk::MenuItem* item = this->addMenuItem (menu, "Draw test");
            item->signal_activate().connect (sigc::mem_fun (*this, &MyWindow::fileAbout));
            // Gtk::Curve* da = Gtk::manage (new Gtk::Curve);
            // this->getPaned().pack1 (*da, Gtk::EXPAND);
            goGUI::PlotView *pv = Gtk::manage (new goGUI::PlotView);
            this->getPaned().pack1 (*pv, Gtk::EXPAND);

            goAutoPtr<goPlot::Graph> graph = new goPlot::Graph;

            //PointsObject* po = new PointsObject;
            goMatrixf M;
            M.readASCII ("curve.txt");
            // po->points() = M;
            goVectorf v1,v2;
            M.refColumn (0, v1);
            M.refColumn (1, v2);
            graph->axis (0)->setLower (goMath::min(v1));
            graph->axis (0)->setUpper (goMath::max(v1));
            graph->axis (1)->setLower (goMath::min(v2));
            graph->axis (1)->setUpper (goMath::max(v2));
            graph->axis (0)->setTics (20);
            graph->axis (1)->setTics (20);

            //graph->add (goPlot::AutoPtr<PointsObject> (po));
            //graph->add (po);
            graph->add (goPlot::object2D (M));

            goAutoPtr<goPlot::Graph> graph2 = new goPlot::Graph;
            goVectorf y (100);
            goVectorf x (100);
            for (int i = 0; i < y.getSize(); ++i)
            {
                x[i] = 2.0 * M_PI * float(i) / float(y.getSize() - 1);
                y[i] = ::sin (x[i]);
            }
            goPlot::AutoPtr<goPlot::Object2DPoints<goPlot::Points2DMatrix<goFloat>, goFloat> > obj = goPlot::object2D (x,y);
            obj->lineTraits ().setColour (goPlot::RGBA (1.0, 0.0, 0.0, 1.0));

            graph2->add (obj);
            graph2->axis (1)->setLower (goMath::min(y));
            graph2->axis (1)->setUpper (goMath::max(y));
            graph2->axis (0)->setLower (goMath::min(x));
            graph2->axis (0)->setUpper (goMath::max(x));
            graph2->axis (0)->setTics (20);
            graph2->axis (1)->setTics (20);

            pv->setGraph (graph2);
            
            this->show_all_children ();
        }

        virtual ~MyWindow ()
        {
        }

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
        }

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
