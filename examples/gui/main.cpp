/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogui/mainwindow.h>
#include <gogui/draw.h>
#include <gtkmm.h>
#include <gothreadobject.h>
#include <gofileio.h>
#include <gosignalhelper.h>
#include <gosignal.h>
#include <gomatrix.h>

#include <goplot.h>
#include <gogui/plotview.h>
#include <goplot/graph.h>
#include <goplot/layout.h>

class MyWindow : public goGUI::MainWindow
{
    public:
        typedef goPlot::Object2DPoints PointsObject;
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
            // graph2->setTransform (goPlot::Trafo2D<goPlot::real> (0.1, 0.0, 0.0, 0.1, 0.1, 0.1));
            goVectorf y (100);
            goVectorf x (100);
            for (int i = 0; i < y.getSize(); ++i)
            {
                x[i] = 2.0 * M_PI * float(i) / float(y.getSize() - 1);
                y[i] = ::sin (x[i]);
            }
            goAutoPtr<goPlot::Object2DPoints> obj = goPlot::object2D (x,y);
            obj->lineTraits ().setColour (goPlot::RGBA (1.0, 0.0, 0.0, 1.0));
            obj->lineTraits ().setWidth (2.0);

            //goPlot::Layout* layout = new goPlot::Layout;
            //layout->setSize (2,2);
            //graph2.getRRefPtr()->incRef ();
            //graph.getRRefPtr()->incRef ();
            //layout->setGraph (&*graph2, 0, 0);
            //layout->setGraph (&*graph, 1, 1, 1, 1);

            //goAutoPtr<goPlot::Graph> lg = new goPlot::Graph;
            //lg->add (layout);

            graph2->add (obj);
            graph2->axis (1)->setLower (goMath::min(y));
            graph2->axis (1)->setUpper (goMath::max(y));
            graph2->axis (0)->setLower (goMath::min(x));
            graph2->axis (0)->setUpper (goMath::max(x));
            graph2->axis (0)->setTics (20);
            graph2->axis (1)->setTics (20);
            graph2->axis (0)->enableTics (true);
            graph2->axis (1)->enableTics (true);
            graph2->axis (0)->enableTicsText (false);
            graph2->axis (1)->enableTicsText (false);
            graph2->axis (2)->setVisible (true);
            graph2->axis (3)->setVisible (true);

            //layout->updateLayout ();
            pv->setGraph (graph2);
            //pv->setGraph (lg);

            {
                goSignal3D<void> i;
                // goFileIO::readImage ("zebrabw.pgm", &i, true);
                try
                {
                    goFileIO::readImage ("image.png", &i, true);
                    goString str;
                    goSignalInfoText (i, str);
                    printf ("%s\n", str.toCharPtr ());
                    goAutoPtr<goPlot::Object2DImage> obj = goPlot::object2DImage (*goSignal::toBGRA (i, 128));
                    // goPlot::AutoPtr<goPlot::Object2DImage> obj = goPlot::object2DImage (i);
                    goPlot::Trafo2D t (2.0 * M_PI / float(i.getSizeX()), 0, 0, -2.0 / float(i.getSizeY()), 0, 1);
                    obj->setTransform (t);
                    graph2->add (obj);
                }
                catch (goException& ex)
                {
                    printf ("Could not load the test image, not displaying an image.\n");
                }
            }
            
            this->show_all_children ();
        }

        virtual ~MyWindow ()
        {
        }

        virtual void fileAbout () override
        {
        	std::cout << "Hello!" << std::endl;

        	goGUI::Draw draw (this->getPaned().get_child2()->get_window());
            draw.line (0.0, 0.0, 1.0, 1.0);
            draw.line (0.0, 1.0, 1.0, 0.0);

            goSignal3D<void> image;
            image.setDataType (GO_UINT8);
            try
            {
            	goFileIO::readImage ("/Users/christian/Pictures/Pictures_other/cannondale_trigger.png", &image, true);
            	printf ("size: %d %d %d\n", image.getSizeX(), image.getSizeY(), image.getSizeZ());
            	printf ("blocksize: %d %d %d\n", image.getBlockSizeX(), image.getBlockSizeY(), image.getBlockSizeZ());

            	goSignal3D<void> image2;
            	image2.setDataType (image.getDataType().getID());
            	image2.make (image.getSize(), image.getBlockSize(), image.getBorderSize(), 3);
            	goCopySignal (&image, &image2);

            	draw.image (image2);
            }
            catch (goException& ex)
            {
            	std::cout << "Failed to load image file." << std::endl;
            }
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

	auto app =
	    Gtk::Application::create(argc, argv,
	      "de.goschs.examples", Gio::APPLICATION_HANDLES_OPEN);

    MyWindow window;
	//  Gtk::Window window;
	  window.set_default_size(200, 200);

	  return app->run(window);
}
