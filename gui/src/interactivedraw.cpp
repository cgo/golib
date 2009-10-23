#include <gogui/interactivedraw.h>
#include <gogui/interactivedrawobject.h>
#include <goplot/object2dbox.h>
#include <iostream>

namespace goGUI
{
    class InteractiveDrawPrivate
    {
        public:
            typedef std::list<goAutoPtr<InteractiveDrawObject> >  DrawObjectList;

        public:
            InteractiveDrawPrivate ()
                : buttonPressConnection (),
                  buttonReleaseConnection (),
                  motionConnection (),
                  mode (InteractiveDraw::NONE),
                  drawWidget (0),
                  graph (0),
                  drawObjects ()
            {
            }

            bool buttonPressed (GdkEventButton* event)
            {
                std::cout << "Button pressed\n";
                std::cout << event->x << ", " << event->y << "\n";
                mode = InteractiveDraw::EDIT;

                if (!this->graph)
                {
                    goLog::warning ("InteractiveDrawPrivate::buttonPressed(): no graph set");
                    return true;
                }

                goAutoPtr<goPlot::Object2DBox> box = new goPlot::Object2DBox;
                this->graph->add (box);
                goAutoPtr<InteractiveDrawObjectT<goPlot::Object2DBox> > drawObject (box);
                this->drawObjects.push_back (drawObject);
                box->setCorners (event->x, event->y, event->x, event->y);
                this->activeDrawObject = *this->drawObjects.rbegin ();

                this->motionConnect ();
                return true;
            }

            bool buttonReleased (GdkEventButton* event)
            {
                std::cout << "Button released\n";
                std::cout << event->x << ", " << event->y << "\n";
                mode = InteractiveDraw::NONE;
                this->motionDisconnect ();
                return true;
            }

            bool motionEvent (GdkEventMotion* event)
            {
                if (mode == InteractiveDraw::MOVE || mode == InteractiveDraw::EDIT)
                {
                    std::cout << event->x << ", " << event->y << "\n";
                }

                switch (mode)
                {
                    case InteractiveDraw::EDIT:
                        {
                            goAutoPtr<InteractiveDrawObjectT<goPlot::Object2DBox> > o = dynamic_cast <InteractiveDrawObjectT<goPlot::Object2DBox>* > (this->activeDrawObject.get())->getObject ();

                            if (!o)
                                return true;

                            goMatrixd c = o->getObject()->getCorners ();
                            c (1, 0) = (float)event->x / (float)this->drawWidget->get_width();
                            c (1, 1) = (float)event->y / (float)this->drawWidget->get_height();
                            o->getObject()->setCorners (c);

                            this->drawWidget->queue_draw ();
                        }
                        break;

                    default: break;
                }

                return true;
            }

            void motionConnect ()
            {
                this->motionDisconnect ();
                if (this->drawWidget)
                {
                    motionConnection = this->drawWidget->signal_motion_notify_event().connect (sigc::mem_fun (*this, &InteractiveDrawPrivate::motionEvent));
                }
            }

            void motionDisconnect ()
            {
                if (motionConnection.connected ())
                {
                    motionConnection.disconnect ();
                }
            }

            sigc::connection buttonPressConnection;
            sigc::connection buttonReleaseConnection;
            sigc::connection motionConnection;

            int mode;

            Gtk::Widget* drawWidget;          //= The widget used to get pointer events

            goAutoPtr<goPlot::Graph> graph;   //= The graph we are drawing with

            InteractiveDrawPrivate::DrawObjectList  drawObjects;       //= List of my objects
            goAutoPtr<InteractiveDrawObject>        activeDrawObject;  //= The one being edited
    };
};


goGUI::InteractiveDraw::InteractiveDraw ()
    : myPrivate (0)
{
    myPrivate = new InteractiveDrawPrivate;
}

goGUI::InteractiveDraw::~InteractiveDraw ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void goGUI::InteractiveDraw::setDrawWidget (Gtk::Widget* widget)
{
    if (myPrivate->buttonPressConnection.connected())
    {
        myPrivate->buttonPressConnection.disconnect ();
    }

    if (myPrivate->buttonReleaseConnection.connected())
    {
        myPrivate->buttonReleaseConnection.disconnect ();
    }

    myPrivate->motionDisconnect ();

    myPrivate->drawWidget = widget;

    if (!widget)
    {
        return;
    }

    myPrivate->buttonPressConnection = widget->signal_button_press_event().connect (sigc::mem_fun (*myPrivate, &InteractiveDrawPrivate::buttonPressed));
    myPrivate->buttonReleaseConnection = widget->signal_button_release_event().connect (sigc::mem_fun (*myPrivate, &InteractiveDrawPrivate::buttonReleased));
}

void goGUI::InteractiveDraw::setGraph (goAutoPtr<goPlot::Graph> g)
{
    myPrivate->graph = g;
}
