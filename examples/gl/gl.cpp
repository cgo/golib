/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogl/offfile.h>
#include <gogui/glwidget.h>
#include <gogui/offview.h>
#include <gogui/mainwindow.h>
#include <gogui/helper.h>

#include <stdio.h>
#include <stdlib.h>

#include <gogl/gl.h>

// #include <gtk/gtkgl.h>

static int check_gl_error (const char* name)
{
    int n = 0;
    if ((n = glGetError()) != GL_NO_ERROR)
    {
        printf ("%s error\n", name);
    }
    return n;
}


class MyWindow : public goGUI::MainWindow
{
    public:
        MyWindow ()
            : goGUI::MainWindow (), glw ()
        {
            Gtk::Frame* F = Gtk::manage (new Gtk::Frame);
            this->getPaned().pack1 (*F);
            F->add (glw);
            F->show ();
            glw.show ();

            Gtk::MenuItem* item = this->addMenuItem (this->getFileMenu(),
                "Open");
            if (item)
            {
                item->signal_activate().connect (sigc::mem_fun (*this, &MyWindow::open));
            }

            this->show_all_children();

        };
        virtual ~MyWindow () {};

        void open ()
        {
            goString filename;
            static goString lastFilename;
            if (!goGUI::getFilenameOpen (filename, lastFilename, "Open"))
            {
                return;
            }
            filename.getPathName (lastFilename);
            this->glw.load (filename.toCharPtr());
        };

        goGUI::OFFView glw;
};

int main (int argc, char* argv[])
{
    if (argc < 2)
    {
        printf ("Tell me the .off filename.\n");
        exit (-1);
    }

    const char* filename = argv[1];
    goGL::OFFFile f;
    if (!f.read (filename))
    {
        printf ("There was an error.\n");
    }
    else
    {
        printf ("Everything seems to be ok.\n");
    }

    goMatrixf normals;
    f.calculateNormals (normals);

    goFixedArray<goList<int> > adj;
    f.getAdjacencyLists (adj);
    for (goSize_t i = 0; i < adj.getSize(); ++i)
    {
        printf ("%d -> ", i);
        goList<int>::Element* el = adj[i].getFrontElement();
        while (el)
        {
            printf ("%d ", el->elem);
            el = el->next;
        }
        printf ("\n");
    }

    Gtk::Main kit(argc, argv);
    //gdk_gl_init (&argc, &argv);
    //gtk_gl_init (&argc, &argv);
    MyWindow w;
    Gtk::Main::run (w);

    exit (1);
}
