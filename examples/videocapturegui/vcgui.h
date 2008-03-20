#ifndef VCGUI_H
#define VCGUI_H

#include <gogui/mainwindow.h>

class VCGuiPrivate;

class VCGui : public goGUI::MainWindow
{
    public:
        VCGui ();
        virtual ~VCGui ();

        int redrawImage ();

    private:
        VCGuiPrivate* myPrivate;
};

#endif
