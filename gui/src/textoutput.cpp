/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <golog.h>
#include <gogui/textoutput.h>
#include <iostream>

namespace goGUI
{
    class TextOutputPrivate
    {
        public:
            TextOutputPrivate ()
                : fd (-1),
                  ioConnection (),
                  ioChannel (0)
            {
            }

            int fd;  //= file descriptor
            sigc::connection ioConnection;
            Glib::RefPtr<Glib::IOChannel>  ioChannel;
    };
};

goGUI::TextOutput::TextOutput (int fd)
    : myPrivate (0)
{
    myPrivate = new TextOutputPrivate;

    this->setFile (fd);
}

goGUI::TextOutput::~TextOutput ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void goGUI::TextOutput::setFile (int fd)
{
    if (myPrivate->ioConnection.connected ())
    {
        myPrivate->ioConnection.disconnect ();
    }

    myPrivate->fd = -1;

    if (fd >= 0)
    {
        myPrivate->ioChannel = Glib::IOChannel::create_from_fd (fd);

        if (myPrivate->ioChannel)
            myPrivate->fd = fd;

        myPrivate->ioConnection = Glib::signal_io().connect (sigc::mem_fun (*this, &TextOutput::ioHandler), myPrivate->ioChannel, Glib::IO_IN | Glib::IO_HUP);
    }
}

bool goGUI::TextOutput::ioHandler (Glib::IOCondition cond)
{
    if (cond & Glib::IO_IN)
    {
        if (!myPrivate->ioChannel)
        {
            goLog::error ("goGUI::TextOutput::ioHandler(): ioChannel is null.");
            return true;
        }

        Glib::ustring s;

        if (myPrivate->ioChannel->read_to_end (s) != Glib::IO_STATUS_NORMAL)
        {
            goLog::error ("goGUI::TextOutput::ioHandler(): ioChannel->read_to_end resulted in error.");
        }

        std::cout << s;

        this->get_buffer()->insert (this->get_buffer()->end(), s);
        this->queue_draw ();
    }

    if (cond & Glib::IO_HUP)
    {
        return false;
    }

    return true; //= If we return false, the signal will be disconnected (see Glibmm documentation on SignalIO::connect()
}
