/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goobjectmessage.h>

/**
 * @brief Constructor
 **/
goObjectMessage::goObjectMessage ()
    :
    mySender        (0),
    myMessageID     (GO_OBJECTMESSAGE_NONE),
    myMessageString (0),
    myData          (0)
{
}

/**
 * @brief Destructor
 **/
goObjectMessage::~goObjectMessage ()
{
    if (myMessageString)
    {
        delete[] myMessageString;
    }
}
