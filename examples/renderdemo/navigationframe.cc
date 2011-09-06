/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <navigationframe.h>

NavigationFrame::NavigationFrame (QWidget* parent = 0, 
				  const char* name = 0, 
				  WFlags fl = 0 )
    : QFrame (parent, name, fl)
{
    moving = false;
    rotating = false;
    lastMoving = new QPoint;
    lastRotating = new QPoint;
}

NavigationFrame::~NavigationFrame ()
{
}

void
NavigationFrame::mousePressEvent (QMouseEvent *e)
{
    switch (e->button())
	{
	case LeftButton: moving = true; *lastMoving = e->pos(); break;
	case RightButton: rotating = true; *lastRotating = e->pos(); break;
	default: break;
	}
}

void
NavigationFrame::mouseReleaseEvent (QMouseEvent *e)
{
    switch (e->button())
	{
	case LeftButton: moving = false; break;
	case RightButton: rotating = false; break;
	default: break;
	}
}

void
NavigationFrame::mouseMoveEvent (QMouseEvent *e)
{
    if (moving)
	{
	    int dx = e->x() - lastMoving->x();
	    int dy = e->y() - lastMoving->y();
	    *lastMoving = e->pos();
	    emit moveSignal (dx,dy);
	}
    if (rotating)
	{
	    int dx = e->x() - lastMoving->x();
	    int dy = e->y() - lastMoving->y();
	    *lastRotating = e->pos();
	    emit rotateSignal (dx,dy);
	}
}
