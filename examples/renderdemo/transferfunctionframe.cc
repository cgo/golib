/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <transferfunctionframe.h>
#include <qpainter.h>


TransferFunctionFrame::TransferFunctionFrame (QWidget * parent=0, const char * name=0, WFlags f=0, bool b = TRUE)
    : QFrame (parent, name, f, b)
{
    yMax = xMax = 1.0f;
    yMin = xMin = 0.0f;
    pointsX.resize (0);
    pointsY.resize (0);
    isDraggingPoint = false;
    closestPoint = new QPoint;
    lastClickedPoint = new QPoint;
    popupMenu = new QPopupMenu (this);
    popupMenu->insertItem ("Add point", this, SLOT(addPoint()));
    popupMenu->insertItem ("Remove point", this, SLOT(removePoint()));
    popupMenu->hide();
    lastClickedPoint->setX (0);
    lastClickedPoint->setY (height() - 1);
    addPoint();
    lastClickedPoint->setX (width() - 1);
    lastClickedPoint->setY (0);
    addPoint();
}

TransferFunctionFrame::~TransferFunctionFrame ()
{
    pointsX.resize(0);
    pointsY.resize(0);
    __pointsX.resize(0);
    __pointsY.resize(0);
}

goArray<volFloat>*
TransferFunctionFrame::getPointsX()
{
    __pointsX.resize(pointsX.getSize());
    goIndex_t i;
    volFloat __width = (xMax - xMin) / (float)this->width();
    for (i = 0; i < pointsX.getSize(); i++)
	{
	    __pointsX[i] = xMin + pointsX[i] * __width;
	}
    return &__pointsX;
}

goArray<volFloat>*
TransferFunctionFrame::getPointsY()
{
    __pointsY.resize(pointsY.getSize());
    goIndex_t i;
    volFloat __height = (yMax - yMin) / (float)this->height();
    for (i = 0; i < pointsY.getSize(); i++)
	{
	    __pointsY[i] = yMin + (height() - pointsY[i]) * __height;
	}
    return &__pointsY;
}

void
TransferFunctionFrame::openPopup ()
{
    QPoint p;
    p.setX (lastClickedPoint->x() + this->x());
    p.setY (lastClickedPoint->y() + this->y());
    popupMenu->popup (p);
}

void
TransferFunctionFrame::addPoint ()
{
    // cout << "Adding point " << lastClickedPoint->x() << "," << lastClickedPoint->y() << endl;
    pointsX += lastClickedPoint->x();
    pointsY += lastClickedPoint->y();
    this->sortPoints();
    this->update();
    emit functionChanged();
}

void
TransferFunctionFrame::removePoint ()
{
    if (pointsX.getSize() <= 2)
	{
	    return;
	}
    int i;
    for (i = 0; i < pointsX.getSize(); i++)
	{
	    if ( (pointsX[i] == closestPoint->x()) &&
		 (pointsY[i] == closestPoint->y()) )
		{
		    break;
		}
	}
    if (i == pointsX.getSize())
	{
	    return;
	}
    pointsX.remove (i);
    pointsY.remove (i);
    this->update();
    emit functionChanged();
}

void
TransferFunctionFrame::sortPoints()
{
    pointsX.sort (pointsY);
}


void
TransferFunctionFrame::mouseMoveEvent (QMouseEvent *e)
{
    if (isDraggingPoint)
	{
	    int x = e->x();
	    int y = e->y();
	    if (x >= width())
		{
		    x = width() - 1;
		}
	    if (x < 0)
		{
		    x = 0;
		}
	    if (y >= height())
		{
		    y = height() - 1;
		}
	    if (y < 0)
		{
		    y = 0;
		}
	    pointsX[draggingPoint] = x;
	    pointsY[draggingPoint] = y;
	    // cout << "Setting point " << draggingPoint << " at " << e->x() << "," << e->y() << endl;
	    this->update();
	    emit functionChanged();
	}
}

void
TransferFunctionFrame::mousePressEvent (QMouseEvent *e)
{
    int x = e->x();
    int y = e->y();
    goIndex_t i = 0, j = 0;
    double minDistance = (pointsX[0] - x) * (pointsX[0] - x) + (pointsY[0] - y) * (pointsY[0] - y);
    double distance;
    for (i = 1; i < pointsX.getSize(); i++)
	{
	    distance = (pointsX[i] - x) * (pointsX[i] - x) + (pointsY[i] - y) * (pointsY[i] - y);
	    if (distance < minDistance)
		{
		    minDistance = distance;
		    j = i;
		}
	}
    closestPoint->setX (pointsX[j]);
    closestPoint->setY (pointsY[j]);
    if (e->button() == RightButton)
	{
	    lastClickedPoint->setX(e->x());
	    lastClickedPoint->setY(e->y());
	    this->openPopup();
	    return;
	}
    if (e->button() == LeftButton)
	{
	    isDraggingPoint = true;
	    draggingPoint = (int)j;
	}
}

void
TransferFunctionFrame::mouseReleaseEvent (QMouseEvent *e)
{
    if (isDraggingPoint && (e->button() == LeftButton))
	{
	    isDraggingPoint = false;
	}
}

void
TransferFunctionFrame::paintEvent (QPaintEvent *e)
{
    sortPoints();
    QPainter p;
    p.begin (this);
    goIndex_t i;
    p.fillRect (0,0,width(),height(),QColor(0,0,0));
    p.setPen (QColor (0,255,0));
    if (pointsX.getSize() > 0)
	{
	    p.moveTo (pointsX[0], pointsY[0]);
	}
    for (i = 1; i < pointsX.getSize(); i++)
	{
	    p.lineTo (pointsX[i], pointsY[i]);
	}
    p.setPen (QColor (255,0,0));
    for (i = 0; i < pointsX.getSize(); i++)
	{
	    p.drawPoint (pointsX[i], pointsY[i]);
	}
    p.flush();
    p.end();
}

void
TransferFunctionFrame::rescalePoints (int w, int h, int newW, int newH)
{
    double scaleX = newW / (float)w;
    double scaleY = newH / (float)h;
    goIndex_t i;
    for (i = 0; i < pointsX.getSize(); i++)
	{
	    pointsX[i] = (int)(pointsX[i] * scaleX);
	    pointsY[i] = (int)(pointsY[i] * scaleY);
	}
}

void
TransferFunctionFrame::resizeEvent (QResizeEvent *e)
{
    this->rescalePoints (e->oldSize().width(), e->oldSize().height(), e->size().width(), e->size().height());
    QFrame::resizeEvent (e);
    this->update();
}
