/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <vfapplication.h>
#include <qapplication.h>
#include <qplatinumstyle.h>

int main(int argc, char* argv[])
{
	QApplication app(argc, argv);
	VFApplication vfapp;
	app.setStyle(new QPlatinumStyle);
	QObject::connect((const QObject*)vfapp.quitButton,SIGNAL(clicked()),
					 (QObject*)&app,SLOT(quit()));
	app.setMainWidget(&vfapp);
	vfapp.show();
	return app.exec();
}

