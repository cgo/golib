/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <vfgui.h>

class VFApplication : public VFGUI
{
	Q_OBJECT
	public:
		VFApplication(QWidget *parent = 0, const char* name = 0, WFlags f = 0);
		~VFApplication();
	
	public slots:
		void convert();
		void browseRawFileName();
		void browseTransFileName();		
	protected:
	private:	
};
