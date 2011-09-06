/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goposdisplay3d.h>
#include <gotypes.h>
int main(int argc, char** argv)
{
	Vol::goPosDisplay3D disp;
	goSize3D sz;
	sz.x = 2136;
	sz.y = 1932;
	sz.z = 1539;
	disp.setVolumeSize(sz);
	goViewVolume v;
	v.setPosition(0,0,0);
	//v.setNormal(0,0,1);
	//v.setUp(0,1,0);
	v.update();
	disp.setViewVolume(v);	
	disp.init(argc, argv);
	char c;
	cin >> c;
	return 1;
}
