/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GO_POSDISPLAY3D_H
#define GO_POSDISPLAY3D_H

#include <gotypes.h>
#include <goviewvolume.h>
#include <go3vector.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include <gothread.h>

namespace Vol {


class
goPosDisplay3D
{
	public:
		goPosDisplay3D();
		virtual ~goPosDisplay3D();

		void init(int argc, char** argv, int width=256, int height=256);
		void setViewVolume(goViewVolume& v);
		void setVolumeSize(goSize3D& sz);
	protected:
	private:
		goSize3D volumeSize;
		bool initialized;
		goThread thread;
};

};
#endif
