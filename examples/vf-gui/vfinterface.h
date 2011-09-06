/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef VFINTERFACE_H
#define VFINTERFACE_H

#include <govolumefile.h>
#include <gopresencemanager.h> // dummy for edit
#include <fstream.h>
#include <gotypes.h>
#include <gostring.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <time.h>


#include <SDL.h>
#include <SDL_image.h>

template<class T>
class VFInterface 
{
	public:
		VFInterface();
		virtual ~VFInterface();
		void setMaxMemory(goSize_t mm);
		void raw2dwtv(const char* rawFileName, const char* dwtvFileName,
					  goSize_t size_x, goSize_t size_y, goSize_t size_z,
					  goSize_t blocksize_x, goSize_t blocksize_y, goSize_t blocksize_z,
					  int file_type, int stages, bool save_memory=false);
		void jpeg2dwtv(const char* textFileName, const char* dwtvFileName,
					   goSize_t size_x, goSize_t size_y, goSize_t size_z,
					   goSize_t blocksize_x, goSize_t blocksize_y, goSize_t blocksize_z,
					   int file_type, int stages);
	protected:
		void loadImage(goSignal3D<T>* signal, const char* filename);
	private:
		goSize_t maxMemory;	
};

#endif

