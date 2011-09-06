/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include "vfinterface.h"
#include <govol.h>
#include <govolumefile.h>
#include <gotypes.h>
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>

template<class T>
VFInterface<T>::VFInterface()
{
	setMaxMemory(0 * 1024 * 1024);
}

template<class T>
VFInterface<T>::~VFInterface()
{
}

#define RAW2DWTV_BODY(TYPE) { \
	Vol::goVolumeFile<TYPE> volFile; \
	volFile.setLinearFileName(rawFileName); \
	volFile.setTransFileName(dwtvFileName); \
	goSize3D volSize; \
	volSize.x = size_x; \
	volSize.y = size_y; \
	volSize.z = size_z; \
	volFile.setVolumeSize(volSize); \
	volFile.setTransFileType (file_type); \
	goSize3D blocksize; \
	blocksize.x = blocksize_x; \
	blocksize.y = blocksize_y; \
	blocksize.z = blocksize_z; \
	clock_t t1,t2; \
	t1 = clock(); \
	volFile.linear2Trans(blocksize,stages); \
	t2 = clock(); \
	cout << "Time: " << (t2 - t1) / (float)CLOCKS_PER_SEC << "s" << endl; \
}

template<class T>
void
VFInterface<T>::setMaxMemory(goSize_t mm)
{
	maxMemory = mm;
}

template<class T>
void
VFInterface<T>::raw2dwtv(const char* rawFileName, const char* dwtvFileName,
					  goSize_t size_x, goSize_t size_y, goSize_t size_z,
					  goSize_t blocksize_x, goSize_t blocksize_y, goSize_t blocksize_z,
					  int file_type, int stages, bool save_memory)
{
	cout << "Converting " << rawFileName << " to " << dwtvFileName << endl;
	Vol::goVolumeFile<T> volFile; 
	volFile.setLinearFileName(rawFileName); 
	volFile.setTransFileName(dwtvFileName); 
	goSize3D volSize; 
	volSize.x = size_x; 
	volSize.y = size_y; 
	volSize.z = size_z; 
	volFile.setVolumeSize(volSize); 
	volFile.setTransFileType (file_type); 
	goSize3D blocksize; 
	blocksize.x = blocksize_x; 
	blocksize.y = blocksize_y; 
	blocksize.z = blocksize_z; 
	bool low_memory = false;
	if (maxMemory < (volSize.x * volSize.y * blocksize.z))
	{
		cout << "Choosing slower linewise conversion method due to low memory\n";
		low_memory = true;
	}
	clock_t t1,t2; 
	t1 = clock();
	volFile.linear2Trans(blocksize,stages,low_memory,save_memory); 
	t2 = clock(); 
	cout << "Time: " << (t2 - t1) / (float)CLOCKS_PER_SEC << "s" << endl; 
}

template<class T>
void 
VFInterface<T>::jpeg2dwtv(const char* textFileName, const char* dwtvFileName,
					   goSize_t size_x, goSize_t size_y, goSize_t size_z,
					   goSize_t blocksize_x, goSize_t blocksize_y, goSize_t blocksize_z,
					   int file_type, int stages)
{
	
	goSize3D blockSize, volSize;
	volSize.x = size_x;
	volSize.y = size_y;
	volSize.z = size_z;
	blockSize.x = blocksize_x;
	blockSize.y = blocksize_y;
	blockSize.z = blocksize_z;
	
	Vol::goVolumeFile<T> volFile;
	volFile.setTransFileName(dwtvFileName);
	volFile.setVolumeSize(volSize);
	volFile.setTransFileType(file_type);
	bool bandwise = (file_type == Vol::GO_VOLUMEFILE_BANDWISE);
	ofstream outfile;
	if (bandwise)
	{
		volFile.l2t_bandwise_init (blockSize,stages);
	} else
	{
		volFile.l2t_blockwise_init (blockSize,stages,&outfile);
	}
	

	goSize_t zSize = (goSize_t)volFile.getFileInfo().blockSize.z;
	goSignal3D<T> slab;
	slab.make (volFile.getFileInfo().blocks.x * volFile.getFileInfo().blockSize.x,
			   volFile.getFileInfo().blocks.y * volFile.getFileInfo().blockSize.y,
			   volFile.getFileInfo().blockSize.z, 0, 0, 0);

	goSubSignal3D<T> subSignal;
	subSignal.setParent (&slab);
	subSignal.setSize (slab.getSizeX(), slab.getSizeY(), 1);
	subSignal.setDiff (slab.getXDiff(), slab.getYDiff(), slab.getZDiff());
	goSize_t i,iMax,j;
	iMax = volFile.getFileInfo().blocks.z;

	char imageFileName[512];
	ifstream textfile;
	textfile.open (textFileName);

	goSize_t imageCounter = volSize.z;	
	clock_t t1,t2;
	t1 = clock();
	for (i = 0; i < iMax; i++)
	{
		for (j = 0; (j < zSize) && (imageCounter > 0); j++)
		{
				/* read jpeg image into slice j of slab */
			textfile >> &imageFileName[0];
			cout << imageFileName << endl;
			subSignal.setPosition (0,0,j);
			loadImage (&subSignal, (const char*)&imageFileName[0]);	
			imageCounter--;
		}
		if (bandwise)
		{
			volFile.l2t_bandwise_writeslab(slab);
		} else
		{
			volFile.l2t_blockwise_writeslab (slab, &outfile);
		}
	}
  	 	textfile.close();    
	if (bandwise)
	{
		volFile.l2t_bandwise_finish();
	} else
	{
		volFile.l2t_blockwise_finish(&outfile);
	}
	slab.destroy();
	t2 = clock();
	cout << "Time: " << (t2 - t1) / (float)CLOCKS_PER_SEC << "s" << endl;      
}

template<class T>
void
VFInterface<T>::
loadImage(goSignal3D<T>* signal, const char* filename)
{
	SDL_Surface *img = IMG_Load (filename);
	if (!img)
	{
		cout << "Error loading file " << filename << endl;
		return;
	}
	int bytesPP = img->format->BytesPerPixel;
	cout << "Bytes per pixel: " << bytesPP << endl;
	cout << "Taking the lowest byte." << endl;
	Uint8* p2 = (Uint8*)img->pixels + bytesPP - 1;
	Uint8* p;
	int x,y;
	int xMax = signal->getSizeX();
	int yMax = signal->getSizeY();
	cout << "Dimensions: " << endl;
	cout << "\t" << img->w << "," << img->h << endl;
	if (xMax > img->w)
	{
		cout << "Image smaller than signal." << endl;
		xMax = img->w;
	}
	if (xMax < img->w)
	{
		cout << "Signal smaller than image." << endl;
	}
	if (yMax > img->h)
	{
		cout << "Image smaller than signal." << endl;
		yMax = img->h;
	}
	if (yMax < img->h)
	{
		cout << "Signal smaller than image." << endl;
	}
	for (y = 0; y < yMax; y++)
	{
		p = p2;
		for (x = 0; x < xMax; x++)
		{
		    // Invertieren
		    *signal->getPtr(x,y,0) = (goUInt8)(255 - *p);
		    p += bytesPP;	
		}
		p2 += img->pitch;
	}
	SDL_FreeSurface (img);
}


template class VFInterface<goInt8>;
template class VFInterface<goUInt8>;
template class VFInterface<goInt16>;
template class VFInterface<goUInt16>;
template class VFInterface<goInt32>;
template class VFInterface<goUInt32>;
template class VFInterface<goFloat>;
template class VFInterface<goDouble>;
