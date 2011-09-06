/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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

#define TYPE goInt16
#define BLOCKSIZE 256

bool
askchange()
{
	char c[2];
	cout << "Change?";
	cin >> c;
	if (c[0] == 'y')
		return true;
	return false;	
}

void
readnewvalue(goString &s)
{
	s.resize(s.getSize()+1);
	cout << " New value: ";
	cin >> s.getPtr();
}

void loadImage (goSignal3D<TYPE>* signal, const char* filename)
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
		    *signal->getPtr(x,y,0) = (TYPE)(255 - *p);
		    p += bytesPP;	
		}
		p2 += img->pitch;
	}
	SDL_FreeSurface (img);
}



int main (int argc, char* argv[])
{
    if (argc < 2)
    {
	cout << argv[0] << " <command>\nWith command in \n\tcreate -- creates a linear volume\n\tconvert -- converts the linear volume file to a dwt volume file\n";
	cout << "\tjpeg2dwtv -- converts a set of JPEG images to a dwt volume file\n";
	cout << "\ttest -- tests the transform file\n";
	exit(1);
    }
    goString command;
    command = argv[1];
    goString filename;
    goString transFileName;
    clock_t t1,t2;

    goSignal3D<TYPE> signal;

    if (command == "create")
    {
	signal.make (256,256,256,0,0,0);
	goSize_t x,y,z;
	for (z = 0; z < signal.getSizeZ(); z++)
	    {
		for (y = 0; y < signal.getSizeY(); y++)
		    {
			for (x = 0; x < signal.getSizeX(); x++)
			    {
				*signal.getPtr (x,y,z) = (TYPE)z;
				//    if ( ((x <= 1) + (y <= 1) + (z <= 1) +
				//  (x >= signal.getSizeX() - 2) +
				//  (y >= signal.getSizeY() - 2) +
				//  (z >= signal.getSizeZ() - 2)) >= 2 )
				//*signal.getPtr(x,y,z) = 127;
				// *signal.getPtr(x,y,z) = (TYPE)signal.getSizeX() - x;
			    }
		    }
	    }
	if (argc < 3)
	{
	  cout << "Format: create <raw file>" << endl;
	  exit(2);
	}
        filename = argv[2];
        cout << "Creating file " << filename << endl;
	ofstream outfile;
	t1 = clock();
	outfile.open(filename.toCharPtr(), ios::out|ios::binary);
	signal.write(outfile);
	outfile.close();
	t2 = clock();
	cout << "Time: " << (t2 - t1) / (float)CLOCKS_PER_SEC << "s" << endl;
    }
    if (command == "convert")
    {
        
        if (argc < 9)
	  {
	    cout << "Format: convert <raw file> <transformed file> <size x> <size y> <size z> <type> <stages>" << endl;
	    cout << "\t<type> is one of {blockwise,bandwise}" << endl;
	    exit(2);
	  }
        filename = argv[2];
        transFileName = argv[3];
	cout << "Converting " << filename << " to " << transFileName << endl;
	Vol::goVolumeFile<TYPE> volFile;
	volFile.setLinearFileName(filename.toCharPtr());
	volFile.setTransFileName(transFileName.toCharPtr());
	goSize3D volSize;
	volSize.x = atoi(argv[4]);
	volSize.y = atoi(argv[5]);
	volSize.z = atoi(argv[6]);
	volFile.setVolumeSize(volSize);
	if (strcmp (argv[7],"blockwise") == 0)
	    {
		volFile.setTransFileType (Vol::GO_VOLUMEFILE_BLOCKWISE);
		cout << "Set to blockwise!" << endl;
	    }
	else 
	    {
		if (strcmp (argv[7],"bandwise") == 0)
		    {
			volFile.setTransFileType (Vol::GO_VOLUMEFILE_BANDWISE);
			cout << "Set to bandwise!" << endl;
		    }
		else
		    {
			cout << "Unknown file type " << argv[7] << endl;
			exit(2);
		    }
	    }
	int stages = atoi(argv[8]);	
	goSize3D blocksize;
	blocksize.x = BLOCKSIZE;
	blocksize.y = BLOCKSIZE;
	blocksize.z = BLOCKSIZE;
	t1 = clock();
	volFile.linear2Trans(blocksize,stages);
	t2 = clock();
	cout << "Time: " << (t2 - t1) / (float)CLOCKS_PER_SEC << "s" << endl;      
    }

    if (command == "jpeg2dwtv")
    {
		if (argc < 9)
		{
			cout << argv[0] << " jpeg2dwtv <textfile> <dwtvfile> <x y z> <filetype> <stages>\n";
			cout << "\t<textfile> -- file containing the paths to the single images, one each line\n";
			cout << "\t<x y z>    -- size of the volume in volume elements\n";
			cout << "\t<filetype> -- [bandwise|blockwise]\n";
			cout << "\t<stages>   -- number of transformation stages\n";
			return 2;
		}
    	filename = argv[2];
   		 transFileName = argv[3];
		cout << "Converting images in " << filename << " to " << transFileName << endl;
		Vol::goVolumeFile<TYPE> volFile;
		volFile.setTransFileName(transFileName.toCharPtr());
		goSize3D volSize;
		volSize.x = atoi(argv[4]);
		volSize.y = atoi(argv[5]);
		volSize.z = atoi(argv[6]);
		volFile.setVolumeSize(volSize);
		bool bandwise = false;
		if (strcmp(argv[7],"bandwise") == 0)
		{
			volFile.setTransFileType (Vol::GO_VOLUMEFILE_BANDWISE);
			bandwise = true;
			cout << "File type set to bandwise.\n";
		} else
		{
			volFile.setTransFileType (Vol::GO_VOLUMEFILE_BLOCKWISE);
			bandwise = false;
			cout << "File type set to blockwise.\n";
		}
		int stages = atoi(argv[8]);
		goSize3D blocksize;
		blocksize.x = BLOCKSIZE;
		blocksize.y = BLOCKSIZE;
		blocksize.z = BLOCKSIZE;
		t1 = clock();
	
		ofstream outfile;
		if (bandwise)
		{
			volFile.l2t_bandwise_init (blocksize,stages);
		} else
		{
			volFile.l2t_blockwise_init (blocksize,stages,&outfile);
		}
	
		goSize_t zSize = (goSize_t)volFile.getFileInfo().blockSize.z;
		goSignal3D<TYPE> slab;
		slab.make (volFile.getFileInfo().blocks.x * volFile.getFileInfo().blockSize.x,
			   volFile.getFileInfo().blocks.y * volFile.getFileInfo().blockSize.y,
			   volFile.getFileInfo().blockSize.z, 0, 0, 0);
		goSubSignal3D<TYPE> subSignal;
		subSignal.setParent (&slab);
		subSignal.setSize (slab.getSizeX(), slab.getSizeY(), 1);
		subSignal.setDiff (slab.getXDiff(), slab.getYDiff(), slab.getZDiff());
		goSize_t i,iMax,j;
		iMax = volFile.getFileInfo().blocks.z;
	
		char imageFileName[512];
		ifstream textfile;
		textfile.open (filename.toCharPtr());
	
		goSize_t imageCounter = volSize.z;	
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

    if (command == "test")
    {
	cout << "Testing and reading file " << transFileName << endl;
	Vol::goVolumeFile<TYPE>	volFile;
	Vol::goCluster3D<TYPE>		cluster;
	goSignal3D<TYPE>		block;
	block.make (BLOCKSIZE,BLOCKSIZE,BLOCKSIZE);
	goSubSignal3D<TYPE>		block2;
	block2.setParent (&signal);
	block2.setSize (BLOCKSIZE,BLOCKSIZE,BLOCKSIZE);
	block2.setDiff (signal.getXDiff(), signal.getYDiff(), signal.getZDiff());
	block2.setPosition (0,0,0);

	volFile.setTransFileName(transFileName.toCharPtr());
	t1 = clock();
	// volFile.readTrans(cluster);
	volFile.openTrans();
	volFile.readTransBlock (0,&block,3);
	volFile.closeTrans();
	t2 = clock();
	cout << "Time: " << (t2 - t1) / (float)CLOCKS_PER_SEC << "s" << endl;
	int x,y;
	for (y = 0; y < block.getSizeY(); y++)
	{
	    for (x = 0; x < block.getSizeX(); x++)
	    {
	      cout << *block.getPtr(x,y,0) << " ";
	    }
	    cout << endl;
	}
	cout << endl;
	cout << "block ";
	goDouble var = 0;
	if (block == block2)
	{
	    cout << "==";
	} else
	{
	    cout << "!=";
	    int i,j,k;
	    for (i = 0; i < block.getSizeX(); i++)
	      {
		for (j = 0; j < block.getSizeY(); j++)
		  {
		    for (k = 0; k < block.getSizeZ(); k++)
		      {
			var += fabs(*block.getPtr(i,j,k) - *block2.getPtr(i,j,k));
		      }
		  }
	      }
	    var /= (float)(i * j * k);

	}
	cout << " block2" << endl;
	cout << "Mean error: " << var << endl;
	block.destroy();
    }

	if (command == "edit")
	{
		cout << "Editing dwtv footer\n";
		if (argc < 3)
		{
			cout << "Format: edit <filename>\n";
			return(2);
		}
		Vol::goVolumeFile<TYPE> volFile;
		Vol::goPresenceManager<TYPE> pm; // dummy for the volume file
		volFile.setPresenceManager(&pm);
		volFile.setTransFileName(argv[2]);
		volFile.openTrans();
		volFile.closeTrans();
		Vol::goVolumeFileInfo info;
		info = volFile.getFileInfo();
		cout << "Version = " << info.version << " ";
		if (askchange())
		{
			goString s;
			s.resize(1);
			readnewvalue(s);
			info.version = s.toInt();
		}
		cout << "Minimum = " << info.minimum << " ";
		if (askchange())
		{
			goString s;
			s.resize(10);
			readnewvalue(s);
			info.minimum = s.toInt();
		}
		cout << "Maximum = " << info.maximum << " ";
		if (askchange())
		{
			goString s;
			s.resize(10);
			readnewvalue(s);
			info.maximum = s.toInt();
		}
		volFile.overwriteFileInfo(info);
		// ... read user input
		// ... overwrite file info
	}
	
	
    cout << "Command was " << command << endl;
    signal.destroy();

	
    exit(1);
}


