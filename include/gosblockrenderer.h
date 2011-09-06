/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOSBLOCKRENDERER_H
#define GOSBLOCKRENDERER_H

#include <goblockrenderer.h>
#include <gohashtable.h>
#include <go4vector.h>
#include <goconsumer.h>
#include <gothread.h>
#include <gosignal2d.h>
#include <gopresencemanager.h>
#include <goarray.h>
#include <gopowfunction.h>

namespace Vol {

/*
 * Table of power functions, each from 0.0 -> 1.0,
 * to make alpha correction a bit quicker.
 * No error checking or anything. Input values MAY ONLY BE BETWEEN 0.0 and 1.0, inclusive.
 * Through the use of tables the function is more inaccurate but faster.
 */
class powFuncTable
{
	public:
		powFuncTable();
		~powFuncTable();
		void init(int maxres);
		inline volFloat get(int resdiff,volFloat v);
		
		goArray<void*> powfuncs;	
};

inline volFloat powFuncTable::get(int resdiff,volFloat v)
{
	return (volFloat)((goPowFunction*)powfuncs[resdiff])->operator[](v);
}

class
goSubImage 
{
 public:
    goSubImage(goSize_t w, goSize_t h)
	{
	    width = w; height = h;
	    left = right = top = bottom = 0;
	    imageData = new goSignal2D<volFloat> (w,h,2,2);
	    alphaData = new goSignal2D<volFloat> (w,h,2,2);
	}
    ~goSubImage()
	{
	    delete imageData;
	    delete alphaData;
	}
    
    goSize_t		width;		
    goSize_t		height;
    /*
     * leftImage, rightImage, topImage, bottomImage are in view plane coordinates BUT WITH UPPER LEFT CORNER OF THE VIEWPLANE BEING (0,0)!!!
     * (see .cc file, initSubImages()!)
     */
    volFloat		left;		// exact left margin on the view plane (in view plane coordinates)
    volFloat		right;		// exact right margin on the view plane (in view plane coordinates)
    volFloat		top;		// exact top margin on the view plane (in view plane coordinates)
    volFloat		bottom;		// exact bottom margin on the view plane (in view plane coordinates)
    goInt32 		leftImage;     	// exact left margin on the view plane (upper left = (0,0))
    goInt32 		rightImage;    	// exact right margin on the view plane (upper left = (0,0))
    goInt32 		topImage;	// exact top margin on the view plane (upper left = (0,0))
    goInt32 		bottomImage;	// exact bottom margin on the view plane (upper left = (0,0))
    volFloat		scaleFactorX;
    volFloat		scaleFactorY;
    goInt32			firstX;
    goInt32			firstY;
    goInt32			lastX;
    goInt32			lastY;

    goInt32			firstXHi;
    goInt32			firstYHi;
    goInt32			lastXHi;
    goInt32			lastYHi;
 
    goInt32			blockX;		// Position of block in block coords
    goInt32			blockY;		// (xBlocks, yBlocks, zBlocks)
    goInt32			blockZ;
    goInt32			resDiff;	// Difference minResolution - thisResolution
    go4Vector<volFloat>		blockStartPos;	// start position of the corresponding block in world coordinates
    goSignal2D<volFloat>*	imageData;	// actual image data, stored linearly
    goSignal2D<volFloat>*	alphaData;	// image alpha values
    goUInt32			ageStamp;	// value determining the "age", the number of views that have passed
						// since this image was created
};

/*!
 * Subimage based renderer.
 * You can use this class as an interface for better renderers.
 * The basic thing to do is to overwrite renderThread() with a method that renders
 * the given block indices onto the tempImage structure (see goBlockRenderer).
 * That structure is to be filled with float values that will be mapped to grey levels
 * using the grey level transfer function (but not by the renderer).
 * If you don't want to use this interface at all, you have to replace it in the
 * goViewManager class by your own (which is not hard at all).
 * @note This code is experimental and changes very often during the course of my Diplomarbeit.
 * @author Christian Gosch
 * @date 2.9.2001
 */
template <class T>
class
    goSBlockRenderer : public goBlockRenderer<T>, public goConsumer
{
 public:
    goSBlockRenderer();
    virtual ~goSBlockRenderer();
    
    /*!
     * Overwrite this method and renderSubImages() to incorporate other renderers than the simple 
     * internal one.
     * @throw goExceptionString
     */
    virtual void init ();
	/// Starts rendering
    virtual void render		 (goArray<goSize_t> *indices);
    // Helper for renderSubImages (experimental)
	// DON'T USE
    void         interpolateBlock (goSignal3D<T>* block, goSize_t index,
		    		   goInt32 blockX, goInt32 blockY,
		    		   goInt32 blockZ);
    
    virtual void renderThread	 (goArray<goSize_t> *indices);
    virtual void renderSubImages (goArray<goSize_t> *indices);
    
    virtual void compositeSubImages (goArray<goSize_t> *indices);
    void 	 compositeMRImages ();

    /*!
     * Sets a pointer to a presence manager object which  will be used to check if a block is loaded already.
     * This is added to enable a comfortable multi threaded architecture.
     * @param p Pointer to the goPresenceManager object to be used for checking the presence of blocks.
     */
    void setPresenceManager (goPresenceManager<T> *p) { pm = p; }

    /*!
     * @return Time for the last rendering process in seconds.
     * When no rendering has taken place yet, the return value is 0.0.
     */
    goDouble getRenderTime () { return renderTime; }

	class render_thread_arg
	{
		public:
    		void* renderer;
    		goArray<goSize_t> indices;
	};

 protected:
    /*!
     * Create and initialize the images, calculate bounds on the final picture
     */
    virtual void initSubImages (goArray<goSize_t>* indices);
    void initMR ();

    // Presence manager used to check the availability of blocks.
    goPresenceManager<T> *pm;
 private:

    /*
     * Add some sub image structure for convenient and fast storage, scaling and
     * retrieval.
     */ 
    
    // goSubImage pointers
    goHashTable<goSize_t,void*>	subImages;
    // goViewVolume* array for multires. views (scaled)
    goArray<void*>		MRViews;		// Filled with views for the different resolutions
    // goSignal2D<volFloat>* array for multires.tempImages
    goArray<void*>		MRImages;		// NOT USED, PLEASE IGNORE
    

    // Time needed for the last rendering process
    goDouble renderTime;
	
	render_thread_arg	 threadArg;

	powFuncTable		 powFuncs;		// pow functions as tables (experimental)
};
};
#endif
