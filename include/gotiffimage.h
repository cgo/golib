/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef __GOTIFFIMAGE_H
#define __GOTIFFIMAGE_H

#include <goconfig.h>
#ifdef HAVE_LIBTIFF
#include <tiffio.h>

/*!
 * TIFF image class. Wrapper for libtiff.
 * Use the corresponding static member in {\tt gameImageLoad} to load a TIFF image.
 * Currently only 32 bit / pixel RGBA data is supported (and it will probably stay like this).
 * @see gameImageLoad
 * @author Christian Gosch */
class goTIFFImage {
 public:
  ///
  goTIFFImage ();
  ///
  virtual ~goTIFFImage ();

  ///
  inline uint32	getWidth() { return (uint32)tiffImage.width; }
  ///
  inline uint32	getHeight() { return (uint32)tiffImage.height; } 

  ///
  TIFFRGBAImage*	getTiffImagePtr() { return &tiffImage; }
  
  ///
  bool			read (const char *filename);
  ///
  bool			write (const char *filename, uint32 width, uint32 height);
  
  ///
  uint32*		getBufferPtr () {return buffer;}
  ///
  void			setBufferPtr (uint32* b) {buffer = b;}

  ///
  goTIFFImage&		operator= (goTIFFImage& other);
 protected:
  ///
  TIFFRGBAImage			tiffImage;
  ///
  uint32*			buffer;
};

#endif /* HAVE_LIBTIFF */
#endif
