#ifndef __GOSIGNAL2D_H
#define __GOSIGNAL2D_H

#define GOSIGNAL_BLOCKSIZE_X 0
#define GOSIGNAL_BLOCKSIZE_Y 0

#include <goarray.h>
#include <gomatrix.h>
#include <gotypes.h>
#include <goobjectbase.h>

/*!
 * 2D signal class
 * @author Christian Gosch
 * @see goFilterFIR
 * @see goFilterGauss
 * @see goFilter
 */
template <class T>
class goSignal2D : goObjectBase {
 public:
  /*!
   * blockSizes are in number of shifts and not used yet !
   * For now, data is stored linearly.
   */
    goSignal2D (goSize_t width, goSize_t height, 
		goSize_t border_x = 16, goSize_t border_y = 16,
		goSize_t blockWidth = GOSIGNAL_BLOCKSIZE_X, 
		goSize_t blockHeight = GOSIGNAL_BLOCKSIZE_Y);

  virtual ~goSignal2D ();

  /*! 
   * Get Ptr to point [x,y]. <br>
   * Uses Multiplication, so don't use excessively yet !
   * @return Pointer to point [x,y]
   */
  T* getPtr (goIndex_t x, goIndex_t y);

  /*!
   * @return Pointer to point [0,0]
   */
  T* getPtrStart ();

  goIndex_t getBorderX () { return borderX; }
  goIndex_t getBorderY () { return borderY; }
  goSize_t  getSizeX () { return sizeX; }
  goSize_t  getSizeY () { return sizeY; }

  /// Currently just returns 1.
  goPtrOffset_t  getOffsetX () { return 1; }
  /// @return Pointer distance between point (x, y) and (x, y + 1)
  goPtrOffset_t  getOffsetY ();

  /*!
   * Resizing the 2D signal. The signal itself is stored in the "upper left" corner of
   * the new resized signal. <br>
   * Speed: slow.
   * Tested: no.
   */
  void resize (goSize_t width, goSize_t height);

  /// Subtracts other from this.
  goSignal2D<T>& operator-= (goSignal2D<T>& other);
  /// Adds other to this.
  goSignal2D<T>& operator+= (goSignal2D<T>& other);
  /// Multiply all samples with value.
  goSignal2D<T>& operator*= (T value);
  /// Copy operator (deep copy).
  goSignal2D<T>& operator= (goSignal2D<T>& other);

  /**
   * Calculates the mean difference between other and this. 
   * Other's upper left corner lies at (xrel, yrel) relative this' upper left corner.
   * TO DO: calculate the deviation of the mean difference (to detect brighness differences).
   */
  T meanDiff ( goSignal2D<T>& other, 
	       goIndex_t xrel, goIndex_t yrel );

//  T meanDiff ( goSignal2D<T>& other, 
//	       goMatrix<goDouble>& m );

  void fill (T val);

  void put (goSignal2D<T>& s, goIndex_t x, goIndex_t y);

  /**
   * Interpolates the signal into the borders (only one sample wide so far).
   * The argument does nothing at the moment, the neighbouring value is simply copied.
   */
  void interpolateBorders (bool interpolate = false);

  /**
   * Copy lines from offset xleft to the the -1st column, xright to the sizeX'st column and same in y direction.
   * (Introduced for upsampled signals).
   */ 
  void interpolateBorders (goIndex_t xleft, goIndex_t xright, 
			   goIndex_t yup, goIndex_t ydown);

  void interpolateBordersPeriodic () ;
  /*!
   * Bilinear interpolation
   */
  inline void sample (goFloat x, goFloat y, goFloat &c);

  goSize_t    memoryUsage () { return total_memory_usage; }
 protected:
  goSize_t sizeX;
  goSize_t sizeY;
  goSize_t blockSizeX;
  goSize_t blockSizeY;
  goIndex_t borderX;
  goIndex_t borderY;
  
  goArray<T> *data;
  goSize_t   total_memory_usage;
};

template <class T>
inline 
void
goSignal2D<T>::sample (goFloat x, goFloat y, goFloat &c)
{
    goFloat px, py;
    double ixd, iyd;
    px = modf (x, &ixd);
    py = modf (y, &iyd);
    goIndex_t ix = (goIndex_t)ixd;
    goIndex_t iy = (goIndex_t)iyd;

    T* p 	= getPtr (ix, iy);
    T A   	= *p;
    T B   	= *(p + getOffsetX());
    T C		= *(p + getOffsetY());
    T D		= *(p + getOffsetY() + getOffsetX());
    // Interpolate bilinearly
    goFloat p1 = A + ((B - A)*px);
    goFloat p2 = C + ((D - C)*px);
    c = (p1 + ((p2 - p1)*py));
    //cout << "px = " << px << ", py = " << py << endl;
    //cout << A << "," << B << "," << C << "," << D << " --> " << c << endl;
}

#endif












