#ifndef GOVIEWPLANE_H
#define GOVIEWPLANE_H

#include <gotypes.h>
#include <go3vector.h>
#include <go44matrix.h>
#include <goobjectbase.h>
#include <govol.h>

/*!
 * Storage class for the parameters describing a view plane in 3D space.
 * @author Christian Gosch
 */
class
goViewPlane : public goObjectBase {
 public:
  goViewPlane ();
  goViewPlane (goViewPlane& other);
   
  virtual ~goViewPlane ();

	/*!
	 * @return Principal point of the view plane in world
	 * coordinates. This is the point in the middle of the
	 * view plane.
	 */
  	go3Vector<volFloat>&  getPosition()  { return position; }
	/*!
	 * @return The size of the view plane in pixels. Only
	 * the x and y members of the returned vector are valid.
	 */
  	go3Vector<volFloat>&  getSize ()  { return size; }
	/*!
	 * @return The intended screen size for this
	 * viewplane in pixels.
	 */
  	go3Vector<volFloat>&  getScreenSize () { return screenSize; }
	/*!
	 * Sets the eye position for the view.
	 * @param p Vector which contains the new eye position.
	 */
  	void 			setEyePos (go3Vector<volFloat>& p) { eyePos = p; }
	/*!
	 * Sets the size of the view plane in pixels. Only
	 * the x and y components of the parameter are valid.
	 * @param s Size of the view plane.
	 */
  	void 			setSize (go3Vector<volFloat>& s) { size = s; }
	/*!
	 * Sets the intended screen size for this viewplane
	 * in pixels.
	 */
  	void 			setScreenSize (go3Vector<volFloat>& s) 
      { 
	  screenSize = s; 
	  size.x = screenSize.x * pixelWidth;
	  size.y = screenSize.y * pixelHeight; 
      }
  	/// For convenience. See setScreenSize(go3Vector<volFloat>& s)
	void setScreenSize (goInt32 x, goInt32 y)
      {
	  go3Vector<volFloat> temp;
	  temp.x = x;
	  temp.y = y;
	  setScreenSize (temp);
      }
  /*!
   * Size only has a meaning if you want it to.
   * You could, for example, use the size of the screen to calculate some size of your plane.
   * Or, you might just want to set it to one. Or whatever.
   */
  inline void setSize (volFloat x, volFloat y) 
    {
      size.x = x;
      size.y = y;
      screenSize.x = 1 / pixelWidth * x;
      screenSize.y = 1 / pixelHeight * y;
    }
  inline void setEyePos (volFloat x, volFloat y, volFloat z) 
    {
      eyePos.x = x;
      eyePos.y = y;
      eyePos.z = z;
    }
	/*!
	 * Sets the with of one pixel in world coordinate units.
	 * Used to calculate the screen size when you set the
	 * size of the viewplane itself.
	 * @param d Width of one screen pixel in world coordinate units.
	 */
  	inline void setPixelWidth  (volFloat d) { pixelWidth = d; size.x = d * screenSize.x; }
	/*!
	 * Sets the height of one pixel in world coordinate units.
	 * Used to calculate the screen size when you set the
	 * size of the viewplane itself.
	 * @param d Height of one screen pixel in world coordinate units.
	 */
  	inline void setPixelHeight (volFloat d) { pixelHeight = d; size.y = d * screenSize.y; }
	/*!
	 * @return Width of one screen pixel in world coordinate units.
	 */
  	inline volFloat getPixelWidth () { return pixelWidth; }
	/*!
	 * @return Height of one screen pixel in world coordinate units.
	 */
  	inline volFloat getPixelHeight () { return pixelHeight; }
	/*!
	 * @return Plane normal of the viewplane.
	 */	
  	inline go3Vector<volFloat>& 		getNormal () { return normal; }
  	inline const go3Vector<volFloat>& 	getNormal () const { return normal; }
	/*!
	 * Sets the plane normal of the view plane.
	 * @param v Plane normal.
	 */
  	void setNormal (go3Vector<volFloat>& v) { normal = v; }
	/*!
	 * @return The up-vector of this viewplane.
	 */
  	inline go3Vector<volFloat>& 		getUp () { return up; }
  	inline const go3Vector<volFloat>& 	getUp () const { return up; }
	/*!
	 * Sets the up-vector of the view plane, that is the vector
	 * that points upwards from the viewer.
	 */
  	void setUp (go3Vector<volFloat>& u) { up = u; }
	/*!
	 * Sets the eye distance, that is the distance from 
	 * the eye position to the principal point of the viewplane,
	 * in world coordinate units.
	 * @param f Eye distance.
	 */
  	void setEyeDistance (volFloat f) { eyeDistance = f; }
	/*!
	 * @return The eye distance from the eye position to the
	 * principal point of this viewplane, in world coordinate units.
	 */	
  	volFloat getEyeDistance () const { return eyeDistance; }

	/*!
	 * Copies the properties of another viewplane into this one.
	 * You should call update() after a call to operator=.
	 * @param other Viewplane to be copied from. 
	 */
  	virtual void operator= (goViewPlane& other);
	/*!
	 * @return The 4x4 view matrix for this viewplane.
	 */	
  	const go44Matrix<volFloat>& getViewMatrix() const;
  	const go44Matrix<volFloat>& getViewMatrix() { return (const go44Matrix<volFloat>&)Tproj; }
  
  	void update();
  	inline go3Vector<volFloat>& getU() { return u; }
  	inline go3Vector<volFloat>& getV() { return v; }
  	inline go3Vector<volFloat>& getEyePos() { return eyePos; }

  /*!
   * Calculates the upper left "visible" corner on the view plane
   * in coordinates relative to the world coordinate system 
   * (NOT the view coordinate system!)
   * @param ul Reference to a go3Vector in which the coordinates are stored.
   */
  void	calculateUpperLeft  (go3Vector<volFloat>& ul);
  /*!
   * @see calculateUpperLeft
   * @param ur Reference to a go3Vector in which the coordinates are stored.
   */
  void	calculateUpperRight (go3Vector<volFloat>& ur);
  /*!
   * @see calculateUpperLeft
   * @param ll Reference to a go3Vector in which the coordinates are stored.
   */
  void	calculateLowerLeft  (go3Vector<volFloat>& ll);
  /*!
   * @see calculateUpperLeft
   * @param lr Reference to a go3Vector in which the coordinates are stored.
   */
  void	calculateLowerRight (go3Vector<volFloat>& lr);

  /*!
   * Scales all scalable values with factor. Call update() afterwards.
   * @param factor double Factor to multiplicate with.
   */
  void scale (volFloat factor)
  {
      position    *= factor;
      size        *= factor;
      eyeDistance *= factor;
  }

  void rotate (volFloat angle, Vol::GO_ROTATION_AXIS ax);
  	

 protected:
  /* View direction */
  go3Vector<volFloat> normal;
  go3Vector<volFloat> up;
  volFloat rotAngleX;
  volFloat rotAngleY;
  volFloat rotAngleZ;
  
  go3Vector<volFloat> position;
  go3Vector<volFloat> size;
  go3Vector<volFloat> screenSize;

  /* X and Y direction unit vectors on the view plane in world coordinates */
  go3Vector<volFloat> u, v; // These ALWAYS are of unit length.
  volFloat pixelWidth;		// Length of vector u in pixels
  volFloat pixelHeight;		// Length of vector v in pixels
  
  volFloat		eyeDistance;
  go3Vector<volFloat>	eyePos;

  /* View matrix: projecting world coordinates to plane coordinates */
  go44Matrix<volFloat> Tproj;
  
  
 private:

};

#endif
















