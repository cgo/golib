#ifndef GOVIEWVOLUME_H
#define GOVIEWVOLUME_H

#include <gotypes.h>
#include <go3vector.h>
#include <goviewplane.h>


/*!
 * Defines a visible volume.
 * Coordinates are in world coordinates.
 * @author Christian Gosch
 * @date 1.8.2001
 * @todo Define operations for this class as they are needed. (Development of
 * goVolumeNavigator will need some more functionality in here).
 */
class
goViewVolume : public goViewPlane
{
 public:
    goViewVolume();
    goViewVolume(goViewPlane& p);
    goViewVolume(goViewVolume& v);
    virtual ~goViewVolume();
    
    volFloat	getDepth() { return depth; }
    void       	setDepth (volFloat d) { depth = d; }
    void	setFrontClip (volFloat d) { frontClip = d; }
    volFloat	getFrontClip () { return frontClip; }
    
    /*! Calculates the bounds in world coordinates of the view volume and stores
	 *  them in the variables referenced by the parameters.
	 *  The values are representing a bounding box around the view frustum given by
	 *  the front clip distance, the eye distance and the view depth.
	 *	@param minx minimal x bound
	 *  @param maxx maximal x bound
	 *	@param miny minimal y bound
	 *	@param miny minimal y bound
	 *  @param maxz maximal z bound
	 *  @param maxz maximal z bound
	 */
    void       	calculateBounds (volFloat &minx, volFloat &maxx,
	       			 volFloat &miny, volFloat &maxy,
	       			 volFloat &minz, volFloat &maxz);

    void operator= (goViewVolume& other);

    /*!
     * Multiplies all scalable values with factor. Call
     * update() afterwards.
     * @param factor Double scale factor.
     * @see goViewPlane::scale()
     */
    void scale (volFloat factor)
    {
		eyePos		*= factor;
		size        *= factor;
		eyeDistance *= factor;
		frontClip   *= factor;
		depth       *= factor;
    }    
	
	void setSampleDistanceX (goFloat d) { sampleDistanceX = d; }
	void setSampleDistanceY (goFloat d) { sampleDistanceY = d; }
	void setSampleDistanceZ (goFloat d) { sampleDistanceZ = d; }
	
	goFloat getSampleDistanceX () { return sampleDistanceX; }
	goFloat getSampleDistanceY () { return sampleDistanceY; }
	goFloat getSampleDistanceZ () { return sampleDistanceZ; }
	
 private:
    /// Depth of the view volume, taken from the projection plane.
    volFloat depth;
    /// Depth from the eye position at which the rendering starts
    volFloat frontClip;
    goFloat  sampleDistanceX;
    goFloat  sampleDistanceY;
    goFloat  sampleDistanceZ;  
};



#endif
