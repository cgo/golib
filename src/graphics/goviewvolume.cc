#include <goviewvolume.h>
#include <gotypes.h>
#include <goerror.h>
#include <gomath.h>  // MIN/MAX macros

goViewVolume::goViewVolume()
: goViewPlane()
{
    setClassName ("goViewVolume");
    setSampleDistanceX(1.0f);
    setSampleDistanceY(1.0f);
    setSampleDistanceZ(1.0f);
    setDepth (64);
    setFrontClip (0);
}

goViewVolume::goViewVolume (goViewPlane& p)
		: goViewPlane (p)
{
}

goViewVolume::goViewVolume (goViewVolume& p)
		: goViewPlane (p)
{
    setDepth (p.getDepth());
}

goViewVolume::~goViewVolume()
{
}

void
goViewVolume::calculateBounds (volFloat &minx, volFloat &maxx,
			       volFloat &miny, volFloat &maxy,
			       volFloat &minz, volFloat &maxz)
{
    volFloat l1;
    // volFloat l2;
    go3Vector<volFloat> ul;	// upper left corner on the view plane
    calculateUpperLeft (ul);

	goDouble frontf = frontClip / eyeDistance;    
	goDouble backf  = (frontClip + depth) / (volFloat)(frontClip);
    // goDouble l1_l = frontf;
	go3Vector<volFloat> v1;
    
    /*
     * Do upper left corners (front and rear)
     */
	 
    // cout << "upper left: " << ul.x << "," << ul.y << "," << ul.z << endl;
    
	v1 = ul;
    v1 -= eyePos;		// upper left - eye position
    l1 = v1.abs();		// absolute value => l1
    
    // volFloat l = l1 * l1_l;	// l = l1 * (d + e) / e
    
	// Use front clip plane point as upper left! 
    v1 *= frontf;    // v1 = vorderer Punkt
	ul = v1;
	ul *= backf;	 // ul = hinterer Punkt
    
    v1 += eyePos;	// v1 = coordinates of the rear upper left point
	ul += eyePos;
    // initialize bounds
    minx = MIN(v1.x, ul.x);
    maxx = MAX(v1.x, ul.x);
    miny = MIN(v1.y, ul.y);
    maxy = MAX(v1.y, ul.y);
    minz = MIN(v1.z, ul.z);
    maxz = MAX(v1.z, ul.z);

    
    /*
     * Do upper right corners (front and rear)
     */
    go3Vector<volFloat> ur;
    calculateUpperRight(ur);
    
	// cout << "upper right: " << ur.x << "," << ur.y << "," << ur.z << endl;    
    v1 = ur;
    v1 -= eyePos;
    // l1 = v1.abs();  // wird nicht mehr gebraucht!!!!
  	// Use front clip plane point as upper right! 
    v1 *= frontf;
	ur = v1;
	ur *= backf;
    v1 += eyePos;
	ur += eyePos;

    // update bounds
    minx = MIN(minx, ur.x);
    maxx = MAX(maxx, ur.x);
    miny = MIN(miny, ur.y);
    maxy = MAX(maxy, ur.y);
    minz = MIN(minz, ur.z);
    maxz = MAX(maxz, ur.z);

    minx = MIN(minx, v1.x);
    maxx = MAX(maxx, v1.x);
    miny = MIN(miny, v1.y);
    maxy = MAX(maxy, v1.y);
    minz = MIN(minz, v1.z);
    maxz = MAX(maxz, v1.z);


    /*
     * Do lower left corners (front and rear)
     */
    go3Vector<volFloat> ll;
    calculateLowerLeft(ll);
    // cout << "lower left: " << ll.x << "," << ll.y << "," << ll.z << endl;    
    v1 = ll;
    v1 -= eyePos;
    // l1 = v1.abs();  // wird nicht mehr gebraucht!!!!
   	// Use front clip plane point as lower left!
    v1 *= frontf;
	ll = v1;
	ll *= backf;
    v1 += eyePos;
	ll += eyePos;
	
    // update bounds
    minx = MIN(minx, ll.x);
    maxx = MAX(maxx, ll.x);
    miny = MIN(miny, ll.y);
    maxy = MAX(maxy, ll.y);
    minz = MIN(minz, ll.z);
    maxz = MAX(maxz, ll.z);

    minx = MIN(minx, v1.x);
    maxx = MAX(maxx, v1.x);
    miny = MIN(miny, v1.y);
    maxy = MAX(maxy, v1.y);
    minz = MIN(minz, v1.z);
    maxz = MAX(maxz, v1.z);

    /*
     * Do lower right corners (front and rear)
     */
    go3Vector<volFloat> lr;
    calculateLowerRight(lr);
    // cout << "lower left: " << lr.x << "," << lr.y << "," << lr.z << endl;    
    v1 = lr;
    v1 -= eyePos;
    // l1 = v1.abs();  // wird nicht mehr gebraucht!!!!
   	// Use front clip plane point as lower right! 
    v1 *= frontf;
	lr = v1;
	lr *= backf;
    v1 += eyePos;
	lr += eyePos;

    // update bounds
    minx = MIN(minx, lr.x);
    maxx = MAX(maxx, lr.x);
    miny = MIN(miny, lr.y);
    maxy = MAX(maxy, lr.y);
    minz = MIN(minz, lr.z);
    maxz = MAX(maxz, lr.z);

    minx = MIN(minx, v1.x);
    maxx = MAX(maxx, v1.x);
    miny = MIN(miny, v1.y);
    maxy = MAX(maxy, v1.y);
    minz = MIN(minz, v1.z);
    maxz = MAX(maxz, v1.z);


    goString s = getClassName();
#if _GODEBUG >= 3
    goError::note(s.toCharPtr(),"Bounds values:");
    cout << "\tX: " << minx << " -> " << maxx << endl;
    cout << "\tY: " << miny << " -> " << maxy << endl;
    cout << "\tZ: " << minz << " -> " << maxz << endl;
#endif
}

void
goViewVolume::operator= (goViewVolume& other)
{
    // cout << "goViewVolume::operator= called" << endl;
    setNormal (other.getNormal());
    setUp     (other.getUp());
    setEyePos (other.getEyePos());
    setEyeDistance (other.getEyeDistance());
    depth = other.getDepth();
    setPixelWidth (other.getPixelWidth());
    setPixelHeight (other.getPixelHeight());
    setScreenSize   (other.getScreenSize());
    setSampleDistanceX (other.getSampleDistanceX());
    setSampleDistanceY (other.getSampleDistanceY());
    setSampleDistanceZ (other.getSampleDistanceZ());
    setFrontClip (other.getFrontClip());
}
