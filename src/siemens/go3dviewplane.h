#ifndef GO3DVIEWPLANE_H
#define GO3DVIEWPLANE_H

#include <gotypes.h>
#include <go3dinfo.h>
#include <go3vector.h>

/*!
 * Storage class for the parameters describing a view plane.
 * @author Christian Gosch
 */
class
go3DViewPlane {
 public:
  go3DViewPlane () 
    {
      eyeDistance = 1.0f;
      normal.x = 0.5f;
      normal.y = 0.5f;
      normal.z = 0.5f;
      up.x = 1;
      up.y = -1;
      up.z = 0;
    }
  virtual ~go3DViewPlane () { }

  go3DInfo& getInfo () { return info; }
  const go3DInfo& getInfo () const { return info; }
  void setInfo (go3DInfo& i) { info = i; }

  /*!
   * Size only has a meaning if you want it to.
   * You could, for example, use the size of the screen to calculate some size of your plane.
   * Or, you might just want to set it to one. Or whatever.
   */
  inline void setSize (goDouble x, goDouble y) 
    {
      info.size.x = x;
      info.size.y = y;
    }
  inline void setPosition (goDouble x, goDouble y, goDouble z) 
    {
      info.position.x = x;
      info.position.y = y;
      info.position.z = z;
    }
  inline go3Vector<goDouble>& getPosition ()
    {
      return info.position;
    }
  inline go3Vector<goDouble>& getNormal () { return normal; }
  inline const go3Vector<goDouble>& getNormal () const { return normal; }
  void setNormal (go3Vector<goDouble>& v) { normal = v; }
  inline go3Vector<goDouble>& getUp () { return up; }
  inline const go3Vector<goDouble>& getUp () const { return up; }
  void setUp (go3Vector<goDouble>& u) { up = u; }
  void setEyeDistance (goDouble f) { eyeDistance = f; }
  
  goDouble getEyeDistance () const { return eyeDistance; }

  inline void operator= (go3DViewPlane& other) {
    normal = other.getNormal();
    up = other.getUp();
    setInfo (other.getInfo());
    setEyeDistance (other.getEyeDistance());
  }
  
 protected:
  /* View direction */
  go3Vector<goDouble> normal;
  go3Vector<goDouble> up;

  go3DInfo	info;

  goDouble eyeDistance;
  
};

#endif





