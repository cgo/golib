#include <goviewplane.h>
#include <iostream>
#include <math.h>
#include <go44matrix.h>
#include <go4vector.h>
#include <go3vector.h>
#include <govol.h>
#include <goerror.h>

#include <goquaternion.h>

goViewPlane::goViewPlane () : goObjectBase() 
{
	setClassName ("goViewPlane");
	eyeDistance = 1.0f;
	normal.x = 0.0f;
	normal.y = 0.0f;
	normal.z = 1.0f;
	up.x = 0.0f;
	up.y = -1.0f;
	up.z = 0.0f;
	pixelWidth = 1.0f;
	pixelHeight = 1.0f;
	rotAngleX = 0.0f;
	rotAngleY = 0.0f;
	rotAngleZ = 0.0f;
}

goViewPlane::goViewPlane (goViewPlane& other) : goObjectBase()
{
	  setClassName ("goViewPlane");
	  eyeDistance = other.getEyeDistance();
	  normal = other.getNormal();
	  up = other.getUp();
	  eyePos  = other.getEyePos();
	  size = other.getSize();
	  update();
}

goViewPlane::~goViewPlane ()
{
}

void
goViewPlane::update() 
{
  // Unit vector in x direction on the viewplane
  u = normal;;
  u *= (up * normal) / (normal.abs());
  v = up;
  v -= u;
  v *= 1 / v.abs();
  
  u = normal;
  u.cross (v);
  v *= -1;
  // u *= pixelWidth / u.abs();
  
  // Unit vector in y direction on the viewplane
  // v *= -pixelHeight / v.abs();
  
  // Eye position
  position = getNormal();
  position *= getEyeDistance();
  position += getEyePos();
  //eyePos = getNormal();
  //eyePos *= -getEyeDistance();
  //eyePos += getPosition();
	
//  cout << getClassName() << "::update()" << endl;
//   cout << "eyePos: " << eyePos.x << "," << eyePos.y << "," << eyePos.z << endl;
//   cout << "normal: " << getNormal().x << "," << getNormal().y << "," << getNormal().z << endl;
//   cout << "u: " << u.x << "," << u.y << "," << u.z << endl;
//   cout << "v: " << v.x << "," << v.y << "," << v.z << endl;
//   cout << "planePos: " << getPosition().x << "," << getPosition().y << "," << getPosition().z << endl;

  // View matrix
  go44Matrix<volFloat> Tm ( 1, 0, 0, -eyePos.x,
			    0, 1, 0, -eyePos.y,
			    0, 0, 1, -eyePos.z,
			    0, 0, 0, 1 );
			   
  go44Matrix<volFloat> B ( u.x, v.x, normal.x, 0,
			   u.y, v.y, normal.y, 0,
			   u.z, v.z, normal.z, 1,
			   0,    0,    0,        1 );


  // Make B the base change matrix from the world coordinate system
  // to the plane coordinate system.
  B.transpose();
  go44Matrix<volFloat> tempM;
  tempM = B;

  tempM *= Tm;
  // Projection Matrix
  Tproj.fill (0.0f);
  Tproj.elem(0,0) = 1;
  Tproj.elem(1,1) = 1;
  Tproj.elem(2,2) = 1;
  Tproj.elem(3,2) = 1 / getEyeDistance();

  Tproj *= tempM;

  // cout << "View matrix: " << endl << Tproj << endl;
}


const go44Matrix<volFloat>&
goViewPlane::getViewMatrix() const
{
  return ((const go44Matrix<volFloat>&)Tproj);
}


/*
 * Calculate upper left corner where the first ray starts on the defined view plane.
 */
void
goViewPlane::calculateUpperLeft (go3Vector<volFloat>& ul)
{
    ul = position;
	
    go3Vector<volFloat> v1;
    v1 = u;
    v1 *= ((size.x) * 0.5);
    ul -= v1;
    v1 = v;
    v1 *= ((size.y) * 0.5);
    ul -= v1;
}

void
goViewPlane::calculateUpperRight (go3Vector<volFloat>& ur)
{
    ur = position;
	
    go3Vector<volFloat> v1;
    v1 = u;
    v1 *= ((size.x) * 0.5);
    ur += v1;
    v1 = v;
    v1 *= ((size.y) * 0.5);
    ur -= v1;
}

void
goViewPlane::calculateLowerLeft (go3Vector<volFloat>& ll)
{
    ll = position;
	
    go3Vector<volFloat> v1;
    v1 = u;
    v1 *= ((size.x) * 0.5);
    ll -= v1;
    v1 = v;
    v1 *= ((size.y) * 0.5);
    ll += v1;
}

void
goViewPlane::calculateLowerRight (go3Vector<volFloat>& lr)
{
    lr = position;
	
    go3Vector<volFloat> v1;
    v1 = u;
    v1 *= ((size.x) * 0.5);
    lr += v1;
    v1 = v;
    v1 *= ((size.y) * 0.5);
    lr += v1;
}



void
goViewPlane::operator= (goViewPlane& other) 
{  
    normal = other.getNormal();
    up = other.getUp();
    setEyePos (other.getEyePos());
    setEyeDistance (other.getEyeDistance());
    setPixelWidth (other.getPixelWidth());
    setPixelHeight (other.getPixelHeight());
    setScreenSize (other.getScreenSize());
}

void
goViewPlane::rotate(volFloat angle, Vol::GO_ROTATION_AXIS ax)
{
	go3Vector<volFloat> rotaxis;
	switch(ax)
	{
		case Vol::GO_ROTATION_X:
			rotaxis = u; break;
		case Vol::GO_ROTATION_Y:
			rotaxis = v; break;
		case Vol::GO_ROTATION_Z:
			rotaxis = normal; break;
	    default: rotaxis = u; break;			
	}
	//go3Vector<volFloat> trans;
	//trans = normal;
	//trans *= eyeDistance;
	//go3Vector<volFloat> temp;
	
	goQuaternion<volFloat> rot_quat, point_quat, temp_quat;
	rot_quat.scalar = cos(angle * 0.5);
	rotaxis 		*= sin(angle * 0.5);
	rot_quat.v      = rotaxis;
	//rot_quat.v     *= 1 / rot_quat.v.abs();
	point_quat.scalar = 0;
	point_quat.v      = normal;
	
	//point_quat.v	 += trans;
	
	temp_quat = rot_quat;
	temp_quat.conjugate();
	point_quat = rot_quat * point_quat * temp_quat;
	normal = point_quat.v; 
	//normal -= trans;
	//normal *= 1 / normal.abs();
	
	
	point_quat.scalar = 0;
	point_quat.v      = up;
	
	//point_quat.v	 += trans;
	
	temp_quat = rot_quat;
	temp_quat.conjugate();
	point_quat = rot_quat * point_quat * temp_quat;
	up = point_quat.v; 
	//up -= trans;
	//up *= 1 / up.abs();
	
	return;
#if 0	
    go44Matrix<volFloat>* Mr = 0;

    switch (ax)
	{
	case Vol::GO_ROTATION_X:
	    cout << getClassName() << " x rotation " << angle << endl;
	    if ( (rotAngleX + angle) > (M_PI * 2) )
		{
		    rotAngleX = rotAngleX + angle - M_PI * 2;
		} 
	    else
		{
		    if ( (rotAngleX + angle) < 0)
			{
			    rotAngleX = 2 * M_PI - (rotAngleX + angle);
			} else
			    {
				rotAngleX += angle;
			    }
		}
	    Mr = new go44Matrix<volFloat> (1, 0,	0, 0,
					   0, cos(rotAngleX), -sin(rotAngleX), 0,
					   0, sin(rotAngleX), cos(rotAngleX),  0,
					   0,		 0,	      0, 1);
	    rotAngleX = angle;
	    break;
	case Vol::GO_ROTATION_Y:
	    cout << getClassName() << " y rotation " << angle << endl;
	    if ( (rotAngleY + angle) > (M_PI * 2) )
		{
		    rotAngleY = rotAngleY + angle - M_PI * 2;
		} else
		    {
			if ( (rotAngleY + angle) < 0)
			    {
				rotAngleY = 2 * M_PI - (rotAngleY + angle);
			    } else
				{
				    rotAngleY += angle;
				}
		    }
	    Mr = new go44Matrix<volFloat> (cos(rotAngleY), 0, -sin(rotAngleY), 0,
					   0,          1,	0,     0,
					   sin(rotAngleY), 0, cos(rotAngleY),  0,
					   0,		 0,	      0, 1);
	    rotAngleY = angle;
	    break;
	case Vol::GO_ROTATION_Z:
	    cout << getClassName() << " z rotation " << angle << endl;
	    if ( (rotAngleZ + angle) > (M_PI * 2) )
		{
		    rotAngleZ = rotAngleZ + angle - M_PI * 2;
		} else
		    {
			if ( (rotAngleZ + angle) < 0)
			    {
				rotAngleZ = 2 * M_PI - (rotAngleZ + angle);
			    } else
				{
				    rotAngleZ += angle;
				}
		    }
	    Mr = new go44Matrix<volFloat> (cos(rotAngleZ), -sin(rotAngleZ), 0, 0,
					   sin(rotAngleZ), cos(rotAngleZ),  0, 0,
					   0,		 0,	      1, 0,
					   0,		 0,	      0, 1);
	    rotAngleZ = angle;
	    break;
	default: 
	    goError::print("goViewPlane::rotate()","Unknown axis");
	    return;
	    break;
	}

    cout << getClassName() << "Angles: " << rotAngleX << "," << rotAngleY << "," << rotAngleZ << endl;

	// Basiswechsel
    go44Matrix<volFloat> B (u.x, u.y, u.z, 0,
			    v.x, v.y, v.z, 0,
			    normal.x, normal.y, normal.z, 0,
			    0, 0, 0, 1);
    go44Matrix<volFloat> B_;
    B_ = B;
    B_.transpose();

    go4Vector<volFloat> temp;
    go3Vector<volFloat> temp3;

    /* Rotate normal */
    temp.x = normal.x;
    temp.y = normal.y;
    temp.z = normal.z;
    temp.t = 1;
    // temp *= B;
    temp *= *Mr;	// temp = Mr * temp;
    // temp *= B_;
    // temp.div();
    temp3.x = temp.x;
    temp3.y = temp.y;
    temp3.z = temp.z;
    normal = temp3;
    normal *= 1 / normal.abs();
    
    /* Rotate up vector */
    temp.x = up.x;
    temp.y = up.y;
    temp.z = up.z;
    temp.t = 1;
    // temp *= B;
    temp *= *Mr;		// temp = Mr * temp;
    // temp *= B_;
    // temp.div();
    temp3.x = temp.x;
    temp3.y = temp.y;
    temp3.z = temp.z;
    up = temp3;
    up *= 1 / up.abs();
    delete Mr;
#endif
}










