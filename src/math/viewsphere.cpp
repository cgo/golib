/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gomath.h>
#include <gomatrix.h>
#include <golist.h>
#include <govector.h>
#include <go3vector.h>

/** 
 * @brief Convert from spherical coordinates to euclidean coordinates.
 * 
 * @param phi      in [-pi,pi]
 * @param theta    in [0,2*pi]
 * @param radius   Radius
 * @param positionRet  3D position, return value
 * @param upRet        3D up-vector, return value
 * 
 * @return True if successful, false otherwise.
 */
template <class T>
bool goMath::sphereToEuclidean (T phi, T theta, T radius,
                                goMath::Vector<T>* positionRet, goMath::Vector<T>* upRet)
{
    //= Spherical to cartesian coordinates
    T sin_theta = ::sin(theta);

    //= Only rotation of phi from x towards y axis
    go3Vector<T> v1 (radius * ::cos(phi), radius * ::sin(phi), 0.0f);
    //= Complete rotation, first phi towards y, then the result towards z axis
    go3Vector<T> v2 (v1.x * sin_theta, v1.y * sin_theta, radius * ::cos(theta));
    //= Create up vector as cross product
    go3Vector<T> up (v2);

    up.cross (v1);
    up *= 1.0f / up.abs();
    if (v2.z < 0.0f)
        up *= -1.0f;

    if (positionRet)
    {
        positionRet->resize (3);
        (*positionRet)[0] = v2.x;
        (*positionRet)[1] = v2.y;
        (*positionRet)[2] = v2.z;
    }
    if (upRet)
    {
        upRet->resize (3);
        (*upRet)[0] = up.x;
        (*upRet)[1] = up.y;
        (*upRet)[2] = up.z;
    }
    return true;
}

template <class T>
bool goMath::sampleSphere (T dist, T radius, goList<goMath::Vector<T> >& positionRet, goList<goMath::Vector<T> >& upRet)
{
    goMath::Vector<T> pos(3);
    goDouble phi = 0;
    goDouble d_phi = dist / radius;
    while (phi < M_PI)
    {
        //= Only rotation of phi from x towards y axis -- needed for up-vector
        go3Vector<T> v1 (radius * ::cos(phi), radius * ::sin(phi), 0.0f);

        goDouble r2 = radius * ::cos(phi);
        goDouble rho = sqrt(radius*radius - r2*r2);
        pos[0] = r2;
        goDouble theta = 0.0f;
        goDouble d_theta = dist / rho;
        while (theta < 2*M_PI)
        {
            pos[1] = rho * ::sin(theta);
            pos[2] = rho * ::cos(theta);
            positionRet.append(pos);

            //= Compute an up-vector:
            //= Complete rotation, first phi towards y, then the result towards z axis
            go3Vector<T> v2 (v1.x * ::sin(theta), v1.y * ::sin(theta), radius * ::cos(theta));
            //= Create up vector as cross product
            go3Vector<T> up (v2);

            up.cross (v1);
            up *= 1.0f / up.abs();
            if (v2.z < 0.0f)
                up *= -1.0f;
            goMath::Vector<T> temp(3);
            temp[0] = up.x; temp[1] = up.y; temp[2] = up.z;
            upRet.append(temp);

            theta = theta + d_theta;
        }
        phi = phi + d_phi;
    }

    return true;
}

template <class T>
bool goMath::sampleSphere (T dist, T radius, goMath::Matrix<T>& viewSphereRet)
{
    goList<goMath::Vector<T> > pl, ul;
    if (!goMath::sampleSphere<T> (dist,radius,pl,ul))
    {
        goLog::warning ("sampleViewSphere() failed.");
        return false;
    }
    goSize_t N = pl.getSize();
    if (viewSphereRet.getRows() != N || viewSphereRet.getColumns() != 3)
    {
        viewSphereRet.resize(N,3);
    }
    typename goList<goMath::Vector<T> >::Element* el = pl.getFrontElement();
    for (goSize_t i = 0; i < N && el; ++i, el = el->next)
    {
        viewSphereRet(i,0) = el->elem[0];
        viewSphereRet(i,1) = el->elem[1];
        viewSphereRet(i,2) = el->elem[2];
    }
    return true;
}

template <class T>
bool goMath::euclideanToSphere (const goMath::Vector<T>& x, T& phiRet, T& thetaRet, T& radiusRet)
{
    if (x.getSize() != 3)
        return false;

    radiusRet = x.norm2();
    thetaRet = ::atan2 (sqrt(x[0]*x[0] + x[1]*x[1]), x[2]);
    phiRet = ::atan2 (x[1], x[0]);

    return true;
}

template <class T>
bool goMath::euclideanToSphere (const goMath::Vector<T>& x, goMath::Vector<T>& phitheta, T* radiusRet)
{
    if (x.getSize() != 3)
        return false;
    if (phitheta.getSize() != 2)
        phitheta.resize(2);

    if (radiusRet)
        *radiusRet = x.norm2();
    phitheta[1] = ::atan2 (sqrt(x[0]*x[0] + x[1]*x[1]), x[2]); // theta
    phitheta[0] = ::atan2 (x[1], x[0]);                        // phi

    return true;
}

template 
bool goMath::sphereToEuclidean<goFloat> (goFloat phi, goFloat theta, goFloat radius,
                                goMath::Vector<goFloat>* positionRet, goMath::Vector<goFloat>* upRet);
template 
bool goMath::sphereToEuclidean<goDouble> (goDouble phi, goDouble theta, goDouble radius,
                                goMath::Vector<goDouble>* positionRet, goMath::Vector<goDouble>* upRet);
template
bool goMath::euclideanToSphere<goFloat> (const goMath::Vector<goFloat>& , goFloat& , goFloat& , goFloat&);
template
bool goMath::euclideanToSphere<goDouble> (const goMath::Vector<goDouble>& , goDouble& , goDouble& , goDouble&);
template
bool goMath::euclideanToSphere<goFloat> (const goMath::Vector<goFloat>&, goMath::Vector<goFloat>&, goFloat*);
template
bool goMath::euclideanToSphere<goDouble> (const goMath::Vector<goDouble>&, goMath::Vector<goDouble>&, goDouble*);

template 
bool goMath::sampleSphere<goFloat> (goFloat dist, goFloat radius, goMath::Matrix<goFloat>& viewSphereRet);
template 
bool goMath::sampleSphere<goDouble> (goDouble dist, goDouble radius, goMath::Matrix<goDouble>& viewSphereRet);

template 
bool goMath::sampleSphere<goFloat> (goFloat, goFloat, goList<goMath::Vector<goFloat> >&, goList<goMath::Vector<goFloat> >&);
template 
bool goMath::sampleSphere<goDouble> (goDouble, goDouble, goList<goMath::Vector<goDouble> >&, goList<goMath::Vector<goDouble> >&);
