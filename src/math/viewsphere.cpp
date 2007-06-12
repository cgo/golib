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
bool goMath::sphereToEuclidean (goFloat phi, goFloat theta, goFloat radius,
                                goVectorf* positionRet, goVectorf* upRet)
{
    //= Spherical to cartesian coordinates
    goFloat sin_theta = ::sin(theta);

    //= Only rotation of phi from x towards y axis
    go3Vector<goFloat> v1 (radius * ::cos(phi), radius * ::sin(phi), 0.0f);
    //= Complete rotation, first phi towards y, then the result towards z axis
    go3Vector<goFloat> v2 (v1.x * sin_theta, v1.y * sin_theta, radius * ::cos(theta));
    //= Create up vector as cross product
    go3Vector<goFloat> up (v2);

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

bool goMath::sampleViewSphere (goFloat dist, goFloat radius,
                               goList<goVectorf>& positionRet, goList<goVectorf>& upRet)
{
    goVectorf pos(3);
    goDouble phi = 0;
    goDouble d_phi = dist / radius;
    while (phi < M_PI)
    {
        //= Only rotation of phi from x towards y axis -- needed for up-vector
        go3Vector<goFloat> v1 (radius * ::cos(phi), radius * ::sin(phi), 0.0f);

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
            go3Vector<goFloat> v2 (v1.x * ::sin(theta), v1.y * ::sin(theta), radius * ::cos(theta));
            //= Create up vector as cross product
            go3Vector<goFloat> up (v2);

            up.cross (v1);
            up *= 1.0f / up.abs();
            if (v2.z < 0.0f)
                up *= -1.0f;
            goVectorf temp(3);
            temp[0] = up.x; temp[1] = up.y; temp[2] = up.z;
            upRet.append(temp);

            theta = theta + d_theta;
        }
        phi = phi + d_phi;
    }

    return true;
}

bool goMath::sampleViewSphere (goFloat dist, goFloat radius,
                               goMatrix<goFloat>& viewSphereRet)
{
    goList<goVector<goFloat> > pl, ul;
    if (!goMath::sampleViewSphere(dist,radius,pl,ul))
    {
        goLog::warning ("sampleViewSphere() failed.");
        return false;
    }
    goSize_t N = pl.getSize();
    if (viewSphereRet.getRows() != N || viewSphereRet.getColumns() != 3)
    {
        viewSphereRet.resize(N,3);
    }
    goList<goVector<goFloat> >::Element* el = pl.getFrontElement();
    for (goSize_t i = 0; i < N && el; ++i, el = el->next)
    {
        viewSphereRet(i,0) = el->elem[0];
        viewSphereRet(i,1) = el->elem[1];
        viewSphereRet(i,2) = el->elem[2];
    }
    return true;
}

bool goMath::euclideanToSphere (const goVectorf& x, goFloat& phiRet, goFloat& thetaRet, goFloat& radiusRet)
{
    if (x.getSize() != 3)
        return false;

    radiusRet = x.norm2();
    thetaRet = ::atan2 (sqrt(x[0]*x[0] + x[1]*x[1]), x[2]);
    phiRet = ::atan2 (x[1], x[0]);

    return true;
}
