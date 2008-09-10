#include <gomath.h>
#include <gocubicspline.h>

template<class T>
goMath::CubicSpline<T>::CubicSpline ()
   : myA_inv(0,0), myM(4,4)
{
    static T A_inv[] = {T(2), T(-3), T(0), T(1),
                        T(-2), T(3), T(0), T(0),
                        T(1), T(-2), T(1), T(0),
                        T(1), T(-1), T(0), T(0)};
    myA_inv.setData (A_inv, 4, 4, 4);
}

template<class T>
goMath::CubicSpline<T>::CubicSpline (const goMath::Matrix<T>& points)
   : myA_inv(0,0), myM(4,4)
{
    static T A_inv[] = {T(2), T(-3), T(0), T(1),
                        T(-2), T(3), T(0), T(0),
                        T(1), T(-2), T(1), T(0),
                        T(1), T(-1), T(0), T(0)};
    myA_inv.setData (A_inv, 4, 4, 4);
    this->fit (points);
}

template<class T>
goMath::CubicSpline<T>::CubicSpline (const Vector<T>& pm1,
        const Vector<T>& p0,
        const Vector<T>& p1,
        const Vector<T>& p2)
   : myA_inv(0,0), myM(4,4)
{
    static T A_inv[] = {T(2), T(-3), T(0), T(1),
                        T(-2), T(3), T(0), T(0),
                        T(1), T(-2), T(1), T(0),
                        T(1), T(-1), T(0), T(0)};
    myA_inv.setData (A_inv, 4, 4, 4);
    this->fit (pm1, p0, p1, p2);
}

template<class T>
goMath::CubicSpline<T>::~CubicSpline ()
{
}

template<class T>
goAutoPtr<goMath::Vector<T> > 
goMath::CubicSpline<T>::operator() (T t) const
{
    goAutoPtr<Vector<T> > ret = goAutoPtr<Vector<T> >(new Vector<T>(2));
    this->eval (t, *ret);
    return ret;
}

template<class T>
bool goMath::CubicSpline<T>::eval (T t, Vector<T>& ret) const
{
    if (ret.getSize() != 2)
        ret.resize(2);
   
    //= Calculate explicitly to save some operations.
    ret[0] = t * (t * (t * this->myM(0,0) + this->myM(0,1)) + this->myM(0,2)) + this->myM(0,3);
    ret[1] = t * (t * (t * this->myM(1,0) + this->myM(1,1)) + this->myM(1,2)) + this->myM(1,3);

    return true;
}

template<class T>
bool goMath::CubicSpline<T>::D (T t, Vector<T>& ret) const
{
    if (ret.getSize() != 2)
        ret.resize(2);
   
    //= Calculate explicitly to save some operations.
    ret[0] = t * (t * 3.0 * this->myM(0,0) + 2.0 * this->myM(0,1)) + this->myM(0,2);
    ret[1] = t * (t * 3.0 * this->myM(1,0) + 2.0 * this->myM(1,1)) + this->myM(1,2);

    return true;
}

template<class T>
bool goMath::CubicSpline<T>::fit (const goMath::Matrix<T>& points)
{
    Vector<T> pm1, p0, p1, p2;
    points.refRow(0,pm1);
    points.refRow(1,p0);
    points.refRow(2,p1);
    points.refRow(3,p2);
    return this->fit (pm1, p0, p1, p2);
}

template<class T>
bool goMath::CubicSpline<T>::fit (const Vector<T>& pm1,
        const Vector<T>& p0,
        const Vector<T>& p1,
        const Vector<T>& p2)
{
    static goMath::Matrix<T> P (2,4);
    P.setColumn(0,p0);
    P.setColumn(1,p1);
    P.setColumn(2,(p1-pm1)*0.5);
    P.setColumn(3,(p2-p0)*0.5);
    
    goMath::matrixMult<T> (T(1), P, false, this->myA_inv, false, T(0), this->myM);
    return true;
}

/** 
 * @brief Integrate the function interpolated by this object.
 * 
 * This makes only sense when the interpolating spline represents a 
 * one dimensional function.
 *
 * @param t1 Start point of integration
 * @param t2 End point of integration
 * 
 * @return \f$ \int_{t_1}^{t_2} f(x) \, dx\f$
 */
template <class T>
T goMath::CubicSpline<T>::integrate (T t1, T t2)
{
    T a = this->myM(1,0); 
    T b = this->myM(1,1); 
    T c = this->myM(1,2); 
    T d = this->myM(1,3);
    T t2_p2 = t2 * t2;
    T t2_p3 = t2_p2 * t2;
    T t2_p4 = t2_p3 * t2;
    T t1_p2 = t1 * t1;
    T t1_p3 = t1_p2 * t1;
    T t1_p4 = t1_p3 * t1;
    return (3.0*a*t2_p4+4.0*b*t2_p3+6.0*c*t2_p2+12.0*d*t2-3.0*a*t1_p4-4.0*b*t1_p3-6.0*c*t1_p2-12.0*d*t1)/12.0;
}

template class goMath::CubicSpline<goFloat>;
template class goMath::CubicSpline<goDouble>;
