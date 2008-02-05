#include <gomath.h>
#include <gomatrix.h>
#include <govector.h>

template <class element_type, class tangent_type>
goMath::Manifold<element_type,tangent_type>::Manifold ()
{
}

template <class element_type, class tangent_type>
goMath::Manifold<element_type,tangent_type>::~Manifold ()
{
}

template <class element_type, class tangent_type>
goDouble goMath::Manifold<element_type,tangent_type>::d (const Element& e1, const Element& e2)
{
    Tangent v;
    this->log (e1, e2, v);
    return ::sqrt (this->innerProduct (e1,v,v));
}

template <class T>
goMath::SO3<T>::SO3 ()
    : Manifold<goMatrix<T>, goVector<T> > ()
      , myId (3,3)
{
    myId.setIdentity();
}

template <class T>
goMath::SO3<T>::~SO3 ()
{
}

template <class T>
static void tangentMatrix (T x, T y, T z, goMatrix<T>& ret)
{
    if (ret.getRows() != 3 || ret.getColumns() != 3)
    {
        ret.resize (3,3);
    }
    ret(0,0) = T(0);
    ret(1,1) = T(0);
    ret(2,2) = T(0);
    ret(0,1) = -z; ret(0,2) = y;
    ret(1,0) = z; ret(1,2) = -x;
    ret(2,0) = -y; ret(2,1) = x;
}

template <class T>
static void tangentVector (const goMatrix<T>& S, goVector<T>& ret)
{
    if (ret.getSize() < 3)
    {
        ret.resize (3);
    }
    ret[0] = S(2,1); ret[1] = S(0,2); ret[2] = S(1,0);
}

template <class T>
void goMath::SO3<T>::exp (const Element& e, const Tangent& v, Element& ret)
{
    Element Sn;
    Tangent n (v);
    T theta = n.norm2();
    if (theta != T(0))
    {
        n *= 1. / theta;
        tangentMatrix (n[0],n[1],n[2],Sn);
    }
    else
    {
        tangentMatrix (T(0),T(0),T(0),Sn);
    }
    // ret = (myId + Sn * sin(theta) + (Sn*Sn) * (1 - cos(theta))) * e;
    goMatrix<T> ret2 = Sn;
    ret2 *= ::sin(theta);
    ret2 += myId;
    goMatrixMult<T> (T(1) - ::cos(theta), Sn, false, Sn, false, T(1), ret2);
    goMatrixMult<T> (T(1), ret2, false, e, false, T(0), ret);
}

/** 
 * @brief Log map.
 *
 * May not be optimal w.r.t. computation.
 * 
 * @param e1 
 * @param e2 
 * @param ret 
 */
template <class T>
void goMath::SO3<T>::log (const Element& e1, const Element& e2, Tangent& ret)
{
    goMatrix<T> R (3,3);
    goMatrix<T> Sr (3,3);
    goMatrixMult<T> (T(1), e2, false, e1, true, T(0), R);
    T theta = acos ( (R.trace() - T(1)) * T(0.5) );
    //=
    //= Handle special cases where theta close to 0 or pi:
    //=
    if (fabs(theta) < 1e-2)
    {
        //= Taylor approximation
        Sr = (R - R.getTranspose()) * (0.5 * (1.0 + theta*theta / 6.0));
        tangentVector (Sr, ret);
        return;
    }
    else if (fabs(M_PI - theta) < 1e-2)
    {
        //=
        //= From Pennec's notes on rotations:
        //=
        // T n[] = {0.0, 0.0, 0.0};
        T rho = 1.0 / (1.0 - ::cos(theta));
        T eps[] = {1.0, 1.0, 1.0}; // Sign of the entries
        Tangent w;
        tangentVector (R - R.getTranspose(), w);
        goIndex_t max_k = 0;
        T max_abs = fabs (w[0]);
        for (goIndex_t k = 1; k < 3; ++k)
        {
            if (max_abs < fabs(w[k]))
            {
                max_k = k;
                max_abs = fabs(w[k]);
            }
        }
        if ( (R(0,max_k) + R(max_k,0)) * w[max_k] < T(0) )
        {
            if (w[max_k] < T(0))
            {
                eps[0] = T(-1);
            }
        }
        for (goIndex_t k = 1; k < 3; ++k)
        {
            if ( (R(0,k) + R(k,0)) < T(0) )
            {
                eps[k] = eps[0] * T(-1);
            }
            else
            {
                eps[k] = eps[0];
            }
        }
        ret.resize (3);
        for (goIndex_t i = 0; i < 3; ++i)
        {
            T temp = eps[i] * ::sqrt (goMath::max<goDouble>(1.0 + rho * (R(i,i) - 1.0), 0.0));  //= Use max(,) to prevent numerical error to produce 
                                                                                                //= values < 0 which lead to NaN.
            ret[i] = temp * theta;
        }
        return;
    }
    Sr = (R - R.getTranspose()) * (theta / (2.0 * ::sin(theta)));
    tangentVector (Sr, ret);
}

template <class T>
goDouble goMath::SO3<T>::innerProduct (const Element& e, const Tangent& v1, const Tangent& v2)
{
    return v1 * v2;
}

template <class T>
void goMath::SO3<T>::matrix (const goVector<T>& w, goMatrix<T>& ret)
{
    this->exp (myId, w, ret);
}

template <class T>
void goMath::SO3<T>::vector (const goMatrix<T>& r, goVector<T>& ret)
{
    this->log (myId, r, ret);
}

template class goMath::SO3<goFloat>;
template class goMath::SO3<goDouble>;
