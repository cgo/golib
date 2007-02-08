#include <gosvd.h>

#ifndef GOLAPACK_H
# include <golapack.h>
#endif

template <class T>
goMath::SVD<T>::SVD (const goMatrix<T>& A)
: U(), V(), s(), myM(0), myN(0)
{
    this->calculate (A);
}

template <class T>
goMath::SVD<T>::~SVD () 
{
}

template <class T>
goMatrix<T>& goMath::SVD<T>::getU () { return this->U; };

template <class T>
const goMatrix<T>& goMath::SVD<T>::getU () const { return this->U; };

template <class T>
goMatrix<T>& goMath::SVD<T>::getV () { return this->V; };

template <class T>
const goMatrix<T>& goMath::SVD<T>::getV () const { return this->V; };

template <class T>
goVector<T>& goMath::SVD<T>::getSingularValues () { return this->s; };

template <class T>
const goVector<T>& goMath::SVD<T>::getSingularValues () const { return this->s; };

template <class T>
void goMath::SVD<T>::getS (goMatrix<T>& S) const 
{
    S.resize (myM, myN);
    S.fill (T(0));
    goSize_t sz = min (myM,myN);
    for (goSize_t i = 0; i < sz; ++i)
    {
        S(i,i) = this->s[i];
    }
};

template <>
bool goMath::SVD<goFloat>::calculate (const goMatrix<goFloat>& A)
{
    goMatrix<goFloat> AT;
    A.getTranspose (AT);
    //= Careful: sgesvd_ expects column major arrays, we use row major.
    //= So we here calculate svd(A), because the f2c-generated code thinks
    //= AT = A.
    integer m = AT.getColumns();  //= rows in row-major
    integer n = AT.getRows();     //= columns in row-major
    myM = m;
    myN = n;
    integer lda = AT.getLeadingDimension();
    s.resize (goMath::min<integer>(m,n));
    U.resize (m,m);
    V.resize (n,n);
    integer ldu = U.getLeadingDimension();
    integer ldv = V.getLeadingDimension();
    goFixedArray<goFloat> work (max(3 * min(m,n) + max(m,n), 5 * min(m,n)));
    integer lwork = work.getSize ();
    integer info = -1;
    char job = 'A';
    char jobvt = 'A';
    sgesvd_ (&job, &jobvt, &m, &n, AT.getPtr(), &lda, s.getPtr(), U.getPtr(), &ldu,
            V.getPtr(), &ldv, work.getPtr(), &lwork, &info);
    //= V now contains V^T in column-major, so it's V in row-major.
    //= We need to transpose U in order to get the correct row-major U.
    U.transpose ();
    if (info != 0)
        return false;
    return true;
};

template <class T>
bool goMath::SVD<T>::calculate (const goMatrix<T>& A)
{
    return false;
}

template class goMath::SVD<goFloat>;
template class goMath::SVD<goDouble>;
