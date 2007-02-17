#include <gosvd.h>
#include <gomath.h>

#ifndef GOLAPACK_H
# include <golapack.h>
#endif

#ifdef min
# undef min
#endif

template <class T>
goMath::SVD<T>::SVD (const goMatrix<T>& A, bool thin)
: U(), V(), s(), myM(0), myN(0)
{
    this->calculate (A, thin);
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
bool goMath::SVD<goFloat>::calculate (const goMatrix<goFloat>& A, bool thin)
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
    if (thin)
    {
        //= sgesvd_ expects column major, so the dimensions are transposed.
        this->U.resize (goMath::min(m,n),m);
        // this->V.resize (n,goMath::min(m,n));
        V.resize (n,n);
    }
    else
    {
        U.resize (m,m);
        V.resize (n,n);
    }
    integer ldu = U.getLeadingDimension();
    integer ldv = V.getLeadingDimension();
    goFixedArray<goFloat> work (max(3 * min(m,n) + max(m,n), 5 * min(m,n)));
    integer lwork = work.getSize ();
    integer info = -1;
    char job = thin ? 'S' : 'A';
    char jobvt = thin ? 'A' : 'A'; //= all n rows of V in any case.
    sgesvd_ (&job, &jobvt, &m, &n, AT.getPtr(), &lda, s.getPtr(), U.getPtr(), &ldu,
            V.getPtr(), &ldv, work.getPtr(), &lwork, &info);
    //= V now contains V^T in column-major, so it's V in row-major.
    //= We need to transpose U in order to get the correct row-major U.
    U.transpose ();
    if (info != 0)
        return false;
    return true;
};

template <>
bool goMath::SVD<goDouble>::calculate (const goMatrix<goDouble>& A, bool thin)
{
    goMatrix<goDouble> AT;
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
    if (thin)
    {
        //= dgesvd_ expects column major, so the dimensions are transposed.
        U.resize (goMath::min(m,n),m);
        V.resize (n,goMath::min(m,n));
    }
    else
    {
        U.resize (m,m);
        V.resize (n,n);
    }
    integer ldu = U.getLeadingDimension();
    integer ldv = V.getLeadingDimension();
    goFixedArray<goDouble> work (max(3 * min(m,n) + max(m,n), 5 * min(m,n)));
    integer lwork = work.getSize ();
    integer info = -1;
    char job = thin ? 'S' : 'A';
    char jobvt = thin ? 'S' : 'A';
    dgesvd_ (&job, &jobvt, &m, &n, AT.getPtr(), &lda, s.getPtr(), U.getPtr(), &ldu,
            V.getPtr(), &ldv, work.getPtr(), &lwork, &info);
    //= V now contains V^T in column-major, so it's V in row-major.
    //= We need to transpose U in order to get the correct row-major U.
    U.transpose ();
    if (info != 0)
        return false;
    return true;
};

template <class T>
bool goMath::SVD<T>::calculate (const goMatrix<T>&, bool)
{
    return false;
}

//=============================================================================

template <class Real>
goMath::ThinSVD<Real>::ThinSVD (const goMatrix<Real> &Arg)
    :  U(), V(), s(), m(0), n(0)
{
    m = Arg.dim1();
    n = Arg.dim2();
    int nu = min(m,n);
    s.resize(min(m+1,n)); 
    U.resize(m,nu); // = goMatrix<Real>(m, nu);
    U.fill (Real(0));
    V.resize (n,n); // = goMatrix<Real>(n,n);
    goArray<Real> e(n);
    goArray<Real> work(m);
    goMatrix<Real> A(Arg.copy());
    int wantu = 1;  					/* boolean */
    int wantv = 1;  					/* boolean */
    int i=0, j=0, k=0;

    // Reduce A to bidiagonal form, storing the diagonal elements
    // in s and the super-diagonal elements in e.

    int nct = min(m-1,n);
    int nrt = max(0,min(n-2,m));
    for (k = 0; k < max(nct,nrt); k++) {
        if (k < nct) {

            // Compute the transformation for the k-th column and
            // place the k-th diagonal in s[k].
            // Compute 2-norm of k-th column without under/overflow.
            s[k] = 0;
            for (i = k; i < m; i++) {
                s[k] = hypot(s[k],A(i,k));
            }
            if (s[k] != 0.0) {
                if (A(k,k) < 0.0) {
                    s[k] = -s[k];
                }
                for (i = k; i < m; i++) {
                    A(i,k) /= s[k];
                }
                A(k,k) += 1.0;
            }
            s[k] = -s[k];
        }
        for (j = k+1; j < n; j++) {
            if ((k < nct) && (s[k] != 0.0))  {

                // Apply the transformation.

                double t = 0;
                for (i = k; i < m; i++) {
                    t += A(i,k)*A(i,j);
                }
                t = -t/A(k,k);
                for (i = k; i < m; i++) {
                    A(i,j) += t*A(i,k);
                }
            }

            // Place the k-th row of A into e for the
            // subsequent calculation of the row transformation.

            e[j] = A(k,j);
        }
        if (wantu & (k < nct)) {

            // Place the transformation in U for subsequent back
            // multiplication.

            for (i = k; i < m; i++) {
                U(i,k) = A(i,k);
            }
        }
        if (k < nrt) {

            // Compute the k-th row transformation and place the
            // k-th super-diagonal in e[k].
            // Compute 2-norm without under/overflow.
            e[k] = 0;
            for (i = k+1; i < n; i++) {
                e[k] = hypot(e[k],e[i]);
            }
            if (e[k] != 0.0) {
                if (e[k+1] < 0.0) {
                    e[k] = -e[k];
                }
                for (i = k+1; i < n; i++) {
                    e[i] /= e[k];
                }
                e[k+1] += 1.0;
            }
            e[k] = -e[k];
            if ((k+1 < m) & (e[k] != 0.0)) {

                // Apply the transformation.

                for (i = k+1; i < m; i++) {
                    work[i] = 0.0;
                }
                for (j = k+1; j < n; j++) {
                    for (i = k+1; i < m; i++) {
                        work[i] += e[j]*A(i,j);
                    }
                }
                for (j = k+1; j < n; j++) {
                    double t = -e[j]/e[k+1];
                    for (i = k+1; i < m; i++) {
                        A(i,j) += t*work[i];
                    }
                }
            }
            if (wantv) {

                // Place the transformation in V for subsequent
                // back multiplication.

                for (i = k+1; i < n; i++) {
                    V(i,k) = e[i];
                }
            }
        }
    }

    // Set up the final bidiagonal matrix or order p.

    int p = min(n,m+1);
    if (nct < n) {
        s[nct] = A(nct,nct);
    }
    if (m < p) {
        s[p-1] = 0.0;
    }
    if (nrt+1 < p) {
        e[nrt] = A(nrt,p-1);
    }
    e[p-1] = 0.0;

    // If required, generate U.

    if (wantu) {
        for (j = nct; j < nu; j++) {
            for (i = 0; i < m; i++) {
                U(i,j) = 0.0;
            }
            U(j,j) = 1.0;
        }
        for (k = nct-1; k >= 0; k--) {
            if (s[k] != 0.0) {
                for (j = k+1; j < nu; j++) {
                    double t = 0;
                    for (i = k; i < m; i++) {
                        t += U(i,k)*U(i,j);
                    }
                    t = -t/U(k,k);
                    for (i = k; i < m; i++) {
                        U(i,j) += t*U(i,k);
                    }
                }
                for (i = k; i < m; i++ ) {
                    U(i,k) = -U(i,k);
                }
                U(k,k) = 1.0 + U(k,k);
                for (i = 0; i < k-1; i++) {
                    U(i,k) = 0.0;
                }
            } else {
                for (i = 0; i < m; i++) {
                    U(i,k) = 0.0;
                }
                U(k,k) = 1.0;
            }
        }
    }

    // If required, generate V.

    if (wantv) {
        for (k = n-1; k >= 0; k--) {
            if ((k < nrt) & (e[k] != 0.0)) {
                for (j = k+1; j < nu; j++) {
                    double t = 0;
                    for (i = k+1; i < n; i++) {
                        t += V(i,k)*V(i,j);
                    }
                    t = -t/V(k+1,k);
                    for (i = k+1; i < n; i++) {
                        V(i,j) += t*V(i,k);
                    }
                }
            }
            for (i = 0; i < n; i++) {
                V(i,k) = 0.0;
            }
            V(k,k) = 1.0;
        }
    }

    // Main iteration loop for the singular values.

    int pp = p-1;
    int iter = 0;
    double eps = pow(2.0,-52.0);
    while (p > 0) {
        int k=0;
        int kase=0;

        // Here is where a test for too many iterations would go.

        // This section of the program inspects for
        // negligible elements in the s and e arrays.  On
        // completion the variables kase and k are set as follows.

        // kase = 1     if s(p) and e[k-1] are negligible and k<p
        // kase = 2     if s(k) is negligible and k<p
        // kase = 3     if e[k-1] is negligible, k<p, and
        //              s(k), ..., s(p) are not negligible (qr step).
        // kase = 4     if e(p-1) is negligible (convergence).

        for (k = p-2; k >= -1; k--) {
            if (k == -1) {
                break;
            }
            if (abs(e[k]) <= eps*(abs(s[k]) + abs(s[k+1]))) {
                e[k] = 0.0;
                break;
            }
        }
        if (k == p-2) {
            kase = 4;
        } else {
            int ks;
            for (ks = p-1; ks >= k; ks--) {
                if (ks == k) {
                    break;
                }
                double t = (ks != p ? abs(e[ks]) : 0.) + 
                    (ks != k+1 ? abs(e[ks-1]) : 0.);
                if (abs(s[ks]) <= eps*t)  {
                    s[ks] = 0.0;
                    break;
                }
            }
            if (ks == k) {
                kase = 3;
            } else if (ks == p-1) {
                kase = 1;
            } else {
                kase = 2;
                k = ks;
            }
        }
        k++;

        // Perform the task indicated by kase.

        switch (kase) {

            // Deflate negligible s(p).

            case 1: {
                        double f = e[p-2];
                        e[p-2] = 0.0;
                        for (j = p-2; j >= k; j--) {
                            double t = hypot((double)s[j],f);
                            double cs = s[j]/t;
                            double sn = f/t;
                            s[j] = t;
                            if (j != k) {
                                f = -sn*e[j-1];
                                e[j-1] = cs*e[j-1];
                            }
                            if (wantv) {
                                for (i = 0; i < n; i++) {
                                    t = cs*V(i,j) + sn*V(i,p-1);
                                    V(i,p-1) = -sn*V(i,j) + cs*V(i,p-1);
                                    V(i,j) = t;
                                }
                            }
                        }
                    }
                    break;

                    // Split at negligible s(k).

            case 2: {
                        double f = e[k-1];
                        e[k-1] = 0.0;
                        for (j = k; j < p; j++) {
                            double t = hypot((double)s[j],f);
                            double cs = s[j]/t;
                            double sn = f/t;
                            s[j] = t;
                            f = -sn*e[j];
                            e[j] = cs*e[j];
                            if (wantu) {
                                for (i = 0; i < m; i++) {
                                    t = cs*U(i,j) + sn*U(i,k-1);
                                    U(i,k-1) = -sn*U(i,j) + cs*U(i,k-1);
                                    U(i,j) = t;
                                }
                            }
                        }
                    }
                    break;

                    // Perform one qr step.

            case 3: {

                        // Calculate the shift.

                        double scale = max(max(max(max(
                                            abs(s[p-1]),abs(s[p-2])),abs(e[p-2])), 
                                    abs(s[k])),abs(e[k]));
                        double sp = s[p-1]/scale;
                        double spm1 = s[p-2]/scale;
                        double epm1 = e[p-2]/scale;
                        double sk = s[k]/scale;
                        double ek = e[k]/scale;
                        double b = ((spm1 + sp)*(spm1 - sp) + epm1*epm1)/2.0;
                        double c = (sp*epm1)*(sp*epm1);
                        double shift = 0.0;
                        if ((b != 0.0) || (c != 0.0)) {
                            shift = sqrt(b*b + c);
                            if (b < 0.0) {
                                shift = -shift;
                            }
                            shift = c/(b + shift);
                        }
                        double f = (sk + sp)*(sk - sp) + shift;
                        double g = sk*ek;

                        // Chase zeros.

                        for (j = k; j < p-1; j++) {
                            double t = hypot(f,g);
                            double cs = f/t;
                            double sn = g/t;
                            if (j != k) {
                                e[j-1] = t;
                            }
                            f = cs*s[j] + sn*e[j];
                            e[j] = cs*e[j] - sn*s[j];
                            g = sn*s[j+1];
                            s[j+1] = cs*s[j+1];
                            if (wantv) {
                                for (i = 0; i < n; i++) {
                                    t = cs*V(i,j) + sn*V(i,j+1);
                                    V(i,j+1) = -sn*V(i,j) + cs*V(i,j+1);
                                    V(i,j) = t;
                                }
                            }
                            t = hypot(f,g);
                            cs = f/t;
                            sn = g/t;
                            s[j] = t;
                            f = cs*e[j] + sn*s[j+1];
                            s[j+1] = -sn*e[j] + cs*s[j+1];
                            g = sn*e[j+1];
                            e[j+1] = cs*e[j+1];
                            if (wantu && (j < m-1)) {
                                for (i = 0; i < m; i++) {
                                    t = cs*U(i,j) + sn*U(i,j+1);
                                    U(i,j+1) = -sn*U(i,j) + cs*U(i,j+1);
                                    U(i,j) = t;
                                }
                            }
                        }
                        e[p-2] = f;
                        iter = iter + 1;
                    }
                    break;

                    // Convergence.

            case 4: {

                        // Make the singular values positive.

                        if (s[k] <= 0.0) {
                            s[k] = (s[k] < 0.0 ? -s[k] : 0.0);
                            if (wantv) {
                                for (i = 0; i <= pp; i++) {
                                    V(i,k) = -V(i,k);
                                }
                            }
                        }

                        // Order the singular values.

                        while (k < pp) {
                            if (s[k] >= s[k+1]) {
                                break;
                            }
                            double t = s[k];
                            s[k] = s[k+1];
                            s[k+1] = t;
                            if (wantv && (k < n-1)) {
                                for (i = 0; i < n; i++) {
                                    t = V(i,k+1); V(i,k+1) = V(i,k); V(i,k) = t;
                                }
                            }
                            if (wantu && (k < m-1)) {
                                for (i = 0; i < m; i++) {
                                    t = U(i,k+1); U(i,k+1) = U(i,k); U(i,k) = t;
                                }
                            }
                            k++;
                        }
                        iter = 0;
                        p--;
                    }
                    break;
        }
    }
}

template <class Real>
goMath::ThinSVD<Real>::~ThinSVD ()
{
}

template <class Real>
void goMath::ThinSVD<Real>::getU (goMatrix<Real> &A) 
{
    int minm = min(m+1,n);

    A = goMatrix<Real>(m, minm);

    for (int i=0; i<m; i++)
        for (int j=0; j<minm; j++)
            A(i,j) = U(i,j);
}

template <class Real>
void goMath::ThinSVD<Real>::getV (goMatrix<Real> &A) 
{
    A = V;
}

template <class Real>
goMatrix<Real>& goMath::ThinSVD<Real>::getU ()
{
    return this->U;
}

template <class Real>
const goMatrix<Real>& goMath::ThinSVD<Real>::getU () const
{
    return this->U;
}

template <class Real>
goMatrix<Real>& goMath::ThinSVD<Real>::getV ()
{
    return this->V;
}

template <class Real>
const goMatrix<Real>& goMath::ThinSVD<Real>::getV () const
{
    return this->V;
}

   /** Return the one-dimensional array of singular values */

template <class Real>
void goMath::ThinSVD<Real>::getSingularValues (goVector<Real> &x) 
{
    x = s;
}

template <class Real>
goVector<Real>& goMath::ThinSVD<Real>::getSingularValues ()
{
    return this->s;
}

template <class Real>
const goVector<Real>& goMath::ThinSVD<Real>::getSingularValues () const
{
    return this->s;
}

   /** Return the diagonal matrix of singular values
   @return     S
   */

template <class Real>
void goMath::ThinSVD<Real>::getS (goMatrix<Real> &A) 
{
    A = goMatrix<Real>(n,n);
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            A(i,j) = 0.0;
        }
        A(i,i) = s[i];
    }
}

   /** Two norm  (max(S)) */

template <class Real>
double goMath::ThinSVD<Real>::norm2 () 
{
    return s[0];
}

   /** Two norm of condition number (max(S)/min(S)) */

template <class Real>
double goMath::ThinSVD<Real>::cond () 
{
    return s[0]/s[min(m,n)-1];
}

   /** Effective numerical matrix rank
   @return     Number of nonnegligible singular values.
   */

template <class Real>
int goMath::ThinSVD<Real>::rank () 
{
    double eps = pow(2.0,-52.0);
    double tol = max(m,n)*s[0]*eps;
    int r = 0;
    int sz = s.getSize ();
    for (int i = 0; i < sz; i++) {
        if (s[i] > tol) {
            r++;
        }
    }
    return r;
}

template class goMath::SVD<goFloat>;
template class goMath::SVD<goDouble>;
template class goMath::ThinSVD<goFloat>;
template class goMath::ThinSVD<goDouble>;
