/* ratqr.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* Subroutine */ int ratqr_(integer *n, real *eps1, real *d__, real *e, real *
	e2, integer *m, real *w, integer *ind, real *bd, logical *type__, 
	integer *idef, integer *ierr)
{
    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2, r__3;

    /* Local variables */
    static real f;
    static integer i__, j, k;
    static real p, q, r__, s;
    static integer k1, ii, jj;
    static real ep, qp, err, tot;
    static integer jdef;
    static real delta;
    extern doublereal epslon_(real *);



/*     this subroutine is a translation of the algol procedure ratqr, */
/*     num. math. 11, 264-272(1968) by reinsch and bauer. */
/*     handbook for auto. comp., vol.ii-linear algebra, 257-265(1971). */

/*     this subroutine finds the algebraically smallest or largest */
/*     eigenvalues of a symmetric tridiagonal matrix by the */
/*     rational qr method with newton corrections. */

/*     on input */

/*        n is the order of the matrix. */

/*        eps1 is a theoretical absolute error tolerance for the */
/*          computed eigenvalues.  if the input eps1 is non-positive, */
/*          or indeed smaller than its default value, it is reset */
/*          at each iteration to the respective default value, */
/*          namely, the product of the relative machine precision */
/*          and the magnitude of the current eigenvalue iterate. */
/*          the theoretical absolute error in the k-th eigenvalue */
/*          is usually not greater than k times eps1. */

/*        d contains the diagonal elements of the input matrix. */

/*        e contains the subdiagonal elements of the input matrix */
/*          in its last n-1 positions.  e(1) is arbitrary. */

/*        e2 contains the squares of the corresponding elements of e. */
/*          e2(1) is arbitrary. */

/*        m is the number of eigenvalues to be found. */

/*        idef should be set to 1 if the input matrix is known to be */
/*          positive definite, to -1 if the input matrix is known to */
/*          be negative definite, and to 0 otherwise. */

/*        type should be set to .true. if the smallest eigenvalues */
/*          are to be found, and to .false. if the largest eigenvalues */
/*          are to be found. */

/*     on output */

/*        eps1 is unaltered unless it has been reset to its */
/*          (last) default value. */

/*        d and e are unaltered (unless w overwrites d). */

/*        elements of e2, corresponding to elements of e regarded */
/*          as negligible, have been replaced by zero causing the */
/*          matrix to split into a direct sum of submatrices. */
/*          e2(1) is set to 0.0e0 if the smallest eigenvalues have been */
/*          found, and to 2.0e0 if the largest eigenvalues have been */
/*          found.  e2 is otherwise unaltered (unless overwritten by bd). */

/*        w contains the m algebraically smallest eigenvalues in */
/*          ascending order, or the m largest eigenvalues in */
/*          descending order.  if an error exit is made because of */
/*          an incorrect specification of idef, no eigenvalues */
/*          are found.  if the newton iterates for a particular */
/*          eigenvalue are not monotone, the best estimate obtained */
/*          is returned and ierr is set.  w may coincide with d. */

/*        ind contains in its first m positions the submatrix indices */
/*          associated with the corresponding eigenvalues in w -- */
/*          1 for eigenvalues belonging to the first submatrix from */
/*          the top, 2 for those belonging to the second submatrix, etc.. */

/*        bd contains refined bounds for the theoretical errors of the */
/*          corresponding eigenvalues in w.  these bounds are usually */
/*          within the tolerance specified by eps1.  bd may coincide */
/*          with e2. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          6*n+1      if  idef  is set to 1 and  type  to .true. */
/*                     when the matrix is not positive definite, or */
/*                     if  idef  is set to -1 and  type  to .false. */
/*                     when the matrix is not negative definite, */
/*          5*n+k      if successive iterates to the k-th eigenvalue */
/*                     are not monotone increasing, where k refers */
/*                     to the last such occurrence. */

/*     note that subroutine tridib is generally faster and more */
/*     accurate than ratqr if the eigenvalues are clustered. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --bd;
    --ind;
    --w;
    --e2;
    --e;
    --d__;

    /* Function Body */
    *ierr = 0;
    jdef = *idef;
/*     .......... copy d array into w .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L20: */
	w[i__] = d__[i__];
    }

    if (*type__) {
	goto L40;
    }
    j = 1;
    goto L400;
L40:
    err = 0.f;
    s = 0.f;
/*     .......... look for small sub-diagonal entries and define */
/*                initial shift from lower gerschgorin bound. */
/*                copy e2 array into bd .......... */
    tot = w[1];
    q = 0.f;
    j = 0;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	p = q;
	if (i__ == 1) {
	    goto L60;
	}
	r__3 = (r__1 = d__[i__], dabs(r__1)) + (r__2 = d__[i__ - 1], dabs(
		r__2));
	if (p > epslon_(&r__3)) {
	    goto L80;
	}
L60:
	e2[i__] = 0.f;
L80:
	bd[i__] = e2[i__];
/*     .......... count also if element of e2 has underflowed .......... */
	if (e2[i__] == 0.f) {
	    ++j;
	}
	ind[i__] = j;
	q = 0.f;
	if (i__ != *n) {
	    q = (r__1 = e[i__ + 1], dabs(r__1));
	}
/* Computing MIN */
	r__1 = w[i__] - p - q;
	tot = dmin(r__1,tot);
/* L100: */
    }

    if (jdef == 1 && tot < 0.f) {
	goto L140;
    }

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L110: */
	w[i__] -= tot;
    }

    goto L160;
L140:
    tot = 0.f;

L160:
    i__1 = *m;
    for (k = 1; k <= i__1; ++k) {
/*     .......... next qr transformation .......... */
L180:
	tot += s;
	delta = w[*n] - s;
	i__ = *n;
	f = (r__1 = epslon_(&tot), dabs(r__1));
	if (*eps1 < f) {
	    *eps1 = f;
	}
	if (delta > *eps1) {
	    goto L190;
	}
	if (delta < -(*eps1)) {
	    goto L1000;
	}
	goto L300;
/*     .......... replace small sub-diagonal squares by zero */
/*                to reduce the incidence of underflows .......... */
L190:
	if (k == *n) {
	    goto L210;
	}
	k1 = k + 1;
	i__2 = *n;
	for (j = k1; j <= i__2; ++j) {
	    r__2 = w[j] + w[j - 1];
/* Computing 2nd power */
	    r__1 = epslon_(&r__2);
	    if (bd[j] <= r__1 * r__1) {
		bd[j] = 0.f;
	    }
/* L200: */
	}

L210:
	f = bd[*n] / delta;
	qp = delta + f;
	p = 1.f;
	if (k == *n) {
	    goto L260;
	}
	k1 = *n - k;
/*     .......... for i=n-1 step -1 until k do -- .......... */
	i__2 = k1;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = *n - ii;
	    q = w[i__] - s - f;
	    r__ = q / qp;
	    p = p * r__ + 1.f;
	    ep = f * r__;
	    w[i__ + 1] = qp + ep;
	    delta = q - ep;
	    if (delta > *eps1) {
		goto L220;
	    }
	    if (delta < -(*eps1)) {
		goto L1000;
	    }
	    goto L300;
L220:
	    f = bd[i__] / q;
	    qp = delta + f;
	    bd[i__ + 1] = qp * ep;
/* L240: */
	}

L260:
	w[k] = qp;
	s = qp / p;
	if (tot + s > tot) {
	    goto L180;
	}
/*     .......... set error -- irregular end of iteration. */
/*                deflate minimum diagonal element .......... */
	*ierr = *n * 5 + k;
	s = 0.f;
	delta = qp;

	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    if (w[j] > delta) {
		goto L280;
	    }
	    i__ = j;
	    delta = w[j];
L280:
	    ;
	}
/*     .......... convergence .......... */
L300:
	if (i__ < *n) {
	    bd[i__ + 1] = bd[i__] * f / qp;
	}
	ii = ind[i__];
	if (i__ == k) {
	    goto L340;
	}
	k1 = i__ - k;
/*     .......... for j=i-1 step -1 until k do -- .......... */
	i__2 = k1;
	for (jj = 1; jj <= i__2; ++jj) {
	    j = i__ - jj;
	    w[j + 1] = w[j] - s;
	    bd[j + 1] = bd[j];
	    ind[j + 1] = ind[j];
/* L320: */
	}

L340:
	w[k] = tot;
	err += dabs(delta);
	bd[k] = err;
	ind[k] = ii;
/* L360: */
    }

    if (*type__) {
	goto L1001;
    }
    f = bd[1];
    e2[1] = 2.f;
    bd[1] = f;
    j = 2;
/*     .......... negate elements of w for largest values .......... */
L400:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L500: */
	w[i__] = -w[i__];
    }

    jdef = -jdef;
    switch (j) {
	case 1:  goto L40;
	case 2:  goto L1001;
    }
/*     .......... set error -- idef specified incorrectly .......... */
L1000:
    *ierr = *n * 6 + 1;
L1001:
    return 0;
} /* ratqr_ */

