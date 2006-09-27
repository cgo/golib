/* tsturm.f -- translated by f2c (version 20050501).
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

/* Table of constant values */

static real c_b26 = 1.f;

/* Subroutine */ int tsturm_(integer *nm, integer *n, real *eps1, real *d__, 
	real *e, real *e2, real *lb, real *ub, integer *mm, integer *m, real *
	w, real *z__, integer *ierr, real *rv1, real *rv2, real *rv3, real *
	rv4, real *rv5, real *rv6)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2, i__3;
    real r__1, r__2, r__3, r__4;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer i__, j, k, p, q, r__, s;
    static real u, v;
    static integer m1, m2;
    static real t1, t2, x0, x1;
    static integer ii, jj, ip;
    static real uk, xu;
    static integer its;
    static real eps2, eps3, eps4, tst1, tst2, norm;
    static integer group;
    extern doublereal pythag_(real *, real *), epslon_(real *);
    static integer isturm;



/*     this subroutine is a translation of the algol procedure tristurm */
/*     by peters and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 418-439(1971). */

/*     this subroutine finds those eigenvalues of a tridiagonal */
/*     symmetric matrix which lie in a specified interval and their */
/*     associated eigenvectors, using bisection and inverse iteration. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        eps1 is an absolute error tolerance for the computed */
/*          eigenvalues.  it should be chosen commensurate with */
/*          relative perturbations in the matrix elements of the */
/*          order of the relative machine precision.  if the */
/*          input eps1 is non-positive, it is reset for each */
/*          submatrix to a default value, namely, minus the */
/*          product of the relative machine precision and the */
/*          1-norm of the submatrix. */

/*        d contains the diagonal elements of the input matrix. */

/*        e contains the subdiagonal elements of the input matrix */
/*          in its last n-1 positions.  e(1) is arbitrary. */

/*        e2 contains the squares of the corresponding elements of e. */
/*          e2(1) is arbitrary. */

/*        lb and ub define the interval to be searched for eigenvalues. */
/*          if lb is not less than ub, no eigenvalues will be found. */

/*        mm should be set to an upper bound for the number of */
/*          eigenvalues in the interval.  warning. if more than */
/*          mm eigenvalues are determined to lie in the interval, */
/*          an error return is made with no values or vectors found. */

/*     on output */

/*        eps1 is unaltered unless it has been reset to its */
/*          (last) default value. */

/*        d and e are unaltered. */

/*        elements of e2, corresponding to elements of e regarded */
/*          as negligible, have been replaced by zero causing the */
/*          matrix to split into a direct sum of submatrices. */
/*          e2(1) is also set to zero. */

/*        m is the number of eigenvalues determined to lie in (lb,ub). */

/*        w contains the m eigenvalues in ascending order if the matrix */
/*          does not split.  if the matrix splits, the eigenvalues are */
/*          in ascending order for each submatrix.  if a vector error */
/*          exit is made, w contains those values already found. */

/*        z contains the associated set of orthonormal eigenvectors. */
/*          if an error exit is made, z contains those vectors */
/*          already found. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          3*n+1      if m exceeds mm. */
/*          4*n+r      if the eigenvector corresponding to the r-th */
/*                     eigenvalue fails to converge in 5 iterations. */

/*        rv1, rv2, rv3, rv4, rv5, and rv6 are temporary storage arrays. */

/*     the algol procedure sturmcnt contained in tristurm */
/*     appears in tsturm in-line. */

/*     calls pythag for  sqrt(a*a + b*b) . */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --rv6;
    --rv5;
    --rv4;
    --rv3;
    --rv2;
    --rv1;
    --e2;
    --e;
    --d__;
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --w;

    /* Function Body */
    *ierr = 0;
    t1 = *lb;
    t2 = *ub;
/*     .......... look for small sub-diagonal entries .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ == 1) {
	    goto L20;
	}
	tst1 = (r__1 = d__[i__], dabs(r__1)) + (r__2 = d__[i__ - 1], dabs(
		r__2));
	tst2 = tst1 + (r__1 = e[i__], dabs(r__1));
	if (tst2 > tst1) {
	    goto L40;
	}
L20:
	e2[i__] = 0.f;
L40:
	;
    }
/*     .......... determine the number of eigenvalues */
/*                in the interval .......... */
    p = 1;
    q = *n;
    x1 = *ub;
    isturm = 1;
    goto L320;
L60:
    *m = s;
    x1 = *lb;
    isturm = 2;
    goto L320;
L80:
    *m -= s;
    if (*m > *mm) {
	goto L980;
    }
    q = 0;
    r__ = 0;
/*     .......... establish and process next submatrix, refining */
/*                interval by the gerschgorin bounds .......... */
L100:
    if (r__ == *m) {
	goto L1001;
    }
    p = q + 1;
    xu = d__[p];
    x0 = d__[p];
    u = 0.f;

    i__1 = *n;
    for (q = p; q <= i__1; ++q) {
	x1 = u;
	u = 0.f;
	v = 0.f;
	if (q == *n) {
	    goto L110;
	}
	u = (r__1 = e[q + 1], dabs(r__1));
	v = e2[q + 1];
L110:
/* Computing MIN */
	r__1 = d__[q] - (x1 + u);
	xu = dmin(r__1,xu);
/* Computing MAX */
	r__1 = d__[q] + (x1 + u);
	x0 = dmax(r__1,x0);
	if (v == 0.f) {
	    goto L140;
	}
/* L120: */
    }

L140:
/* Computing MAX */
    r__2 = dabs(xu), r__3 = dabs(x0);
    r__1 = dmax(r__2,r__3);
    x1 = epslon_(&r__1);
    if (*eps1 <= 0.f) {
	*eps1 = -x1;
    }
    if (p != q) {
	goto L180;
    }
/*     .......... check for isolated root within interval .......... */
    if (t1 > d__[p] || d__[p] >= t2) {
	goto L940;
    }
    ++r__;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L160: */
	z__[i__ + r__ * z_dim1] = 0.f;
    }

    w[r__] = d__[p];
    z__[p + r__ * z_dim1] = 1.f;
    goto L940;
L180:
    u = (real) (q - p + 1);
    x1 = u * x1;
/* Computing MAX */
    r__1 = t1, r__2 = xu - x1;
    *lb = dmax(r__1,r__2);
/* Computing MIN */
    r__1 = t2, r__2 = x0 + x1;
    *ub = dmin(r__1,r__2);
    x1 = *lb;
    isturm = 3;
    goto L320;
L200:
    m1 = s + 1;
    x1 = *ub;
    isturm = 4;
    goto L320;
L220:
    m2 = s;
    if (m1 > m2) {
	goto L940;
    }
/*     .......... find roots by bisection .......... */
    x0 = *ub;
    isturm = 5;

    i__1 = m2;
    for (i__ = m1; i__ <= i__1; ++i__) {
	rv5[i__] = *ub;
	rv4[i__] = *lb;
/* L240: */
    }
/*     .......... loop for k-th eigenvalue */
/*                for k=m2 step -1 until m1 do -- */
/*                (-do- not used to legalize -computed go to-) .......... */
    k = m2;
L250:
    xu = *lb;
/*     .......... for i=k step -1 until m1 do -- .......... */
    i__1 = k;
    for (ii = m1; ii <= i__1; ++ii) {
	i__ = m1 + k - ii;
	if (xu >= rv4[i__]) {
	    goto L260;
	}
	xu = rv4[i__];
	goto L280;
L260:
	;
    }

L280:
    if (x0 > rv5[k]) {
	x0 = rv5[k];
    }
/*     .......... next bisection step .......... */
L300:
    x1 = (xu + x0) * .5f;
    if (x0 - xu <= dabs(*eps1)) {
	goto L420;
    }
    tst1 = (dabs(xu) + dabs(x0)) * 2.f;
    tst2 = tst1 + (x0 - xu);
    if (tst2 == tst1) {
	goto L420;
    }
/*     .......... in-line procedure for sturm sequence .......... */
L320:
    s = p - 1;
    u = 1.f;

    i__1 = q;
    for (i__ = p; i__ <= i__1; ++i__) {
	if (u != 0.f) {
	    goto L325;
	}
	v = (r__1 = e[i__], dabs(r__1)) / epslon_(&c_b26);
	if (e2[i__] == 0.f) {
	    v = 0.f;
	}
	goto L330;
L325:
	v = e2[i__] / u;
L330:
	u = d__[i__] - x1 - v;
	if (u < 0.f) {
	    ++s;
	}
/* L340: */
    }

    switch (isturm) {
	case 1:  goto L60;
	case 2:  goto L80;
	case 3:  goto L200;
	case 4:  goto L220;
	case 5:  goto L360;
    }
/*     .......... refine intervals .......... */
L360:
    if (s >= k) {
	goto L400;
    }
    xu = x1;
    if (s >= m1) {
	goto L380;
    }
    rv4[m1] = x1;
    goto L300;
L380:
    rv4[s + 1] = x1;
    if (rv5[s] > x1) {
	rv5[s] = x1;
    }
    goto L300;
L400:
    x0 = x1;
    goto L300;
/*     .......... k-th eigenvalue found .......... */
L420:
    rv5[k] = x1;
    --k;
    if (k >= m1) {
	goto L250;
    }
/*     .......... find vectors by inverse iteration .......... */
    norm = (r__1 = d__[p], dabs(r__1));
    ip = p + 1;

    i__1 = q;
    for (i__ = ip; i__ <= i__1; ++i__) {
/* L500: */
/* Computing MAX */
	r__3 = norm, r__4 = (r__1 = d__[i__], dabs(r__1)) + (r__2 = e[i__], 
		dabs(r__2));
	norm = dmax(r__3,r__4);
    }
/*     .......... eps2 is the criterion for grouping, */
/*                eps3 replaces zero pivots and equal */
/*                roots are modified by eps3, */
/*                eps4 is taken very small to avoid overflow .......... */
    eps2 = norm * .001f;
    eps3 = epslon_(&norm);
    uk = (real) (q - p + 1);
    eps4 = uk * eps3;
    uk = eps4 / sqrt(uk);
    group = 0;
    s = p;

    i__1 = m2;
    for (k = m1; k <= i__1; ++k) {
	++r__;
	its = 1;
	w[r__] = rv5[k];
	x1 = rv5[k];
/*     .......... look for close or coincident roots .......... */
	if (k == m1) {
	    goto L520;
	}
	if (x1 - x0 >= eps2) {
	    group = -1;
	}
	++group;
	if (x1 <= x0) {
	    x1 = x0 + eps3;
	}
/*     .......... elimination with interchanges and */
/*                initialization of vector .......... */
L520:
	v = 0.f;

	i__2 = q;
	for (i__ = p; i__ <= i__2; ++i__) {
	    rv6[i__] = uk;
	    if (i__ == p) {
		goto L560;
	    }
	    if ((r__1 = e[i__], dabs(r__1)) < dabs(u)) {
		goto L540;
	    }
	    xu = u / e[i__];
	    rv4[i__] = xu;
	    rv1[i__ - 1] = e[i__];
	    rv2[i__ - 1] = d__[i__] - x1;
	    rv3[i__ - 1] = 0.f;
	    if (i__ != q) {
		rv3[i__ - 1] = e[i__ + 1];
	    }
	    u = v - xu * rv2[i__ - 1];
	    v = -xu * rv3[i__ - 1];
	    goto L580;
L540:
	    xu = e[i__] / u;
	    rv4[i__] = xu;
	    rv1[i__ - 1] = u;
	    rv2[i__ - 1] = v;
	    rv3[i__ - 1] = 0.f;
L560:
	    u = d__[i__] - x1 - xu * v;
	    if (i__ != q) {
		v = e[i__ + 1];
	    }
L580:
	    ;
	}

	if (u == 0.f) {
	    u = eps3;
	}
	rv1[q] = u;
	rv2[q] = 0.f;
	rv3[q] = 0.f;
/*     .......... back substitution */
/*                for i=q step -1 until p do -- .......... */
L600:
	i__2 = q;
	for (ii = p; ii <= i__2; ++ii) {
	    i__ = p + q - ii;
	    rv6[i__] = (rv6[i__] - u * rv2[i__] - v * rv3[i__]) / rv1[i__];
	    v = u;
	    u = rv6[i__];
/* L620: */
	}
/*     .......... orthogonalize with respect to previous */
/*                members of group .......... */
	if (group == 0) {
	    goto L700;
	}

	i__2 = group;
	for (jj = 1; jj <= i__2; ++jj) {
	    j = r__ - group - 1 + jj;
	    xu = 0.f;

	    i__3 = q;
	    for (i__ = p; i__ <= i__3; ++i__) {
/* L640: */
		xu += rv6[i__] * z__[i__ + j * z_dim1];
	    }

	    i__3 = q;
	    for (i__ = p; i__ <= i__3; ++i__) {
/* L660: */
		rv6[i__] -= xu * z__[i__ + j * z_dim1];
	    }

/* L680: */
	}

L700:
	norm = 0.f;

	i__2 = q;
	for (i__ = p; i__ <= i__2; ++i__) {
/* L720: */
	    norm += (r__1 = rv6[i__], dabs(r__1));
	}

	if (norm >= 1.f) {
	    goto L840;
	}
/*     .......... forward substitution .......... */
	if (its == 5) {
	    goto L960;
	}
	if (norm != 0.f) {
	    goto L740;
	}
	rv6[s] = eps4;
	++s;
	if (s > q) {
	    s = p;
	}
	goto L780;
L740:
	xu = eps4 / norm;

	i__2 = q;
	for (i__ = p; i__ <= i__2; ++i__) {
/* L760: */
	    rv6[i__] *= xu;
	}
/*     .......... elimination operations on next vector */
/*                iterate .......... */
L780:
	i__2 = q;
	for (i__ = ip; i__ <= i__2; ++i__) {
	    u = rv6[i__];
/*     .......... if rv1(i-1) .eq. e(i), a row interchange */
/*                was performed earlier in the */
/*                triangularization process .......... */
	    if (rv1[i__ - 1] != e[i__]) {
		goto L800;
	    }
	    u = rv6[i__ - 1];
	    rv6[i__ - 1] = rv6[i__];
L800:
	    rv6[i__] = u - rv4[i__] * rv6[i__ - 1];
/* L820: */
	}

	++its;
	goto L600;
/*     .......... normalize so that sum of squares is */
/*                1 and expand to full order .......... */
L840:
	u = 0.f;

	i__2 = q;
	for (i__ = p; i__ <= i__2; ++i__) {
/* L860: */
	    u = pythag_(&u, &rv6[i__]);
	}

	xu = 1.f / u;

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L880: */
	    z__[i__ + r__ * z_dim1] = 0.f;
	}

	i__2 = q;
	for (i__ = p; i__ <= i__2; ++i__) {
/* L900: */
	    z__[i__ + r__ * z_dim1] = rv6[i__] * xu;
	}

	x0 = x1;
/* L920: */
    }

L940:
    if (q < *n) {
	goto L100;
    }
    goto L1001;
/*     .......... set error -- non-converged eigenvector .......... */
L960:
    *ierr = (*n << 2) + r__;
    goto L1001;
/*     .......... set error -- underestimate of number of */
/*                eigenvalues in interval .......... */
L980:
    *ierr = *n * 3 + 1;
L1001:
    *lb = t1;
    *ub = t2;
    return 0;
} /* tsturm_ */

