/* tinvit.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int tinvit_(integer *nm, integer *n, real *d__, real *e, 
	real *e2, integer *m, real *w, integer *ind, real *z__, integer *ierr,
	 real *rv1, real *rv2, real *rv3, real *rv4, real *rv6)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2, i__3;
    real r__1, r__2, r__3, r__4;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer i__, j, p, q, r__, s;
    static real u, v, x0, x1;
    static integer ii, jj, ip;
    static real uk, xu;
    static integer tag, its;
    static real eps2, eps3, eps4, norm, order;
    static integer group;
    extern doublereal pythag_(real *, real *), epslon_(real *);



/*     this subroutine is a translation of the inverse iteration tech- */
/*     nique in the algol procedure tristurm by peters and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 418-439(1971). */

/*     this subroutine finds those eigenvectors of a tridiagonal */
/*     symmetric matrix corresponding to specified eigenvalues, */
/*     using inverse iteration. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        d contains the diagonal elements of the input matrix. */

/*        e contains the subdiagonal elements of the input matrix */
/*          in its last n-1 positions.  e(1) is arbitrary. */

/*        e2 contains the squares of the corresponding elements of e, */
/*          with zeros corresponding to negligible elements of e. */
/*          e(i) is considered negligible if it is not larger than */
/*          the product of the relative machine precision and the sum */
/*          of the magnitudes of d(i) and d(i-1).  e2(1) must contain */
/*          0.0e0 if the eigenvalues are in ascending order, or 2.0e0 */
/*          if the eigenvalues are in descending order.  if  bisect, */
/*          tridib, or  imtqlv  has been used to find the eigenvalues, */
/*          their output e2 array is exactly what is expected here. */

/*        m is the number of specified eigenvalues. */

/*        w contains the m eigenvalues in ascending or descending order. */

/*        ind contains in its first m positions the submatrix indices */
/*          associated with the corresponding eigenvalues in w -- */
/*          1 for eigenvalues belonging to the first submatrix from */
/*          the top, 2 for those belonging to the second submatrix, etc. */

/*     on output */

/*        all input arrays are unaltered. */

/*        z contains the associated set of orthonormal eigenvectors. */
/*          any vector which fails to converge is set to zero. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          -r         if the eigenvector corresponding to the r-th */
/*                     eigenvalue fails to converge in 5 iterations. */

/*        rv1, rv2, rv3, rv4, and rv6 are temporary storage arrays. */

/*     calls pythag for  sqrt(a*a + b*b) . */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --rv6;
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
    --ind;
    --w;

    /* Function Body */
    *ierr = 0;
    if (*m == 0) {
	goto L1001;
    }
    tag = 0;
    order = 1.f - e2[1];
    q = 0;
/*     .......... establish and process next submatrix .......... */
L100:
    p = q + 1;

    i__1 = *n;
    for (q = p; q <= i__1; ++q) {
	if (q == *n) {
	    goto L140;
	}
	if (e2[q + 1] == 0.f) {
	    goto L140;
	}
/* L120: */
    }
/*     .......... find vectors by inverse iteration .......... */
L140:
    ++tag;
    s = 0;

    i__1 = *m;
    for (r__ = 1; r__ <= i__1; ++r__) {
	if (ind[r__] != tag) {
	    goto L920;
	}
	its = 1;
	x1 = w[r__];
	if (s != 0) {
	    goto L510;
	}
/*     .......... check for isolated root .......... */
	xu = 1.f;
	if (p != q) {
	    goto L490;
	}
	rv6[p] = 1.f;
	goto L870;
L490:
	norm = (r__1 = d__[p], dabs(r__1));
	ip = p + 1;

	i__2 = q;
	for (i__ = ip; i__ <= i__2; ++i__) {
/* L500: */
/* Computing MAX */
	    r__3 = norm, r__4 = (r__1 = d__[i__], dabs(r__1)) + (r__2 = e[i__]
		    , dabs(r__2));
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
	s = p;
L505:
	group = 0;
	goto L520;
/*     .......... look for close or coincident roots .......... */
L510:
	if ((r__1 = x1 - x0, dabs(r__1)) >= eps2) {
	    goto L505;
	}
	++group;
	if (order * (x1 - x0) <= 0.f) {
	    x1 = x0 + order * eps3;
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
/*     .......... warning -- a divide check may occur here if */
/*                e2 array has not been specified correctly .......... */
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
	j = r__;

	i__2 = group;
	for (jj = 1; jj <= i__2; ++jj) {
L630:
	    --j;
	    if (ind[j] != tag) {
		goto L630;
	    }
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
	    goto L830;
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
/*     .......... set error -- non-converged eigenvector .......... */
L830:
	*ierr = -r__;
	xu = 0.f;
	goto L870;
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

L870:
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
L920:
	;
    }

    if (q < *n) {
	goto L100;
    }
L1001:
    return 0;
} /* tinvit_ */

