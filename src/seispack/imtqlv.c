/* imtqlv.f -- translated by f2c (version 20050501).
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

static real c_b11 = 1.f;

/* Subroutine */ int imtqlv_(integer *n, real *d__, real *e, real *e2, real *
	w, integer *ind, integer *ierr, real *rv1)
{
    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2;

    /* Builtin functions */
    double r_sign(real *, real *);

    /* Local variables */
    static real b, c__, f, g;
    static integer i__, j, k, l, m;
    static real p, r__, s;
    static integer ii, tag, mml;
    static real tst1, tst2;
    extern doublereal pythag_(real *, real *);



/*     this subroutine is a variant of  imtql1  which is a translation of */
/*     algol procedure imtql1, num. math. 12, 377-383(1968) by martin and */
/*     wilkinson, as modified in num. math. 15, 450(1970) by dubrulle. */
/*     handbook for auto. comp., vol.ii-linear algebra, 241-248(1971). */

/*     this subroutine finds the eigenvalues of a symmetric tridiagonal */
/*     matrix by the implicit ql method and associates with them */
/*     their corresponding submatrix indices. */

/*     on input */

/*        n is the order of the matrix. */

/*        d contains the diagonal elements of the input matrix. */

/*        e contains the subdiagonal elements of the input matrix */
/*          in its last n-1 positions.  e(1) is arbitrary. */

/*        e2 contains the squares of the corresponding elements of e. */
/*          e2(1) is arbitrary. */

/*     on output */

/*        d and e are unaltered. */

/*        elements of e2, corresponding to elements of e regarded */
/*          as negligible, have been replaced by zero causing the */
/*          matrix to split into a direct sum of submatrices. */
/*          e2(1) is also set to zero. */

/*        w contains the eigenvalues in ascending order.  if an */
/*          error exit is made, the eigenvalues are correct and */
/*          ordered for indices 1,2,...ierr-1, but may not be */
/*          the smallest eigenvalues. */

/*        ind contains the submatrix indices associated with the */
/*          corresponding eigenvalues in w -- 1 for eigenvalues */
/*          belonging to the first submatrix from the top, */
/*          2 for those belonging to the second submatrix, etc.. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          j          if the j-th eigenvalue has not been */
/*                     determined after 30 iterations. */

/*        rv1 is a temporary storage array. */

/*     calls pythag for  sqrt(a*a + b*b) . */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --rv1;
    --ind;
    --w;
    --e2;
    --e;
    --d__;

    /* Function Body */
    *ierr = 0;
    k = 0;
    tag = 0;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	w[i__] = d__[i__];
	if (i__ != 1) {
	    rv1[i__ - 1] = e[i__];
	}
/* L100: */
    }

    e2[1] = 0.f;
    rv1[*n] = 0.f;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
/*     .......... look for small sub-diagonal element .......... */
L105:
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    if (m == *n) {
		goto L120;
	    }
	    tst1 = (r__1 = w[m], dabs(r__1)) + (r__2 = w[m + 1], dabs(r__2));
	    tst2 = tst1 + (r__1 = rv1[m], dabs(r__1));
	    if (tst2 == tst1) {
		goto L120;
	    }
/*     .......... guard against underflowed element of e2 .......... */
	    if (e2[m + 1] == 0.f) {
		goto L125;
	    }
/* L110: */
	}

L120:
	if (m <= k) {
	    goto L130;
	}
	if (m != *n) {
	    e2[m + 1] = 0.f;
	}
L125:
	k = m;
	++tag;
L130:
	p = w[l];
	if (m == l) {
	    goto L215;
	}
	if (j == 30) {
	    goto L1000;
	}
	++j;
/*     .......... form shift .......... */
	g = (w[l + 1] - p) / (rv1[l] * 2.f);
	r__ = pythag_(&g, &c_b11);
	g = w[m] - p + rv1[l] / (g + r_sign(&r__, &g));
	s = 1.f;
	c__ = 1.f;
	p = 0.f;
	mml = m - l;
/*     .......... for i=m-1 step -1 until l do -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = m - ii;
	    f = s * rv1[i__];
	    b = c__ * rv1[i__];
	    r__ = pythag_(&f, &g);
	    rv1[i__ + 1] = r__;
	    if (r__ == 0.f) {
		goto L210;
	    }
	    s = f / r__;
	    c__ = g / r__;
	    g = w[i__ + 1] - p;
	    r__ = (w[i__] - g) * s + c__ * 2.f * b;
	    p = s * r__;
	    w[i__ + 1] = g + p;
	    g = c__ * r__ - b;
/* L200: */
	}

	w[l] -= p;
	rv1[l] = g;
	rv1[m] = 0.f;
	goto L105;
/*     .......... recover from underflow .......... */
L210:
	w[i__ + 1] -= p;
	rv1[m] = 0.f;
	goto L105;
/*     .......... order eigenvalues .......... */
L215:
	if (l == 1) {
	    goto L250;
	}
/*     .......... for i=l step -1 until 2 do -- .......... */
	i__2 = l;
	for (ii = 2; ii <= i__2; ++ii) {
	    i__ = l + 2 - ii;
	    if (p >= w[i__ - 1]) {
		goto L270;
	    }
	    w[i__] = w[i__ - 1];
	    ind[i__] = ind[i__ - 1];
/* L230: */
	}

L250:
	i__ = 1;
L270:
	w[i__] = p;
	ind[i__] = tag;
/* L290: */
    }

    goto L1001;
/*     .......... set error -- no convergence to an */
/*                eigenvalue after 30 iterations .......... */
L1000:
    *ierr = l;
L1001:
    return 0;
} /* imtqlv_ */

