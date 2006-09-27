/* imtql1.f -- translated by f2c (version 20050501).
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

static real c_b10 = 1.f;

/* Subroutine */ int imtql1_(integer *n, real *d__, real *e, integer *ierr)
{
    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2;

    /* Builtin functions */
    double r_sign(real *, real *);

    /* Local variables */
    static real b, c__, f, g;
    static integer i__, j, l, m;
    static real p, r__, s;
    static integer ii, mml;
    static real tst1, tst2;
    extern doublereal pythag_(real *, real *);



/*     this subroutine is a translation of the algol procedure imtql1, */
/*     num. math. 12, 377-383(1968) by martin and wilkinson, */
/*     as modified in num. math. 15, 450(1970) by dubrulle. */
/*     handbook for auto. comp., vol.ii-linear algebra, 241-248(1971). */

/*     this subroutine finds the eigenvalues of a symmetric */
/*     tridiagonal matrix by the implicit ql method. */

/*     on input */

/*        n is the order of the matrix. */

/*        d contains the diagonal elements of the input matrix. */

/*        e contains the subdiagonal elements of the input matrix */
/*          in its last n-1 positions.  e(1) is arbitrary. */

/*      on output */

/*        d contains the eigenvalues in ascending order.  if an */
/*          error exit is made, the eigenvalues are correct and */
/*          ordered for indices 1,2,...ierr-1, but may not be */
/*          the smallest eigenvalues. */

/*        e has been destroyed. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          j          if the j-th eigenvalue has not been */
/*                     determined after 30 iterations. */

/*     calls pythag for  sqrt(a*a + b*b) . */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --e;
    --d__;

    /* Function Body */
    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/* L100: */
	e[i__ - 1] = e[i__];
    }

    e[*n] = 0.f;

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
	    tst1 = (r__1 = d__[m], dabs(r__1)) + (r__2 = d__[m + 1], dabs(
		    r__2));
	    tst2 = tst1 + (r__1 = e[m], dabs(r__1));
	    if (tst2 == tst1) {
		goto L120;
	    }
/* L110: */
	}

L120:
	p = d__[l];
	if (m == l) {
	    goto L215;
	}
	if (j == 30) {
	    goto L1000;
	}
	++j;
/*     .......... form shift .......... */
	g = (d__[l + 1] - p) / (e[l] * 2.f);
	r__ = pythag_(&g, &c_b10);
	g = d__[m] - p + e[l] / (g + r_sign(&r__, &g));
	s = 1.f;
	c__ = 1.f;
	p = 0.f;
	mml = m - l;
/*     .......... for i=m-1 step -1 until l do -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = m - ii;
	    f = s * e[i__];
	    b = c__ * e[i__];
	    r__ = pythag_(&f, &g);
	    e[i__ + 1] = r__;
	    if (r__ == 0.f) {
		goto L210;
	    }
	    s = f / r__;
	    c__ = g / r__;
	    g = d__[i__ + 1] - p;
	    r__ = (d__[i__] - g) * s + c__ * 2.f * b;
	    p = s * r__;
	    d__[i__ + 1] = g + p;
	    g = c__ * r__ - b;
/* L200: */
	}

	d__[l] -= p;
	e[l] = g;
	e[m] = 0.f;
	goto L105;
/*     .......... recover from underflow .......... */
L210:
	d__[i__ + 1] -= p;
	e[m] = 0.f;
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
	    if (p >= d__[i__ - 1]) {
		goto L270;
	    }
	    d__[i__] = d__[i__ - 1];
/* L230: */
	}

L250:
	i__ = 1;
L270:
	d__[i__] = p;
/* L290: */
    }

    goto L1001;
/*     .......... set error -- no convergence to an */
/*                eigenvalue after 30 iterations .......... */
L1000:
    *ierr = l;
L1001:
    return 0;
} /* imtql1_ */

