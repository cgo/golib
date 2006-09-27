/* tred3.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int tred3_(integer *n, integer *nv, real *a, real *d__, real 
	*e, real *e2)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    real r__1;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static real f, g, h__;
    static integer i__, j, k, l;
    static real hh;
    static integer ii, jk, iz, jm1;
    static real scale;



/*     this subroutine is a translation of the algol procedure tred3, */
/*     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). */

/*     this subroutine reduces a real symmetric matrix, stored as */
/*     a one-dimensional array, to a symmetric tridiagonal matrix */
/*     using orthogonal similarity transformations. */

/*     on input */

/*        n is the order of the matrix. */

/*        nv must be set to the dimension of the array parameter a */
/*          as declared in the calling program dimension statement. */

/*        a contains the lower triangle of the real symmetric */
/*          input matrix, stored row-wise as a one-dimensional */
/*          array, in its first n*(n+1)/2 positions. */

/*     on output */

/*        a contains information about the orthogonal */
/*          transformations used in the reduction. */

/*        d contains the diagonal elements of the tridiagonal matrix. */

/*        e contains the subdiagonal elements of the tridiagonal */
/*          matrix in its last n-1 positions.  e(1) is set to zero. */

/*        e2 contains the squares of the corresponding elements of e. */
/*          e2 may coincide with e if the squares are not needed. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

/*     .......... for i=n step -1 until 1 do -- .......... */
    /* Parameter adjustments */
    --e2;
    --e;
    --d__;
    --a;

    /* Function Body */
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = *n + 1 - ii;
	l = i__ - 1;
	iz = i__ * l / 2;
	h__ = 0.f;
	scale = 0.f;
	if (l < 1) {
	    goto L130;
	}
/*     .......... scale row (algol tol then not needed) .......... */
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    ++iz;
	    d__[k] = a[iz];
	    scale += (r__1 = d__[k], dabs(r__1));
/* L120: */
	}

	if (scale != 0.f) {
	    goto L140;
	}
L130:
	e[i__] = 0.f;
	e2[i__] = 0.f;
	goto L290;

L140:
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    d__[k] /= scale;
	    h__ += d__[k] * d__[k];
/* L150: */
	}

	e2[i__] = scale * scale * h__;
	f = d__[l];
	r__1 = sqrt(h__);
	g = -r_sign(&r__1, &f);
	e[i__] = scale * g;
	h__ -= f * g;
	d__[l] = f - g;
	a[iz] = scale * d__[l];
	if (l == 1) {
	    goto L290;
	}
	jk = 1;

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d__[j];
	    g = 0.f;
	    jm1 = j - 1;
	    if (jm1 < 1) {
		goto L220;
	    }

	    i__3 = jm1;
	    for (k = 1; k <= i__3; ++k) {
		g += a[jk] * d__[k];
		e[k] += a[jk] * f;
		++jk;
/* L200: */
	    }

L220:
	    e[j] = g + a[jk] * f;
	    ++jk;
/* L240: */
	}
/*     .......... form p .......... */
	f = 0.f;

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    e[j] /= h__;
	    f += e[j] * d__[j];
/* L245: */
	}

	hh = f / (h__ + h__);
/*     .......... form q .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L250: */
	    e[j] -= hh * d__[j];
	}

	jk = 1;
/*     .......... form reduced a .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d__[j];
	    g = e[j];

	    i__3 = j;
	    for (k = 1; k <= i__3; ++k) {
		a[jk] = a[jk] - f * e[k] - g * d__[k];
		++jk;
/* L260: */
	    }

/* L280: */
	}

L290:
	d__[i__] = a[iz + 1];
	a[iz + 1] = scale * sqrt(h__);
/* L300: */
    }

    return 0;
} /* tred3_ */

