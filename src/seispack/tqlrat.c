/* tqlrat.f -- translated by f2c (version 20050501).
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

static real c_b12 = 1.f;

/* *** for old version, "send otqlrat from seispack" */
/* * From dana!moler Tue, 1 Sep 87 10:15:40 PDT */
/* * New TQLRAT */
/* Subroutine */ int tqlrat_(integer *n, real *d__, real *e2, integer *ierr)
{
    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static real b, c__, f, g, h__;
    static integer i__, j, l, m;
    static real p, r__, s, t;
    static integer l1, ii, mml;
    extern doublereal pythag_(real *, real *), epslon_(real *);



/*     This subroutine is a translation of the Algol procedure tqlrat, */
/*     Algorithm 464, Comm. ACM 16, 689(1973) by Reinsch. */

/*     This subroutine finds the eigenvalues of a symmetric */
/*     tridiagonal matrix by the rational QL method. */

/*     On input */

/*        N is the order of the matrix. */

/*        D contains the diagonal elements of the input matrix. */

/*        E2 contains the squares of the subdiagonal elements of the */
/*          input matrix in its last N-1 positions.  E2(1) is arbitrary. */

/*      On output */

/*        D contains the eigenvalues in ascending order.  If an */
/*          error exit is made, the eigenvalues are correct and */
/*          ordered for indices 1,2,...IERR-1, but may not be */
/*          the smallest eigenvalues. */

/*        E2 has been destroyed. */

/*        IERR is set to */
/*          zero       for normal return, */
/*          J          if the J-th eigenvalue has not been */
/*                     determined after 30 iterations. */

/*     Calls PYTHAG for  SQRT(A*A + B*B) . */

/*     Questions and comments should be directed to Burton S. Garbow, */
/*     Mathematics and Computer Science Div, Argonne National Laboratory */

/*     This version dated August 1987. */
/*     Modified by C. Moler to fix underflow/overflow difficulties, */
/*     especially on the VAX and other machines where epslon(1.0e0)**2 */
/*     nearly underflows.  See the loop involving statement 102 and */
/*     the two statements just before statement 200. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --e2;
    --d__;

    /* Function Body */
    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/* L100: */
	e2[i__ - 1] = e2[i__];
    }

    f = 0.f;
    t = 0.f;
    e2[*n] = 0.f;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
	h__ = (r__1 = d__[l], dabs(r__1)) + sqrt(e2[l]);
	if (t > h__) {
	    goto L105;
	}
	t = h__;
	b = epslon_(&t);
	c__ = b * b;
	if (c__ != 0.f) {
	    goto L105;
	}
/*        Spliting tolerance underflowed.  Look for larger value. */
	i__2 = *n;
	for (i__ = l; i__ <= i__2; ++i__) {
	    h__ = (r__1 = d__[i__], dabs(r__1)) + sqrt(e2[i__]);
	    if (h__ > t) {
		t = h__;
	    }
/* L102: */
	}
	b = epslon_(&t);
	c__ = b * b;
/*     .......... LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT .......... */
L105:
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    if (e2[m] <= c__) {
		goto L120;
	    }
/*     .......... E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT */
/*                THROUGH THE BOTTOM OF THE LOOP .......... */
/* L110: */
	}

L120:
	if (m == l) {
	    goto L210;
	}
L130:
	if (j == 30) {
	    goto L1000;
	}
	++j;
/*     .......... FORM SHIFT .......... */
	l1 = l + 1;
	s = sqrt(e2[l]);
	g = d__[l];
	p = (d__[l1] - g) / (s * 2.f);
	r__ = pythag_(&p, &c_b12);
	d__[l] = s / (p + r_sign(&r__, &p));
	h__ = g - d__[l];

	i__2 = *n;
	for (i__ = l1; i__ <= i__2; ++i__) {
/* L140: */
	    d__[i__] -= h__;
	}

	f += h__;
/*     .......... RATIONAL QL TRANSFORMATION .......... */
	g = d__[m];
	if (g == 0.f) {
	    g = b;
	}
	h__ = g;
	s = 0.f;
	mml = m - l;
/*     .......... FOR I=M-1 STEP -1 UNTIL L DO -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = m - ii;
	    p = g * h__;
	    r__ = p + e2[i__];
	    e2[i__ + 1] = s * r__;
	    s = e2[i__] / r__;
	    d__[i__ + 1] = h__ + s * (h__ + d__[i__]);
	    g = d__[i__] - e2[i__] / g;
/*           Avoid division by zero on next pass */
	    if (g == 0.f) {
		g = epslon_(&d__[i__]);
	    }
	    h__ = g * (p / r__);
/* L200: */
	}

	e2[l] = s * g;
	d__[l] = h__;
/*     .......... GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST .......... */
	if (h__ == 0.f) {
	    goto L210;
	}
	if ((r__1 = e2[l], dabs(r__1)) <= (r__2 = c__ / h__, dabs(r__2))) {
	    goto L210;
	}
	e2[l] = h__ * e2[l];
	if (e2[l] != 0.f) {
	    goto L130;
	}
L210:
	p = d__[l] + f;
/*     .......... ORDER EIGENVALUES .......... */
	if (l == 1) {
	    goto L250;
	}
/*     .......... FOR I=L STEP -1 UNTIL 2 DO -- .......... */
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
/*     .......... SET ERROR -- NO CONVERGENCE TO AN */
/*                EIGENVALUE AFTER 30 ITERATIONS .......... */
L1000:
    *ierr = l;
L1001:
    return 0;
} /* tqlrat_ */

