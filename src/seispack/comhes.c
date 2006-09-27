/* comhes.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int comhes_(integer *nm, integer *n, integer *low, integer *
	igh, real *ar, real *ai, integer *int__)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3;
    real r__1, r__2;

    /* Local variables */
    static integer i__, j, m, la;
    static real xi, yi, xr, yr;
    static integer mm1, kp1, mp1;
    extern /* Subroutine */ int cdiv_(real *, real *, real *, real *, real *, 
	    real *);



/*     this subroutine is a translation of the algol procedure comhes, */
/*     num. math. 12, 349-368(1968) by martin and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971). */

/*     given a complex general matrix, this subroutine */
/*     reduces a submatrix situated in rows and columns */
/*     low through igh to upper hessenberg form by */
/*     stabilized elementary similarity transformations. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        low and igh are integers determined by the balancing */
/*          subroutine  cbal.  if  cbal  has not been used, */
/*          set low=1, igh=n. */

/*        ar and ai contain the real and imaginary parts, */
/*          respectively, of the complex input matrix. */

/*     on output */

/*        ar and ai contain the real and imaginary parts, */
/*          respectively, of the hessenberg matrix.  the */
/*          multipliers which were used in the reduction */
/*          are stored in the remaining triangles under the */
/*          hessenberg matrix. */

/*        int contains information on the rows and columns */
/*          interchanged in the reduction. */
/*          only elements low through igh are used. */

/*     calls cdiv for complex division. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    ai_dim1 = *nm;
    ai_offset = 1 + ai_dim1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = 1 + ar_dim1;
    ar -= ar_offset;
    --int__;

    /* Function Body */
    la = *igh - 1;
    kp1 = *low + 1;
    if (la < kp1) {
	goto L200;
    }

    i__1 = la;
    for (m = kp1; m <= i__1; ++m) {
	mm1 = m - 1;
	xr = 0.f;
	xi = 0.f;
	i__ = m;

	i__2 = *igh;
	for (j = m; j <= i__2; ++j) {
	    if ((r__1 = ar[j + mm1 * ar_dim1], dabs(r__1)) + (r__2 = ai[j + 
		    mm1 * ai_dim1], dabs(r__2)) <= dabs(xr) + dabs(xi)) {
		goto L100;
	    }
	    xr = ar[j + mm1 * ar_dim1];
	    xi = ai[j + mm1 * ai_dim1];
	    i__ = j;
L100:
	    ;
	}

	int__[m] = i__;
	if (i__ == m) {
	    goto L130;
	}
/*     .......... interchange rows and columns of ar and ai .......... */
	i__2 = *n;
	for (j = mm1; j <= i__2; ++j) {
	    yr = ar[i__ + j * ar_dim1];
	    ar[i__ + j * ar_dim1] = ar[m + j * ar_dim1];
	    ar[m + j * ar_dim1] = yr;
	    yi = ai[i__ + j * ai_dim1];
	    ai[i__ + j * ai_dim1] = ai[m + j * ai_dim1];
	    ai[m + j * ai_dim1] = yi;
/* L110: */
	}

	i__2 = *igh;
	for (j = 1; j <= i__2; ++j) {
	    yr = ar[j + i__ * ar_dim1];
	    ar[j + i__ * ar_dim1] = ar[j + m * ar_dim1];
	    ar[j + m * ar_dim1] = yr;
	    yi = ai[j + i__ * ai_dim1];
	    ai[j + i__ * ai_dim1] = ai[j + m * ai_dim1];
	    ai[j + m * ai_dim1] = yi;
/* L120: */
	}
/*     .......... end interchange .......... */
L130:
	if (xr == 0.f && xi == 0.f) {
	    goto L180;
	}
	mp1 = m + 1;

	i__2 = *igh;
	for (i__ = mp1; i__ <= i__2; ++i__) {
	    yr = ar[i__ + mm1 * ar_dim1];
	    yi = ai[i__ + mm1 * ai_dim1];
	    if (yr == 0.f && yi == 0.f) {
		goto L160;
	    }
	    cdiv_(&yr, &yi, &xr, &xi, &yr, &yi);
	    ar[i__ + mm1 * ar_dim1] = yr;
	    ai[i__ + mm1 * ai_dim1] = yi;

	    i__3 = *n;
	    for (j = m; j <= i__3; ++j) {
		ar[i__ + j * ar_dim1] = ar[i__ + j * ar_dim1] - yr * ar[m + j 
			* ar_dim1] + yi * ai[m + j * ai_dim1];
		ai[i__ + j * ai_dim1] = ai[i__ + j * ai_dim1] - yr * ai[m + j 
			* ai_dim1] - yi * ar[m + j * ar_dim1];
/* L140: */
	    }

	    i__3 = *igh;
	    for (j = 1; j <= i__3; ++j) {
		ar[j + m * ar_dim1] = ar[j + m * ar_dim1] + yr * ar[j + i__ * 
			ar_dim1] - yi * ai[j + i__ * ai_dim1];
		ai[j + m * ai_dim1] = ai[j + m * ai_dim1] + yr * ai[j + i__ * 
			ai_dim1] + yi * ar[j + i__ * ar_dim1];
/* L150: */
	    }

L160:
	    ;
	}

L180:
	;
    }

L200:
    return 0;
} /* comhes_ */

