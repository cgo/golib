/* ortbak.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int ortbak_(integer *nm, integer *low, integer *igh, real *a,
	 real *ort, integer *m, real *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2, i__3;

    /* Local variables */
    static real g;
    static integer i__, j, la, mm, mp, kp1, mp1;



/*     this subroutine is a translation of the algol procedure ortbak, */
/*     num. math. 12, 349-368(1968) by martin and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971). */

/*     this subroutine forms the eigenvectors of a real general */
/*     matrix by back transforming those of the corresponding */
/*     upper hessenberg matrix determined by  orthes. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        low and igh are integers determined by the balancing */
/*          subroutine  balanc.  if  balanc  has not been used, */
/*          set low=1 and igh equal to the order of the matrix. */

/*        a contains information about the orthogonal trans- */
/*          formations used in the reduction by  orthes */
/*          in its strict lower triangle. */

/*        ort contains further information about the trans- */
/*          formations used in the reduction by  orthes. */
/*          only elements low through igh are used. */

/*        m is the number of columns of z to be back transformed. */

/*        z contains the real and imaginary parts of the eigen- */
/*          vectors to be back transformed in its first m columns. */

/*     on output */

/*        z contains the real and imaginary parts of the */
/*          transformed eigenvectors in its first m columns. */

/*        ort has been altered. */

/*     note that ortbak preserves vector euclidean norms. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --ort;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;

    /* Function Body */
    if (*m == 0) {
	goto L200;
    }
    la = *igh - 1;
    kp1 = *low + 1;
    if (la < kp1) {
	goto L200;
    }
/*     .......... for mp=igh-1 step -1 until low+1 do -- .......... */
    i__1 = la;
    for (mm = kp1; mm <= i__1; ++mm) {
	mp = *low + *igh - mm;
	if (a[mp + (mp - 1) * a_dim1] == 0.f) {
	    goto L140;
	}
	mp1 = mp + 1;

	i__2 = *igh;
	for (i__ = mp1; i__ <= i__2; ++i__) {
/* L100: */
	    ort[i__] = a[i__ + (mp - 1) * a_dim1];
	}

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    g = 0.f;

	    i__3 = *igh;
	    for (i__ = mp; i__ <= i__3; ++i__) {
/* L110: */
		g += ort[i__] * z__[i__ + j * z_dim1];
	    }
/*     .......... divisor below is negative of h formed in orthes. */
/*                double division avoids possible underflow .......... */
	    g = g / ort[mp] / a[mp + (mp - 1) * a_dim1];

	    i__3 = *igh;
	    for (i__ = mp; i__ <= i__3; ++i__) {
/* L120: */
		z__[i__ + j * z_dim1] += g * ort[i__];
	    }

/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* ortbak_ */

