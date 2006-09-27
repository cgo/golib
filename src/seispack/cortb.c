/* cortb.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int cortb_(integer *nm, integer *low, integer *igh, real *ar,
	 real *ai, real *ortr, real *orti, integer *m, real *zr, real *zi)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset, i__1, i__2, i__3;

    /* Local variables */
    static real h__;
    static integer i__, j, la;
    static real gi, gr;
    static integer mm, mp, kp1, mp1;



/*     this subroutine is a translation of a complex analogue of */
/*     the algol procedure ortbak, num. math. 12, 349-368(1968) */
/*     by martin and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971). */

/*     this subroutine forms the eigenvectors of a complex general */
/*     matrix by back transforming those of the corresponding */
/*     upper hessenberg matrix determined by  corth. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        low and igh are integers determined by the balancing */
/*          subroutine  cbal.  if  cbal  has not been used, */
/*          set low=1 and igh equal to the order of the matrix. */

/*        ar and ai contain information about the unitary */
/*          transformations used in the reduction by  corth */
/*          in their strict lower triangles. */

/*        ortr and orti contain further information about the */
/*          transformations used in the reduction by  corth. */
/*          only elements low through igh are used. */

/*        m is the number of columns of zr and zi to be back transformed. */

/*        zr and zi contain the real and imaginary parts, */
/*          respectively, of the eigenvectors to be */
/*          back transformed in their first m columns. */

/*     on output */

/*        zr and zi contain the real and imaginary parts, */
/*          respectively, of the transformed eigenvectors */
/*          in their first m columns. */

/*        ortr and orti have been altered. */

/*     note that cortb preserves vector euclidean norms. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --orti;
    --ortr;
    ai_dim1 = *nm;
    ai_offset = 1 + ai_dim1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = 1 + ar_dim1;
    ar -= ar_offset;
    zi_dim1 = *nm;
    zi_offset = 1 + zi_dim1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = 1 + zr_dim1;
    zr -= zr_offset;

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
	if (ar[mp + (mp - 1) * ar_dim1] == 0.f && ai[mp + (mp - 1) * ai_dim1] 
		== 0.f) {
	    goto L140;
	}
/*     .......... h below is negative of h formed in corth .......... */
	h__ = ar[mp + (mp - 1) * ar_dim1] * ortr[mp] + ai[mp + (mp - 1) * 
		ai_dim1] * orti[mp];
	mp1 = mp + 1;

	i__2 = *igh;
	for (i__ = mp1; i__ <= i__2; ++i__) {
	    ortr[i__] = ar[i__ + (mp - 1) * ar_dim1];
	    orti[i__] = ai[i__ + (mp - 1) * ai_dim1];
/* L100: */
	}

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    gr = 0.f;
	    gi = 0.f;

	    i__3 = *igh;
	    for (i__ = mp; i__ <= i__3; ++i__) {
		gr = gr + ortr[i__] * zr[i__ + j * zr_dim1] + orti[i__] * zi[
			i__ + j * zi_dim1];
		gi = gi + ortr[i__] * zi[i__ + j * zi_dim1] - orti[i__] * zr[
			i__ + j * zr_dim1];
/* L110: */
	    }

	    gr /= h__;
	    gi /= h__;

	    i__3 = *igh;
	    for (i__ = mp; i__ <= i__3; ++i__) {
		zr[i__ + j * zr_dim1] = zr[i__ + j * zr_dim1] + gr * ortr[i__]
			 - gi * orti[i__];
		zi[i__ + j * zi_dim1] = zi[i__ + j * zi_dim1] + gr * orti[i__]
			 + gi * ortr[i__];
/* L120: */
	    }

/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* cortb_ */

