/* trbak3.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int trbak3_(integer *nm, integer *n, integer *nv, real *a, 
	integer *m, real *z__)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2, i__3;

    /* Local variables */
    static real h__;
    static integer i__, j, k, l;
    static real s;
    static integer ik, iz;



/*     this subroutine is a translation of the algol procedure trbak3, */
/*     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). */

/*     this subroutine forms the eigenvectors of a real symmetric */
/*     matrix by back transforming those of the corresponding */
/*     symmetric tridiagonal matrix determined by  tred3. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        nv must be set to the dimension of the array parameter a */
/*          as declared in the calling program dimension statement. */

/*        a contains information about the orthogonal transformations */
/*          used in the reduction by  tred3  in its first */
/*          n*(n+1)/2 positions. */

/*        m is the number of eigenvectors to be back transformed. */

/*        z contains the eigenvectors to be back transformed */
/*          in its first m columns. */

/*     on output */

/*        z contains the transformed eigenvectors */
/*          in its first m columns. */

/*     note that trbak3 preserves vector euclidean norms. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --a;
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;

    /* Function Body */
    if (*m == 0) {
	goto L200;
    }
    if (*n == 1) {
	goto L200;
    }

    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	l = i__ - 1;
	iz = i__ * l / 2;
	ik = iz + i__;
	h__ = a[ik];
	if (h__ == 0.f) {
	    goto L140;
	}

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    s = 0.f;
	    ik = iz;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		++ik;
		s += a[ik] * z__[k + j * z_dim1];
/* L110: */
	    }
/*     .......... double division avoids possible underflow .......... */
	    s = s / h__ / h__;
	    ik = iz;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		++ik;
		z__[k + j * z_dim1] -= s * a[ik];
/* L120: */
	    }

/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* trbak3_ */

