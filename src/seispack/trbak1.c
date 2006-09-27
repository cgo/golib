/* trbak1.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int trbak1_(integer *nm, integer *n, real *a, real *e, 
	integer *m, real *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2, i__3;

    /* Local variables */
    static integer i__, j, k, l;
    static real s;



/*     this subroutine is a translation of the algol procedure trbak1, */
/*     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). */

/*     this subroutine forms the eigenvectors of a real symmetric */
/*     matrix by back transforming those of the corresponding */
/*     symmetric tridiagonal matrix determined by  tred1. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        a contains information about the orthogonal trans- */
/*          formations used in the reduction by  tred1 */
/*          in its strict lower triangle. */

/*        e contains the subdiagonal elements of the tridiagonal */
/*          matrix in its last n-1 positions.  e(1) is arbitrary. */

/*        m is the number of eigenvectors to be back transformed. */

/*        z contains the eigenvectors to be back transformed */
/*          in its first m columns. */

/*     on output */

/*        z contains the transformed eigenvectors */
/*          in its first m columns. */

/*     note that trbak1 preserves vector euclidean norms. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --e;
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
    if (*n == 1) {
	goto L200;
    }

    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	l = i__ - 1;
	if (e[i__] == 0.f) {
	    goto L140;
	}

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    s = 0.f;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
/* L110: */
		s += a[i__ + k * a_dim1] * z__[k + j * z_dim1];
	    }
/*     .......... divisor below is negative of h formed in tred1. */
/*                double division avoids possible underflow .......... */
	    s = s / a[i__ + l * a_dim1] / e[i__];

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
/* L120: */
		z__[k + j * z_dim1] += s * a[i__ + k * a_dim1];
	    }

/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* trbak1_ */

