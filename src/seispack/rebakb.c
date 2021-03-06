/* rebakb.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int rebakb_(integer *nm, integer *n, real *b, real *dl, 
	integer *m, real *z__)
{
    /* System generated locals */
    integer b_dim1, b_offset, z_dim1, z_offset, i__1, i__2, i__3;

    /* Local variables */
    static integer i__, j, k;
    static real x;
    static integer i1, ii;



/*     this subroutine is a translation of the algol procedure rebakb, */
/*     num. math. 11, 99-110(1968) by martin and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 303-314(1971). */

/*     this subroutine forms the eigenvectors of a generalized */
/*     symmetric eigensystem by back transforming those of the */
/*     derived symmetric matrix determined by  reduc2. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix system. */

/*        b contains information about the similarity transformation */
/*          (cholesky decomposition) used in the reduction by  reduc2 */
/*          in its strict lower triangle. */

/*        dl contains further information about the transformation. */

/*        m is the number of eigenvectors to be back transformed. */

/*        z contains the eigenvectors to be back transformed */
/*          in its first m columns. */

/*     on output */

/*        z contains the transformed eigenvectors */
/*          in its first m columns. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --dl;
    b_dim1 = *nm;
    b_offset = 1 + b_dim1;
    b -= b_offset;
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;

    /* Function Body */
    if (*m == 0) {
	goto L200;
    }

    i__1 = *m;
    for (j = 1; j <= i__1; ++j) {
/*     .......... for i=n step -1 until 1 do -- .......... */
	i__2 = *n;
	for (ii = 1; ii <= i__2; ++ii) {
	    i1 = *n - ii;
	    i__ = i1 + 1;
	    x = dl[i__] * z__[i__ + j * z_dim1];
	    if (i__ == 1) {
		goto L80;
	    }

	    i__3 = i1;
	    for (k = 1; k <= i__3; ++k) {
/* L60: */
		x += b[i__ + k * b_dim1] * z__[k + j * z_dim1];
	    }

L80:
	    z__[i__ + j * z_dim1] = x;
/* L100: */
	}
    }

L200:
    return 0;
} /* rebakb_ */

