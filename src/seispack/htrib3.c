/* htrib3.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int htrib3_(integer *nm, integer *n, real *a, real *tau, 
	integer *m, real *zr, real *zi)
{
    /* System generated locals */
    integer a_dim1, a_offset, zr_dim1, zr_offset, zi_dim1, zi_offset, i__1, 
	    i__2, i__3;

    /* Local variables */
    static real h__;
    static integer i__, j, k, l;
    static real s, si;



/*     this subroutine is a translation of a complex analogue of */
/*     the algol procedure trbak3, num. math. 11, 181-195(1968) */
/*     by martin, reinsch, and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). */

/*     this subroutine forms the eigenvectors of a complex hermitian */
/*     matrix by back transforming those of the corresponding */
/*     real symmetric tridiagonal matrix determined by  htrid3. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        a contains information about the unitary transformations */
/*          used in the reduction by  htrid3. */

/*        tau contains further information about the transformations. */

/*        m is the number of eigenvectors to be back transformed. */

/*        zr contains the eigenvectors to be back transformed */
/*          in its first m columns. */

/*     on output */

/*        zr and zi contain the real and imaginary parts, */
/*          respectively, of the transformed eigenvectors */
/*          in their first m columns. */

/*     note that the last component of each returned vector */
/*     is real and that vector euclidean norms are preserved. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    tau -= 3;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;
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
/*     .......... transform the eigenvectors of the real symmetric */
/*                tridiagonal matrix to those of the hermitian */
/*                tridiagonal matrix. .......... */
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    zi[k + j * zi_dim1] = -zr[k + j * zr_dim1] * tau[(k << 1) + 2];
	    zr[k + j * zr_dim1] *= tau[(k << 1) + 1];
/* L50: */
	}
    }

    if (*n == 1) {
	goto L200;
    }
/*     .......... recover and apply the householder matrices .......... */
    i__2 = *n;
    for (i__ = 2; i__ <= i__2; ++i__) {
	l = i__ - 1;
	h__ = a[i__ + i__ * a_dim1];
	if (h__ == 0.f) {
	    goto L140;
	}

	i__1 = *m;
	for (j = 1; j <= i__1; ++j) {
	    s = 0.f;
	    si = 0.f;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		s = s + a[i__ + k * a_dim1] * zr[k + j * zr_dim1] - a[k + i__ 
			* a_dim1] * zi[k + j * zi_dim1];
		si = si + a[i__ + k * a_dim1] * zi[k + j * zi_dim1] + a[k + 
			i__ * a_dim1] * zr[k + j * zr_dim1];
/* L110: */
	    }
/*     .......... double divisions avoid possible underflow .......... */
	    s = s / h__ / h__;
	    si = si / h__ / h__;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		zr[k + j * zr_dim1] = zr[k + j * zr_dim1] - s * a[i__ + k * 
			a_dim1] - si * a[k + i__ * a_dim1];
		zi[k + j * zi_dim1] = zi[k + j * zi_dim1] - si * a[i__ + k * 
			a_dim1] + s * a[k + i__ * a_dim1];
/* L120: */
	    }

/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* htrib3_ */

