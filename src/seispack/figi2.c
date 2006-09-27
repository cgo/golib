/* figi2.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int figi2_(integer *nm, integer *n, real *t, real *d__, real 
	*e, real *z__, integer *ierr)
{
    /* System generated locals */
    integer t_dim1, t_offset, z_dim1, z_offset, i__1, i__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static real h__;
    static integer i__, j;



/*     given a nonsymmetric tridiagonal matrix such that the products */
/*     of corresponding pairs of off-diagonal elements are all */
/*     non-negative, and zero only when both factors are zero, this */
/*     subroutine reduces it to a symmetric tridiagonal matrix */
/*     using and accumulating diagonal similarity transformations. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        t contains the input matrix.  its subdiagonal is */
/*          stored in the last n-1 positions of the first column, */
/*          its diagonal in the n positions of the second column, */
/*          and its superdiagonal in the first n-1 positions of */
/*          the third column.  t(1,1) and t(n,3) are arbitrary. */

/*     on output */

/*        t is unaltered. */

/*        d contains the diagonal elements of the symmetric matrix. */

/*        e contains the subdiagonal elements of the symmetric */
/*          matrix in its last n-1 positions.  e(1) is not set. */

/*        z contains the transformation matrix produced in */
/*          the reduction. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          n+i        if t(i,1)*t(i-1,3) is negative, */
/*          2*n+i      if t(i,1)*t(i-1,3) is zero with */
/*                     one factor non-zero. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    t_dim1 = *nm;
    t_offset = 1 + t_dim1;
    t -= t_offset;
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --e;
    --d__;

    /* Function Body */
    *ierr = 0;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
/* L50: */
	    z__[i__ + j * z_dim1] = 0.f;
	}

	if (i__ == 1) {
	    goto L70;
	}
	h__ = t[i__ + t_dim1] * t[i__ - 1 + t_dim1 * 3];
	if (h__ < 0.f) {
	    goto L900;
	} else if (h__ == 0) {
	    goto L60;
	} else {
	    goto L80;
	}
L60:
	if (t[i__ + t_dim1] != 0.f || t[i__ - 1 + t_dim1 * 3] != 0.f) {
	    goto L1000;
	}
	e[i__] = 0.f;
L70:
	z__[i__ + i__ * z_dim1] = 1.f;
	goto L90;
L80:
	e[i__] = sqrt(h__);
	z__[i__ + i__ * z_dim1] = z__[i__ - 1 + (i__ - 1) * z_dim1] * e[i__] /
		 t[i__ - 1 + t_dim1 * 3];
L90:
	d__[i__] = t[i__ + (t_dim1 << 1)];
/* L100: */
    }

    goto L1001;
/*     .......... set error -- product of some pair of off-diagonal */
/*                elements is negative .......... */
L900:
    *ierr = *n + i__;
    goto L1001;
/*     .......... set error -- product of some pair of off-diagonal */
/*                elements is zero with one member non-zero .......... */
L1000:
    *ierr = (*n << 1) + i__;
L1001:
    return 0;
} /* figi2_ */

