/* bakvec.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int bakvec_(integer *nm, integer *n, real *t, real *e, 
	integer *m, real *z__, integer *ierr)
{
    /* System generated locals */
    integer t_dim1, t_offset, z_dim1, z_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j;



/*     this subroutine forms the eigenvectors of a nonsymmetric */
/*     tridiagonal matrix by back transforming those of the */
/*     corresponding symmetric matrix determined by  figi. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        t contains the nonsymmetric matrix.  its subdiagonal is */
/*          stored in the last n-1 positions of the first column, */
/*          its diagonal in the n positions of the second column, */
/*          and its superdiagonal in the first n-1 positions of */
/*          the third column.  t(1,1) and t(n,3) are arbitrary. */

/*        e contains the subdiagonal elements of the symmetric */
/*          matrix in its last n-1 positions.  e(1) is arbitrary. */

/*        m is the number of eigenvectors to be back transformed. */

/*        z contains the eigenvectors to be back transformed */
/*          in its first m columns. */

/*     on output */

/*        t is unaltered. */

/*        e is destroyed. */

/*        z contains the transformed eigenvectors */
/*          in its first m columns. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          2*n+i      if e(i) is zero with t(i,1) or t(i-1,3) non-zero. */
/*                     in this case, the symmetric matrix is not similar */
/*                     to the original matrix, and the eigenvectors */
/*                     cannot be found by this program. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    t_dim1 = *nm;
    t_offset = 1 + t_dim1;
    t -= t_offset;
    --e;
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;

    /* Function Body */
    *ierr = 0;
    if (*m == 0) {
	goto L1001;
    }
    e[1] = 1.f;
    if (*n == 1) {
	goto L1001;
    }

    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (e[i__] != 0.f) {
	    goto L80;
	}
	if (t[i__ + t_dim1] != 0.f || t[i__ - 1 + t_dim1 * 3] != 0.f) {
	    goto L1000;
	}
	e[i__] = 1.f;
	goto L100;
L80:
	e[i__] = e[i__ - 1] * e[i__] / t[i__ - 1 + t_dim1 * 3];
L100:
	;
    }

    i__1 = *m;
    for (j = 1; j <= i__1; ++j) {

	i__2 = *n;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    z__[i__ + j * z_dim1] *= e[i__];
/* L120: */
	}
    }

    goto L1001;
/*     .......... set error -- eigenvectors cannot be */
/*                found by this program .......... */
L1000:
    *ierr = (*n << 1) + i__;
L1001:
    return 0;
} /* bakvec_ */

