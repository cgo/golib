/* rt.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int rt_(integer *nm, integer *n, real *a, real *w, integer *
	matz, real *z__, real *fv1, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset;

    /* Local variables */
    extern /* Subroutine */ int figi_(integer *, integer *, real *, real *, 
	    real *, real *, integer *), figi2_(integer *, integer *, real *, 
	    real *, real *, real *, integer *), imtql1_(integer *, real *, 
	    real *, integer *), imtql2_(integer *, integer *, real *, real *, 
	    real *, integer *);



/*     this subroutine calls the recommended sequence of */
/*     subroutines from the eigensystem subroutine package (eispack) */
/*     to find the eigenvalues and eigenvectors (if desired) */
/*     of a special real tridiagonal matrix. */

/*     on input */

/*        nm  must be set to the row dimension of the two-dimensional */
/*        array parameters as declared in the calling program */
/*        dimension statement. */

/*        n  is the order of the matrix  a. */

/*        a  contains the special real tridiagonal matrix in its */
/*        first three columns.  the subdiagonal elements are stored */
/*        in the last  n-1  positions of the first column, the */
/*        diagonal elements in the second column, and the superdiagonal */
/*        elements in the first  n-1  positions of the third column. */
/*        elements  a(1,1)  and  a(n,3)  are arbitrary. */

/*        matz  is an integer variable set equal to zero if */
/*        only eigenvalues are desired.  otherwise it is set to */
/*        any non-zero integer for both eigenvalues and eigenvectors. */

/*     on output */

/*        w  contains the eigenvalues in ascending order. */

/*        z  contains the eigenvectors if matz is not zero. */

/*        ierr  is an integer output variable set equal to an error */
/*           completion code described in the documentation for imtql1 */
/*           and imtql2.  the normal completion code is zero. */

/*        fv1  is a temporary storage array. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --fv1;
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --w;

    /* Function Body */
    if (*n <= *nm) {
	goto L10;
    }
    *ierr = *n * 10;
    goto L50;

L10:
    if (*matz != 0) {
	goto L20;
    }
/*     .......... find eigenvalues only .......... */
    figi_(nm, n, &a[a_offset], &w[1], &fv1[1], &fv1[1], ierr);
    if (*ierr > 0) {
	goto L50;
    }
    imtql1_(n, &w[1], &fv1[1], ierr);
    goto L50;
/*     .......... find both eigenvalues and eigenvectors .......... */
L20:
    figi2_(nm, n, &a[a_offset], &w[1], &fv1[1], &z__[z_offset], ierr);
    if (*ierr != 0) {
	goto L50;
    }
    imtql2_(nm, n, &w[1], &fv1[1], &z__[z_offset], ierr);
L50:
    return 0;
} /* rt_ */

