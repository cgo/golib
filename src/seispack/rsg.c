/* rsg.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int rsg_(integer *nm, integer *n, real *a, real *b, real *w, 
	integer *matz, real *z__, real *fv1, real *fv2, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset;

    /* Local variables */
    extern /* Subroutine */ int tql2_(integer *, integer *, real *, real *, 
	    real *, integer *), tred1_(integer *, integer *, real *, real *, 
	    real *, real *), tred2_(integer *, integer *, real *, real *, 
	    real *, real *), rebak_(integer *, integer *, real *, real *, 
	    integer *, real *), reduc_(integer *, integer *, real *, real *, 
	    real *, integer *), tqlrat_(integer *, real *, real *, integer *);



/*     this subroutine calls the recommended sequence of */
/*     subroutines from the eigensystem subroutine package (eispack) */
/*     to find the eigenvalues and eigenvectors (if desired) */
/*     for the real symmetric generalized eigenproblem  ax = (lambda)bx. */

/*     on input */

/*        nm  must be set to the row dimension of the two-dimensional */
/*        array parameters as declared in the calling program */
/*        dimension statement. */

/*        n  is the order of the matrices  a  and  b. */

/*        a  contains a real symmetric matrix. */

/*        b  contains a positive definite real symmetric matrix. */

/*        matz  is an integer variable set equal to zero if */
/*        only eigenvalues are desired.  otherwise it is set to */
/*        any non-zero integer for both eigenvalues and eigenvectors. */

/*     on output */

/*        w  contains the eigenvalues in ascending order. */

/*        z  contains the eigenvectors if matz is not zero. */

/*        ierr  is an integer output variable set equal to an error */
/*           completion code described in the documentation for tqlrat */
/*           and tql2.  the normal completion code is zero. */

/*        fv1  and  fv2  are temporary storage arrays. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --fv2;
    --fv1;
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --w;
    b_dim1 = *nm;
    b_offset = 1 + b_dim1;
    b -= b_offset;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    if (*n <= *nm) {
	goto L10;
    }
    *ierr = *n * 10;
    goto L50;

L10:
    reduc_(nm, n, &a[a_offset], &b[b_offset], &fv2[1], ierr);
    if (*ierr != 0) {
	goto L50;
    }
    if (*matz != 0) {
	goto L20;
    }
/*     .......... find eigenvalues only .......... */
    tred1_(nm, n, &a[a_offset], &w[1], &fv1[1], &fv2[1]);
    tqlrat_(n, &w[1], &fv2[1], ierr);
    goto L50;
/*     .......... find both eigenvalues and eigenvectors .......... */
L20:
    tred2_(nm, n, &a[a_offset], &w[1], &fv1[1], &z__[z_offset]);
    tql2_(nm, n, &w[1], &fv1[1], &z__[z_offset], ierr);
    if (*ierr != 0) {
	goto L50;
    }
    rebak_(nm, n, &b[b_offset], &fv2[1], n, &z__[z_offset]);
L50:
    return 0;
} /* rsg_ */

