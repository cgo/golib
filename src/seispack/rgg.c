/* rgg.f -- translated by f2c (version 20050501).
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

/* Table of constant values */

static real c_b5 = 0.f;

/* Subroutine */ int rgg_(integer *nm, integer *n, real *a, real *b, real *
	alfr, real *alfi, real *beta, integer *matz, real *z__, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset;

    /* Local variables */
    static logical tf;
    extern /* Subroutine */ int qzit_(integer *, integer *, real *, real *, 
	    real *, logical *, real *, integer *), qzvec_(integer *, integer *
	    , real *, real *, real *, real *, real *, real *), qzhes_(integer 
	    *, integer *, real *, real *, logical *, real *), qzval_(integer *
	    , integer *, real *, real *, real *, real *, real *, logical *, 
	    real *);



/*     this subroutine calls the recommended sequence of */
/*     subroutines from the eigensystem subroutine package (eispack) */
/*     to find the eigenvalues and eigenvectors (if desired) */
/*     for the real general generalized eigenproblem  ax = (lambda)bx. */

/*     on input */

/*        nm  must be set to the row dimension of the two-dimensional */
/*        array parameters as declared in the calling program */
/*        dimension statement. */

/*        n  is the order of the matrices  a  and  b. */

/*        a  contains a real general matrix. */

/*        b  contains a real general matrix. */

/*        matz  is an integer variable set equal to zero if */
/*        only eigenvalues are desired.  otherwise it is set to */
/*        any non-zero integer for both eigenvalues and eigenvectors. */

/*     on output */

/*        alfr  and  alfi  contain the real and imaginary parts, */
/*        respectively, of the numerators of the eigenvalues. */

/*        beta  contains the denominators of the eigenvalues, */
/*        which are thus given by the ratios  (alfr+i*alfi)/beta. */
/*        complex conjugate pairs of eigenvalues appear consecutively */
/*        with the eigenvalue having the positive imaginary part first. */

/*        z  contains the real and imaginary parts of the eigenvectors */
/*        if matz is not zero.  if the j-th eigenvalue is real, the */
/*        j-th column of  z  contains its eigenvector.  if the j-th */
/*        eigenvalue is complex with positive imaginary part, the */
/*        j-th and (j+1)-th columns of  z  contain the real and */
/*        imaginary parts of its eigenvector.  the conjugate of this */
/*        vector is the eigenvector for the conjugate eigenvalue. */

/*        ierr  is an integer output variable set equal to an error */
/*           completion code described in the documentation for qzit. */
/*           the normal completion code is zero. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --beta;
    --alfi;
    --alfr;
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
    if (*matz != 0) {
	goto L20;
    }
/*     .......... find eigenvalues only .......... */
    tf = FALSE_;
    qzhes_(nm, n, &a[a_offset], &b[b_offset], &tf, &z__[z_offset]);
    qzit_(nm, n, &a[a_offset], &b[b_offset], &c_b5, &tf, &z__[z_offset], ierr)
	    ;
    qzval_(nm, n, &a[a_offset], &b[b_offset], &alfr[1], &alfi[1], &beta[1], &
	    tf, &z__[z_offset]);
    goto L50;
/*     .......... find both eigenvalues and eigenvectors .......... */
L20:
    tf = TRUE_;
    qzhes_(nm, n, &a[a_offset], &b[b_offset], &tf, &z__[z_offset]);
    qzit_(nm, n, &a[a_offset], &b[b_offset], &c_b5, &tf, &z__[z_offset], ierr)
	    ;
    qzval_(nm, n, &a[a_offset], &b[b_offset], &alfr[1], &alfi[1], &beta[1], &
	    tf, &z__[z_offset]);
    if (*ierr != 0) {
	goto L50;
    }
    qzvec_(nm, n, &a[a_offset], &b[b_offset], &alfr[1], &alfi[1], &beta[1], &
	    z__[z_offset]);
L50:
    return 0;
} /* rgg_ */

