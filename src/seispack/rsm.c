/* rsm.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int rsm_(integer *nm, integer *n, real *a, real *w, integer *
	m, real *z__, real *fwork, integer *iwork, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset;

    /* Local variables */
    static integer k1, k2, k3, k4, k5, k6, k7, k8;
    extern /* Subroutine */ int tred1_(integer *, integer *, real *, real *, 
	    real *, real *), trbak1_(integer *, integer *, real *, real *, 
	    integer *, real *), tqlrat_(integer *, real *, real *, integer *),
	     imtqlv_(integer *, real *, real *, real *, real *, integer *, 
	    integer *, real *), tinvit_(integer *, integer *, real *, real *, 
	    real *, integer *, real *, integer *, real *, integer *, real *, 
	    real *, real *, real *, real *);



/*     this subroutine calls the recommended sequence of */
/*     subroutines from the eigensystem subroutine package (eispack) */
/*     to find all of the eigenvalues and some of the eigenvectors */
/*     of a real symmetric matrix. */

/*     on input */

/*        nm  must be set to the row dimension of the two-dimensional */
/*        array parameters as declared in the calling program */
/*        dimension statement. */

/*        n  is the order of the matrix  a. */

/*        a  contains the real symmetric matrix. */

/*        m  the eigenvectors corresponding to the first m eigenvalues */
/*           are to be computed. */
/*           if m = 0 then no eigenvectors are computed. */
/*           if m = n then all of the eigenvectors are computed. */

/*     on output */

/*        w  contains all n eigenvalues in ascending order. */

/*        z  contains the orthonormal eigenvectors associated with */
/*           the first m eigenvalues. */

/*        ierr  is an integer output variable set equal to an error */
/*           completion code described in the documentation for tqlrat, */
/*           imtqlv and tinvit.  the normal completion code is zero. */

/*        fwork  is a temporary storage array of dimension 8*n. */

/*        iwork  is an integer temporary storage array of dimension n. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --iwork;
    --w;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --fwork;

    /* Function Body */
    *ierr = *n * 10;
    if (*n > *nm || *m > *nm) {
	goto L50;
    }
    k1 = 1;
    k2 = k1 + *n;
    k3 = k2 + *n;
    k4 = k3 + *n;
    k5 = k4 + *n;
    k6 = k5 + *n;
    k7 = k6 + *n;
    k8 = k7 + *n;
    if (*m > 0) {
	goto L10;
    }
/*     .......... find eigenvalues only .......... */
    tred1_(nm, n, &a[a_offset], &w[1], &fwork[k1], &fwork[k2]);
    tqlrat_(n, &w[1], &fwork[k2], ierr);
    goto L50;
/*     .......... find all eigenvalues and m eigenvectors .......... */
L10:
    tred1_(nm, n, &a[a_offset], &fwork[k1], &fwork[k2], &fwork[k3]);
    imtqlv_(n, &fwork[k1], &fwork[k2], &fwork[k3], &w[1], &iwork[1], ierr, &
	    fwork[k4]);
    tinvit_(nm, n, &fwork[k1], &fwork[k2], &fwork[k3], m, &w[1], &iwork[1], &
	    z__[z_offset], ierr, &fwork[k4], &fwork[k5], &fwork[k6], &fwork[
	    k7], &fwork[k8]);
    trbak1_(nm, n, &a[a_offset], &fwork[k2], m, &z__[z_offset]);
L50:
    return 0;
} /* rsm_ */

