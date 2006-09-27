/* rsp.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int rsp_(integer *nm, integer *n, integer *nv, real *a, real 
	*w, integer *matz, real *z__, real *fv1, real *fv2, integer *ierr)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j;
    extern /* Subroutine */ int tql2_(integer *, integer *, real *, real *, 
	    real *, integer *), tred3_(integer *, integer *, real *, real *, 
	    real *, real *), trbak3_(integer *, integer *, integer *, real *, 
	    integer *, real *), tqlrat_(integer *, real *, real *, integer *);



/*     this subroutine calls the recommended sequence of */
/*     subroutines from the eigensystem subroutine package (eispack) */
/*     to find the eigenvalues and eigenvectors (if desired) */
/*     of a real symmetric packed matrix. */

/*     on input */

/*        nm  must be set to the row dimension of the two-dimensional */
/*        array parameters as declared in the calling program */
/*        dimension statement. */

/*        n  is the order of the matrix  a. */

/*        nv  is an integer variable set equal to the */
/*        dimension of the array  a  as specified for */
/*        a  in the calling program.  nv  must not be */
/*        less than  n*(n+1)/2. */

/*        a  contains the lower triangle of the real symmetric */
/*        packed matrix stored row-wise. */

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
    --a;

    /* Function Body */
    if (*n <= *nm) {
	goto L5;
    }
    *ierr = *n * 10;
    goto L50;
L5:
    if (*nv >= *n * (*n + 1) / 2) {
	goto L10;
    }
    *ierr = *n * 20;
    goto L50;

L10:
    tred3_(n, nv, &a[1], &w[1], &fv1[1], &fv2[1]);
    if (*matz != 0) {
	goto L20;
    }
/*     .......... find eigenvalues only .......... */
    tqlrat_(n, &w[1], &fv2[1], ierr);
    goto L50;
/*     .......... find both eigenvalues and eigenvectors .......... */
L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    z__[j + i__ * z_dim1] = 0.f;
/* L30: */
	}

	z__[i__ + i__ * z_dim1] = 1.f;
/* L40: */
    }

    tql2_(nm, n, &w[1], &fv1[1], &z__[z_offset], ierr);
    if (*ierr != 0) {
	goto L50;
    }
    trbak3_(nm, n, nv, &a[1], n, &z__[z_offset]);
L50:
    return 0;
} /* rsp_ */

