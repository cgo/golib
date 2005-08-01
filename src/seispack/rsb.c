/* rsb.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int rsb_(integer *nm, integer *n, integer *mb, real *a, real 
	*w, integer *matz, real *z__, real *fv1, real *fv2, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset;

    /* Local variables */
    static logical tf;
    extern /* Subroutine */ int tql2_(integer *, integer *, real *, real *, 
	    real *, integer *), bandr_(integer *, integer *, integer *, real *
	    , real *, real *, real *, logical *, real *), tqlrat_(integer *, 
	    real *, real *, integer *);



/*     this subroutine calls the recommended sequence of */
/*     subroutines from the eigensystem subroutine package (eispack) */
/*     to find the eigenvalues and eigenvectors (if desired) */
/*     of a real symmetric band matrix. */

/*     on input */

/*        nm  must be set to the row dimension of the two-dimensional */
/*        array parameters as declared in the calling program */
/*        dimension statement. */

/*        n  is the order of the matrix  a. */

/*        mb  is the half band width of the matrix, defined as the */
/*        number of adjacent diagonals, including the principal */
/*        diagonal, required to specify the non-zero portion of the */
/*        lower triangle of the matrix. */

/*        a  contains the lower triangle of the real symmetric */
/*        band matrix.  its lowest subdiagonal is stored in the */
/*        last  n+1-mb  positions of the first column, its next */
/*        subdiagonal in the last  n+2-mb  positions of the */
/*        second column, further subdiagonals similarly, and */
/*        finally its principal diagonal in the  n  positions */
/*        of the last column.  contents of storages not part */
/*        of the matrix are arbitrary. */

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
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    if (*n <= *nm) {
	goto L5;
    }
    *ierr = *n * 10;
    goto L50;
L5:
    if (*mb > 0) {
	goto L10;
    }
    *ierr = *n * 12;
    goto L50;
L10:
    if (*mb <= *n) {
	goto L15;
    }
    *ierr = *n * 12;
    goto L50;

L15:
    if (*matz != 0) {
	goto L20;
    }
/*     .......... find eigenvalues only .......... */
    tf = FALSE_;
    bandr_(nm, n, mb, &a[a_offset], &w[1], &fv1[1], &fv2[1], &tf, &z__[
	    z_offset]);
    tqlrat_(n, &w[1], &fv2[1], ierr);
    goto L50;
/*     .......... find both eigenvalues and eigenvectors .......... */
L20:
    tf = TRUE_;
    bandr_(nm, n, mb, &a[a_offset], &w[1], &fv1[1], &fv1[1], &tf, &z__[
	    z_offset]);
    tql2_(nm, n, &w[1], &fv1[1], &z__[z_offset], ierr);
L50:
    return 0;
} /* rsb_ */

