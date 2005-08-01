/* qzhes.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int qzhes_(integer *nm, integer *n, real *a, real *b, 
	logical *matz, real *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, i__1, i__2, 
	    i__3;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static integer i__, j, k, l;
    static real r__, s, t;
    static integer l1;
    static real u1, u2, v1, v2;
    static integer lb, nk1, nm1, nm2;
    static real rho;



/*     this subroutine is the first step of the qz algorithm */
/*     for solving generalized matrix eigenvalue problems, */
/*     siam j. numer. anal. 10, 241-256(1973) by moler and stewart. */

/*     this subroutine accepts a pair of real general matrices and */
/*     reduces one of them to upper hessenberg form and the other */
/*     to upper triangular form using orthogonal transformations. */
/*     it is usually followed by  qzit,  qzval  and, possibly,  qzvec. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrices. */

/*        a contains a real general matrix. */

/*        b contains a real general matrix. */

/*        matz should be set to .true. if the right hand transformations */
/*          are to be accumulated for later use in computing */
/*          eigenvectors, and to .false. otherwise. */

/*     on output */

/*        a has been reduced to upper hessenberg form.  the elements */
/*          below the first subdiagonal have been set to zero. */

/*        b has been reduced to upper triangular form.  the elements */
/*          below the main diagonal have been set to zero. */

/*        z contains the product of the right hand transformations if */
/*          matz has been set to .true.  otherwise, z is not referenced. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

/*     .......... initialize z .......... */
    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    b_dim1 = *nm;
    b_offset = 1 + b_dim1;
    b -= b_offset;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    if (! (*matz)) {
	goto L10;
    }

    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    z__[i__ + j * z_dim1] = 0.f;
/* L2: */
	}

	z__[j + j * z_dim1] = 1.f;
/* L3: */
    }
/*     .......... reduce b to upper triangular form .......... */
L10:
    if (*n <= 1) {
	goto L170;
    }
    nm1 = *n - 1;

    i__1 = nm1;
    for (l = 1; l <= i__1; ++l) {
	l1 = l + 1;
	s = 0.f;

	i__2 = *n;
	for (i__ = l1; i__ <= i__2; ++i__) {
	    s += (r__1 = b[i__ + l * b_dim1], dabs(r__1));
/* L20: */
	}

	if (s == 0.f) {
	    goto L100;
	}
	s += (r__1 = b[l + l * b_dim1], dabs(r__1));
	r__ = 0.f;

	i__2 = *n;
	for (i__ = l; i__ <= i__2; ++i__) {
	    b[i__ + l * b_dim1] /= s;
/* Computing 2nd power */
	    r__1 = b[i__ + l * b_dim1];
	    r__ += r__1 * r__1;
/* L25: */
	}

	r__1 = sqrt(r__);
	r__ = r_sign(&r__1, &b[l + l * b_dim1]);
	b[l + l * b_dim1] += r__;
	rho = r__ * b[l + l * b_dim1];

	i__2 = *n;
	for (j = l1; j <= i__2; ++j) {
	    t = 0.f;

	    i__3 = *n;
	    for (i__ = l; i__ <= i__3; ++i__) {
		t += b[i__ + l * b_dim1] * b[i__ + j * b_dim1];
/* L30: */
	    }

	    t = -t / rho;

	    i__3 = *n;
	    for (i__ = l; i__ <= i__3; ++i__) {
		b[i__ + j * b_dim1] += t * b[i__ + l * b_dim1];
/* L40: */
	    }

/* L50: */
	}

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    t = 0.f;

	    i__3 = *n;
	    for (i__ = l; i__ <= i__3; ++i__) {
		t += b[i__ + l * b_dim1] * a[i__ + j * a_dim1];
/* L60: */
	    }

	    t = -t / rho;

	    i__3 = *n;
	    for (i__ = l; i__ <= i__3; ++i__) {
		a[i__ + j * a_dim1] += t * b[i__ + l * b_dim1];
/* L70: */
	    }

/* L80: */
	}

	b[l + l * b_dim1] = -s * r__;

	i__2 = *n;
	for (i__ = l1; i__ <= i__2; ++i__) {
	    b[i__ + l * b_dim1] = 0.f;
/* L90: */
	}

L100:
	;
    }
/*     .......... reduce a to upper hessenberg form, while */
/*                keeping b triangular .......... */
    if (*n == 2) {
	goto L170;
    }
    nm2 = *n - 2;

    i__1 = nm2;
    for (k = 1; k <= i__1; ++k) {
	nk1 = nm1 - k;
/*     .......... for l=n-1 step -1 until k+1 do -- .......... */
	i__2 = nk1;
	for (lb = 1; lb <= i__2; ++lb) {
	    l = *n - lb;
	    l1 = l + 1;
/*     .......... zero a(l+1,k) .......... */
	    s = (r__1 = a[l + k * a_dim1], dabs(r__1)) + (r__2 = a[l1 + k * 
		    a_dim1], dabs(r__2));
	    if (s == 0.f) {
		goto L150;
	    }
	    u1 = a[l + k * a_dim1] / s;
	    u2 = a[l1 + k * a_dim1] / s;
	    r__1 = sqrt(u1 * u1 + u2 * u2);
	    r__ = r_sign(&r__1, &u1);
	    v1 = -(u1 + r__) / r__;
	    v2 = -u2 / r__;
	    u2 = v2 / v1;

	    i__3 = *n;
	    for (j = k; j <= i__3; ++j) {
		t = a[l + j * a_dim1] + u2 * a[l1 + j * a_dim1];
		a[l + j * a_dim1] += t * v1;
		a[l1 + j * a_dim1] += t * v2;
/* L110: */
	    }

	    a[l1 + k * a_dim1] = 0.f;

	    i__3 = *n;
	    for (j = l; j <= i__3; ++j) {
		t = b[l + j * b_dim1] + u2 * b[l1 + j * b_dim1];
		b[l + j * b_dim1] += t * v1;
		b[l1 + j * b_dim1] += t * v2;
/* L120: */
	    }
/*     .......... zero b(l+1,l) .......... */
	    s = (r__1 = b[l1 + l1 * b_dim1], dabs(r__1)) + (r__2 = b[l1 + l * 
		    b_dim1], dabs(r__2));
	    if (s == 0.f) {
		goto L150;
	    }
	    u1 = b[l1 + l1 * b_dim1] / s;
	    u2 = b[l1 + l * b_dim1] / s;
	    r__1 = sqrt(u1 * u1 + u2 * u2);
	    r__ = r_sign(&r__1, &u1);
	    v1 = -(u1 + r__) / r__;
	    v2 = -u2 / r__;
	    u2 = v2 / v1;

	    i__3 = l1;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		t = b[i__ + l1 * b_dim1] + u2 * b[i__ + l * b_dim1];
		b[i__ + l1 * b_dim1] += t * v1;
		b[i__ + l * b_dim1] += t * v2;
/* L130: */
	    }

	    b[l1 + l * b_dim1] = 0.f;

	    i__3 = *n;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		t = a[i__ + l1 * a_dim1] + u2 * a[i__ + l * a_dim1];
		a[i__ + l1 * a_dim1] += t * v1;
		a[i__ + l * a_dim1] += t * v2;
/* L140: */
	    }

	    if (! (*matz)) {
		goto L150;
	    }

	    i__3 = *n;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		t = z__[i__ + l1 * z_dim1] + u2 * z__[i__ + l * z_dim1];
		z__[i__ + l1 * z_dim1] += t * v1;
		z__[i__ + l * z_dim1] += t * v2;
/* L145: */
	    }

L150:
	    ;
	}

/* L160: */
    }

L170:
    return 0;
} /* qzhes_ */

