/* qzvec.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int qzvec_(integer *nm, integer *n, real *a, real *b, real *
	alfr, real *alfi, real *beta, real *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, i__1, i__2, 
	    i__3;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static real d__;
    static integer i__, j, k, m;
    static real q, r__, s, t, w, x, y, t1, t2, w1, x1, z1, di;
    static integer na, ii, en, jj;
    static real ra, dr, sa;
    static integer nn;
    static real ti, rr, tr, zz;
    static integer isw, enm2;
    static real alfm, almi, betm, epsb, almr;



/*     this subroutine is the optional fourth step of the qz algorithm */
/*     for solving generalized matrix eigenvalue problems, */
/*     siam j. numer. anal. 10, 241-256(1973) by moler and stewart. */

/*     this subroutine accepts a pair of real matrices, one of them in */
/*     quasi-triangular form (in which each 2-by-2 block corresponds to */
/*     a pair of complex eigenvalues) and the other in upper triangular */
/*     form.  it computes the eigenvectors of the triangular problem and */
/*     transforms the results back to the original coordinate system. */
/*     it is usually preceded by  qzhes,  qzit, and  qzval. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrices. */

/*        a contains a real upper quasi-triangular matrix. */

/*        b contains a real upper triangular matrix.  in addition, */
/*          location b(n,1) contains the tolerance quantity (epsb) */
/*          computed and saved in  qzit. */

/*        alfr, alfi, and beta  are vectors with components whose */
/*          ratios ((alfr+i*alfi)/beta) are the generalized */
/*          eigenvalues.  they are usually obtained from  qzval. */

/*        z contains the transformation matrix produced in the */
/*          reductions by  qzhes,  qzit, and  qzval, if performed. */
/*          if the eigenvectors of the triangular problem are */
/*          desired, z must contain the identity matrix. */

/*     on output */

/*        a is unaltered.  its subdiagonal elements provide information */
/*           about the storage of the complex eigenvectors. */

/*        b has been destroyed. */

/*        alfr, alfi, and beta are unaltered. */

/*        z contains the real and imaginary parts of the eigenvectors. */
/*          if alfi(i) .eq. 0.0, the i-th eigenvalue is real and */
/*            the i-th column of z contains its eigenvector. */
/*          if alfi(i) .ne. 0.0, the i-th eigenvalue is complex. */
/*            if alfi(i) .gt. 0.0, the eigenvalue is the first of */
/*              a complex pair and the i-th and (i+1)-th columns */
/*              of z contain its eigenvector. */
/*            if alfi(i) .lt. 0.0, the eigenvalue is the second of */
/*              a complex pair and the (i-1)-th and i-th columns */
/*              of z contain the conjugate of its eigenvector. */
/*          each eigenvector is normalized so that the modulus */
/*          of its largest component is 1.0 . */

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
    epsb = b[*n + b_dim1];
    isw = 1;
/*     .......... for en=n step -1 until 1 do -- .......... */
    i__1 = *n;
    for (nn = 1; nn <= i__1; ++nn) {
	en = *n + 1 - nn;
	na = en - 1;
	if (isw == 2) {
	    goto L795;
	}
	if (alfi[en] != 0.f) {
	    goto L710;
	}
/*     .......... real vector .......... */
	m = en;
	b[en + en * b_dim1] = 1.f;
	if (na == 0) {
	    goto L800;
	}
	alfm = alfr[m];
	betm = beta[m];
/*     .......... for i=en-1 step -1 until 1 do -- .......... */
	i__2 = na;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = en - ii;
	    w = betm * a[i__ + i__ * a_dim1] - alfm * b[i__ + i__ * b_dim1];
	    r__ = 0.f;

	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
/* L610: */
		r__ += (betm * a[i__ + j * a_dim1] - alfm * b[i__ + j * 
			b_dim1]) * b[j + en * b_dim1];
	    }

	    if (i__ == 1 || isw == 2) {
		goto L630;
	    }
	    if (betm * a[i__ + (i__ - 1) * a_dim1] == 0.f) {
		goto L630;
	    }
	    zz = w;
	    s = r__;
	    goto L690;
L630:
	    m = i__;
	    if (isw == 2) {
		goto L640;
	    }
/*     .......... real 1-by-1 block .......... */
	    t = w;
	    if (w == 0.f) {
		t = epsb;
	    }
	    b[i__ + en * b_dim1] = -r__ / t;
	    goto L700;
/*     .......... real 2-by-2 block .......... */
L640:
	    x = betm * a[i__ + (i__ + 1) * a_dim1] - alfm * b[i__ + (i__ + 1) 
		    * b_dim1];
	    y = betm * a[i__ + 1 + i__ * a_dim1];
	    q = w * zz - x * y;
	    t = (x * s - zz * r__) / q;
	    b[i__ + en * b_dim1] = t;
	    if (dabs(x) <= dabs(zz)) {
		goto L650;
	    }
	    b[i__ + 1 + en * b_dim1] = (-r__ - w * t) / x;
	    goto L690;
L650:
	    b[i__ + 1 + en * b_dim1] = (-s - y * t) / zz;
L690:
	    isw = 3 - isw;
L700:
	    ;
	}
/*     .......... end real vector .......... */
	goto L800;
/*     .......... complex vector .......... */
L710:
	m = na;
	almr = alfr[m];
	almi = alfi[m];
	betm = beta[m];
/*     .......... last vector component chosen imaginary so that */
/*                eigenvector matrix is triangular .......... */
	y = betm * a[en + na * a_dim1];
	b[na + na * b_dim1] = -almi * b[en + en * b_dim1] / y;
	b[na + en * b_dim1] = (almr * b[en + en * b_dim1] - betm * a[en + en *
		 a_dim1]) / y;
	b[en + na * b_dim1] = 0.f;
	b[en + en * b_dim1] = 1.f;
	enm2 = na - 1;
	if (enm2 == 0) {
	    goto L795;
	}
/*     .......... for i=en-2 step -1 until 1 do -- .......... */
	i__2 = enm2;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = na - ii;
	    w = betm * a[i__ + i__ * a_dim1] - almr * b[i__ + i__ * b_dim1];
	    w1 = -almi * b[i__ + i__ * b_dim1];
	    ra = 0.f;
	    sa = 0.f;

	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
		x = betm * a[i__ + j * a_dim1] - almr * b[i__ + j * b_dim1];
		x1 = -almi * b[i__ + j * b_dim1];
		ra = ra + x * b[j + na * b_dim1] - x1 * b[j + en * b_dim1];
		sa = sa + x * b[j + en * b_dim1] + x1 * b[j + na * b_dim1];
/* L760: */
	    }

	    if (i__ == 1 || isw == 2) {
		goto L770;
	    }
	    if (betm * a[i__ + (i__ - 1) * a_dim1] == 0.f) {
		goto L770;
	    }
	    zz = w;
	    z1 = w1;
	    r__ = ra;
	    s = sa;
	    isw = 2;
	    goto L790;
L770:
	    m = i__;
	    if (isw == 2) {
		goto L780;
	    }
/*     .......... complex 1-by-1 block .......... */
	    tr = -ra;
	    ti = -sa;
L773:
	    dr = w;
	    di = w1;
/*     .......... complex divide (t1,t2) = (tr,ti) / (dr,di) .......... */
L775:
	    if (dabs(di) > dabs(dr)) {
		goto L777;
	    }
	    rr = di / dr;
	    d__ = dr + di * rr;
	    t1 = (tr + ti * rr) / d__;
	    t2 = (ti - tr * rr) / d__;
	    switch (isw) {
		case 1:  goto L787;
		case 2:  goto L782;
	    }
L777:
	    rr = dr / di;
	    d__ = dr * rr + di;
	    t1 = (tr * rr + ti) / d__;
	    t2 = (ti * rr - tr) / d__;
	    switch (isw) {
		case 1:  goto L787;
		case 2:  goto L782;
	    }
/*     .......... complex 2-by-2 block .......... */
L780:
	    x = betm * a[i__ + (i__ + 1) * a_dim1] - almr * b[i__ + (i__ + 1) 
		    * b_dim1];
	    x1 = -almi * b[i__ + (i__ + 1) * b_dim1];
	    y = betm * a[i__ + 1 + i__ * a_dim1];
	    tr = y * ra - w * r__ + w1 * s;
	    ti = y * sa - w * s - w1 * r__;
	    dr = w * zz - w1 * z1 - x * y;
	    di = w * z1 + w1 * zz - x1 * y;
	    if (dr == 0.f && di == 0.f) {
		dr = epsb;
	    }
	    goto L775;
L782:
	    b[i__ + 1 + na * b_dim1] = t1;
	    b[i__ + 1 + en * b_dim1] = t2;
	    isw = 1;
	    if (dabs(y) > dabs(w) + dabs(w1)) {
		goto L785;
	    }
	    tr = -ra - x * b[i__ + 1 + na * b_dim1] + x1 * b[i__ + 1 + en * 
		    b_dim1];
	    ti = -sa - x * b[i__ + 1 + en * b_dim1] - x1 * b[i__ + 1 + na * 
		    b_dim1];
	    goto L773;
L785:
	    t1 = (-r__ - zz * b[i__ + 1 + na * b_dim1] + z1 * b[i__ + 1 + en *
		     b_dim1]) / y;
	    t2 = (-s - zz * b[i__ + 1 + en * b_dim1] - z1 * b[i__ + 1 + na * 
		    b_dim1]) / y;
L787:
	    b[i__ + na * b_dim1] = t1;
	    b[i__ + en * b_dim1] = t2;
L790:
	    ;
	}
/*     .......... end complex vector .......... */
L795:
	isw = 3 - isw;
L800:
	;
    }
/*     .......... end back substitution. */
/*                transform to original coordinate system. */
/*                for j=n step -1 until 1 do -- .......... */
    i__1 = *n;
    for (jj = 1; jj <= i__1; ++jj) {
	j = *n + 1 - jj;

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    zz = 0.f;

	    i__3 = j;
	    for (k = 1; k <= i__3; ++k) {
/* L860: */
		zz += z__[i__ + k * z_dim1] * b[k + j * b_dim1];
	    }

	    z__[i__ + j * z_dim1] = zz;
/* L880: */
	}
    }
/*     .......... normalize so that modulus of largest */
/*                component of each vector is 1. */
/*                (isw is 1 initially from before) .......... */
    i__2 = *n;
    for (j = 1; j <= i__2; ++j) {
	d__ = 0.f;
	if (isw == 2) {
	    goto L920;
	}
	if (alfi[j] != 0.f) {
	    goto L945;
	}

	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if ((r__1 = z__[i__ + j * z_dim1], dabs(r__1)) > d__) {
		d__ = (r__2 = z__[i__ + j * z_dim1], dabs(r__2));
	    }
/* L890: */
	}

	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* L900: */
	    z__[i__ + j * z_dim1] /= d__;
	}

	goto L950;

L920:
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    r__ = (r__1 = z__[i__ + (j - 1) * z_dim1], dabs(r__1)) + (r__2 = 
		    z__[i__ + j * z_dim1], dabs(r__2));
	    if (r__ != 0.f) {
/* Computing 2nd power */
		r__1 = z__[i__ + (j - 1) * z_dim1] / r__;
/* Computing 2nd power */
		r__2 = z__[i__ + j * z_dim1] / r__;
		r__ *= sqrt(r__1 * r__1 + r__2 * r__2);
	    }
	    if (r__ > d__) {
		d__ = r__;
	    }
/* L930: */
	}

	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    z__[i__ + (j - 1) * z_dim1] /= d__;
	    z__[i__ + j * z_dim1] /= d__;
/* L940: */
	}

L945:
	isw = 3 - isw;
L950:
	;
    }

    return 0;
} /* qzvec_ */

