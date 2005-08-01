/* qzit.f -- translated by f2c (version 20050501).
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

static real c_b5 = 1.f;

/* Subroutine */ int qzit_(integer *nm, integer *n, real *a, real *b, real *
	eps1, logical *matz, real *z__, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, i__1, i__2, 
	    i__3;
    real r__1, r__2, r__3;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static integer i__, j, k, l;
    static real r__, s, t, a1, a2, a3;
    static integer k1, k2, l1;
    static real u1, u2, u3, v1, v2, v3, a11, a12, a21, a22, a33, a34, a43, 
	    a44, b11, b12, b22, b33;
    static integer na, ld;
    static real b34, b44;
    static integer en;
    static real ep;
    static integer ll;
    static real sh;
    static integer km1, lm1;
    static real ani, bni;
    static integer ish, itn, its, enm2, lor1;
    static real epsa, epsb, anorm, bnorm;
    static integer enorn;
    extern doublereal epslon_(real *);
    static logical notlas;



/*     this subroutine is the second step of the qz algorithm */
/*     for solving generalized matrix eigenvalue problems, */
/*     siam j. numer. anal. 10, 241-256(1973) by moler and stewart, */
/*     as modified in technical note nasa tn d-7305(1973) by ward. */

/*     this subroutine accepts a pair of real matrices, one of them */
/*     in upper hessenberg form and the other in upper triangular form. */
/*     it reduces the hessenberg matrix to quasi-triangular form using */
/*     orthogonal transformations while maintaining the triangular form */
/*     of the other matrix.  it is usually preceded by  qzhes  and */
/*     followed by  qzval  and, possibly,  qzvec. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrices. */

/*        a contains a real upper hessenberg matrix. */

/*        b contains a real upper triangular matrix. */

/*        eps1 is a tolerance used to determine negligible elements. */
/*          eps1 = 0.0 (or negative) may be input, in which case an */
/*          element will be neglected only if it is less than roundoff */
/*          error times the norm of its matrix.  if the input eps1 is */
/*          positive, then an element will be considered negligible */
/*          if it is less than eps1 times the norm of its matrix.  a */
/*          positive value of eps1 may result in faster execution, */
/*          but less accurate results. */

/*        matz should be set to .true. if the right hand transformations */
/*          are to be accumulated for later use in computing */
/*          eigenvectors, and to .false. otherwise. */

/*        z contains, if matz has been set to .true., the */
/*          transformation matrix produced in the reduction */
/*          by  qzhes, if performed, or else the identity matrix. */
/*          if matz has been set to .false., z is not referenced. */

/*     on output */

/*        a has been reduced to quasi-triangular form.  the elements */
/*          below the first subdiagonal are still zero and no two */
/*          consecutive subdiagonal elements are nonzero. */

/*        b is still in upper triangular form, although its elements */
/*          have been altered.  the location b(n,1) is used to store */
/*          eps1 times the norm of b for later use by  qzval  and  qzvec. */

/*        z contains the product of the right hand transformations */
/*          (for both steps) if matz has been set to .true.. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          j          if the limit of 30*n iterations is exhausted */
/*                     while the j-th eigenvalue is being sought. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

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
    *ierr = 0;
/*     .......... compute epsa,epsb .......... */
    anorm = 0.f;
    bnorm = 0.f;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ani = 0.f;
	if (i__ != 1) {
	    ani = (r__1 = a[i__ + (i__ - 1) * a_dim1], dabs(r__1));
	}
	bni = 0.f;

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
	    ani += (r__1 = a[i__ + j * a_dim1], dabs(r__1));
	    bni += (r__1 = b[i__ + j * b_dim1], dabs(r__1));
/* L20: */
	}

	if (ani > anorm) {
	    anorm = ani;
	}
	if (bni > bnorm) {
	    bnorm = bni;
	}
/* L30: */
    }

    if (anorm == 0.f) {
	anorm = 1.f;
    }
    if (bnorm == 0.f) {
	bnorm = 1.f;
    }
    ep = *eps1;
    if (ep > 0.f) {
	goto L50;
    }
/*     .......... use roundoff level if eps1 is zero .......... */
    ep = epslon_(&c_b5);
L50:
    epsa = ep * anorm;
    epsb = ep * bnorm;
/*     .......... reduce a to quasi-triangular form, while */
/*                keeping b triangular .......... */
    lor1 = 1;
    enorn = *n;
    en = *n;
    itn = *n * 30;
/*     .......... begin qz step .......... */
L60:
    if (en <= 2) {
	goto L1001;
    }
    if (! (*matz)) {
	enorn = en;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
L70:
    ish = 2;
/*     .......... check for convergence or reducibility. */
/*                for l=en step -1 until 1 do -- .......... */
    i__1 = en;
    for (ll = 1; ll <= i__1; ++ll) {
	lm1 = en - ll;
	l = lm1 + 1;
	if (l == 1) {
	    goto L95;
	}
	if ((r__1 = a[l + lm1 * a_dim1], dabs(r__1)) <= epsa) {
	    goto L90;
	}
/* L80: */
    }

L90:
    a[l + lm1 * a_dim1] = 0.f;
    if (l < na) {
	goto L95;
    }
/*     .......... 1-by-1 or 2-by-2 block isolated .......... */
    en = lm1;
    goto L60;
/*     .......... check for small top of b .......... */
L95:
    ld = l;
L100:
    l1 = l + 1;
    b11 = b[l + l * b_dim1];
    if (dabs(b11) > epsb) {
	goto L120;
    }
    b[l + l * b_dim1] = 0.f;
    s = (r__1 = a[l + l * a_dim1], dabs(r__1)) + (r__2 = a[l1 + l * a_dim1], 
	    dabs(r__2));
    u1 = a[l + l * a_dim1] / s;
    u2 = a[l1 + l * a_dim1] / s;
    r__1 = sqrt(u1 * u1 + u2 * u2);
    r__ = r_sign(&r__1, &u1);
    v1 = -(u1 + r__) / r__;
    v2 = -u2 / r__;
    u2 = v2 / v1;

    i__1 = enorn;
    for (j = l; j <= i__1; ++j) {
	t = a[l + j * a_dim1] + u2 * a[l1 + j * a_dim1];
	a[l + j * a_dim1] += t * v1;
	a[l1 + j * a_dim1] += t * v2;
	t = b[l + j * b_dim1] + u2 * b[l1 + j * b_dim1];
	b[l + j * b_dim1] += t * v1;
	b[l1 + j * b_dim1] += t * v2;
/* L110: */
    }

    if (l != 1) {
	a[l + lm1 * a_dim1] = -a[l + lm1 * a_dim1];
    }
    lm1 = l;
    l = l1;
    goto L90;
L120:
    a11 = a[l + l * a_dim1] / b11;
    a21 = a[l1 + l * a_dim1] / b11;
    if (ish == 1) {
	goto L140;
    }
/*     .......... iteration strategy .......... */
    if (itn == 0) {
	goto L1000;
    }
    if (its == 10) {
	goto L155;
    }
/*     .......... determine type of shift .......... */
    b22 = b[l1 + l1 * b_dim1];
    if (dabs(b22) < epsb) {
	b22 = epsb;
    }
    b33 = b[na + na * b_dim1];
    if (dabs(b33) < epsb) {
	b33 = epsb;
    }
    b44 = b[en + en * b_dim1];
    if (dabs(b44) < epsb) {
	b44 = epsb;
    }
    a33 = a[na + na * a_dim1] / b33;
    a34 = a[na + en * a_dim1] / b44;
    a43 = a[en + na * a_dim1] / b33;
    a44 = a[en + en * a_dim1] / b44;
    b34 = b[na + en * b_dim1] / b44;
    t = (a43 * b34 - a33 - a44) * .5f;
    r__ = t * t + a34 * a43 - a33 * a44;
    if (r__ < 0.f) {
	goto L150;
    }
/*     .......... determine single shift zeroth column of a .......... */
    ish = 1;
    r__ = sqrt(r__);
    sh = -t + r__;
    s = -t - r__;
    if ((r__1 = s - a44, dabs(r__1)) < (r__2 = sh - a44, dabs(r__2))) {
	sh = s;
    }
/*     .......... look for two consecutive small */
/*                sub-diagonal elements of a. */
/*                for l=en-2 step -1 until ld do -- .......... */
    i__1 = enm2;
    for (ll = ld; ll <= i__1; ++ll) {
	l = enm2 + ld - ll;
	if (l == ld) {
	    goto L140;
	}
	lm1 = l - 1;
	l1 = l + 1;
	t = a[l + l * a_dim1];
	if ((r__1 = b[l + l * b_dim1], dabs(r__1)) > epsb) {
	    t -= sh * b[l + l * b_dim1];
	}
	if ((r__2 = a[l + lm1 * a_dim1], dabs(r__2)) <= (r__1 = t / a[l1 + l *
		 a_dim1], dabs(r__1)) * epsa) {
	    goto L100;
	}
/* L130: */
    }

L140:
    a1 = a11 - sh;
    a2 = a21;
    if (l != ld) {
	a[l + lm1 * a_dim1] = -a[l + lm1 * a_dim1];
    }
    goto L160;
/*     .......... determine double shift zeroth column of a .......... */
L150:
    a12 = a[l + l1 * a_dim1] / b22;
    a22 = a[l1 + l1 * a_dim1] / b22;
    b12 = b[l + l1 * b_dim1] / b22;
    a1 = ((a33 - a11) * (a44 - a11) - a34 * a43 + a43 * b34 * a11) / a21 + 
	    a12 - a11 * b12;
    a2 = a22 - a11 - a21 * b12 - (a33 - a11) - (a44 - a11) + a43 * b34;
    a3 = a[l1 + 1 + l1 * a_dim1] / b22;
    goto L160;
/*     .......... ad hoc shift .......... */
L155:
    a1 = 0.f;
    a2 = 1.f;
    a3 = 1.1605f;
L160:
    ++its;
    --itn;
    if (! (*matz)) {
	lor1 = ld;
    }
/*     .......... main loop .......... */
    i__1 = na;
    for (k = l; k <= i__1; ++k) {
	notlas = k != na && ish == 2;
	k1 = k + 1;
	k2 = k + 2;
/* Computing MAX */
	i__2 = k - 1;
	km1 = max(i__2,l);
/* Computing MIN */
	i__2 = en, i__3 = k1 + ish;
	ll = min(i__2,i__3);
	if (notlas) {
	    goto L190;
	}
/*     .......... zero a(k+1,k-1) .......... */
	if (k == l) {
	    goto L170;
	}
	a1 = a[k + km1 * a_dim1];
	a2 = a[k1 + km1 * a_dim1];
L170:
	s = dabs(a1) + dabs(a2);
	if (s == 0.f) {
	    goto L70;
	}
	u1 = a1 / s;
	u2 = a2 / s;
	r__1 = sqrt(u1 * u1 + u2 * u2);
	r__ = r_sign(&r__1, &u1);
	v1 = -(u1 + r__) / r__;
	v2 = -u2 / r__;
	u2 = v2 / v1;

	i__2 = enorn;
	for (j = km1; j <= i__2; ++j) {
	    t = a[k + j * a_dim1] + u2 * a[k1 + j * a_dim1];
	    a[k + j * a_dim1] += t * v1;
	    a[k1 + j * a_dim1] += t * v2;
	    t = b[k + j * b_dim1] + u2 * b[k1 + j * b_dim1];
	    b[k + j * b_dim1] += t * v1;
	    b[k1 + j * b_dim1] += t * v2;
/* L180: */
	}

	if (k != l) {
	    a[k1 + km1 * a_dim1] = 0.f;
	}
	goto L240;
/*     .......... zero a(k+1,k-1) and a(k+2,k-1) .......... */
L190:
	if (k == l) {
	    goto L200;
	}
	a1 = a[k + km1 * a_dim1];
	a2 = a[k1 + km1 * a_dim1];
	a3 = a[k2 + km1 * a_dim1];
L200:
	s = dabs(a1) + dabs(a2) + dabs(a3);
	if (s == 0.f) {
	    goto L260;
	}
	u1 = a1 / s;
	u2 = a2 / s;
	u3 = a3 / s;
	r__1 = sqrt(u1 * u1 + u2 * u2 + u3 * u3);
	r__ = r_sign(&r__1, &u1);
	v1 = -(u1 + r__) / r__;
	v2 = -u2 / r__;
	v3 = -u3 / r__;
	u2 = v2 / v1;
	u3 = v3 / v1;

	i__2 = enorn;
	for (j = km1; j <= i__2; ++j) {
	    t = a[k + j * a_dim1] + u2 * a[k1 + j * a_dim1] + u3 * a[k2 + j * 
		    a_dim1];
	    a[k + j * a_dim1] += t * v1;
	    a[k1 + j * a_dim1] += t * v2;
	    a[k2 + j * a_dim1] += t * v3;
	    t = b[k + j * b_dim1] + u2 * b[k1 + j * b_dim1] + u3 * b[k2 + j * 
		    b_dim1];
	    b[k + j * b_dim1] += t * v1;
	    b[k1 + j * b_dim1] += t * v2;
	    b[k2 + j * b_dim1] += t * v3;
/* L210: */
	}

	if (k == l) {
	    goto L220;
	}
	a[k1 + km1 * a_dim1] = 0.f;
	a[k2 + km1 * a_dim1] = 0.f;
/*     .......... zero b(k+2,k+1) and b(k+2,k) .......... */
L220:
	s = (r__1 = b[k2 + k2 * b_dim1], dabs(r__1)) + (r__2 = b[k2 + k1 * 
		b_dim1], dabs(r__2)) + (r__3 = b[k2 + k * b_dim1], dabs(r__3))
		;
	if (s == 0.f) {
	    goto L240;
	}
	u1 = b[k2 + k2 * b_dim1] / s;
	u2 = b[k2 + k1 * b_dim1] / s;
	u3 = b[k2 + k * b_dim1] / s;
	r__1 = sqrt(u1 * u1 + u2 * u2 + u3 * u3);
	r__ = r_sign(&r__1, &u1);
	v1 = -(u1 + r__) / r__;
	v2 = -u2 / r__;
	v3 = -u3 / r__;
	u2 = v2 / v1;
	u3 = v3 / v1;

	i__2 = ll;
	for (i__ = lor1; i__ <= i__2; ++i__) {
	    t = a[i__ + k2 * a_dim1] + u2 * a[i__ + k1 * a_dim1] + u3 * a[i__ 
		    + k * a_dim1];
	    a[i__ + k2 * a_dim1] += t * v1;
	    a[i__ + k1 * a_dim1] += t * v2;
	    a[i__ + k * a_dim1] += t * v3;
	    t = b[i__ + k2 * b_dim1] + u2 * b[i__ + k1 * b_dim1] + u3 * b[i__ 
		    + k * b_dim1];
	    b[i__ + k2 * b_dim1] += t * v1;
	    b[i__ + k1 * b_dim1] += t * v2;
	    b[i__ + k * b_dim1] += t * v3;
/* L230: */
	}

	b[k2 + k * b_dim1] = 0.f;
	b[k2 + k1 * b_dim1] = 0.f;
	if (! (*matz)) {
	    goto L240;
	}

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    t = z__[i__ + k2 * z_dim1] + u2 * z__[i__ + k1 * z_dim1] + u3 * 
		    z__[i__ + k * z_dim1];
	    z__[i__ + k2 * z_dim1] += t * v1;
	    z__[i__ + k1 * z_dim1] += t * v2;
	    z__[i__ + k * z_dim1] += t * v3;
/* L235: */
	}
/*     .......... zero b(k+1,k) .......... */
L240:
	s = (r__1 = b[k1 + k1 * b_dim1], dabs(r__1)) + (r__2 = b[k1 + k * 
		b_dim1], dabs(r__2));
	if (s == 0.f) {
	    goto L260;
	}
	u1 = b[k1 + k1 * b_dim1] / s;
	u2 = b[k1 + k * b_dim1] / s;
	r__1 = sqrt(u1 * u1 + u2 * u2);
	r__ = r_sign(&r__1, &u1);
	v1 = -(u1 + r__) / r__;
	v2 = -u2 / r__;
	u2 = v2 / v1;

	i__2 = ll;
	for (i__ = lor1; i__ <= i__2; ++i__) {
	    t = a[i__ + k1 * a_dim1] + u2 * a[i__ + k * a_dim1];
	    a[i__ + k1 * a_dim1] += t * v1;
	    a[i__ + k * a_dim1] += t * v2;
	    t = b[i__ + k1 * b_dim1] + u2 * b[i__ + k * b_dim1];
	    b[i__ + k1 * b_dim1] += t * v1;
	    b[i__ + k * b_dim1] += t * v2;
/* L250: */
	}

	b[k1 + k * b_dim1] = 0.f;
	if (! (*matz)) {
	    goto L260;
	}

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    t = z__[i__ + k1 * z_dim1] + u2 * z__[i__ + k * z_dim1];
	    z__[i__ + k1 * z_dim1] += t * v1;
	    z__[i__ + k * z_dim1] += t * v2;
/* L255: */
	}

L260:
	;
    }
/*     .......... end qz step .......... */
    goto L70;
/*     .......... set error -- all eigenvalues have not */
/*                converged after 30*n iterations .......... */
L1000:
    *ierr = en;
/*     .......... save epsb for use by qzval and qzvec .......... */
L1001:
    if (*n > 1) {
	b[*n + b_dim1] = epsb;
    }
    return 0;
} /* qzit_ */

