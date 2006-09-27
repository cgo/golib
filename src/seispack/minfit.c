/* minfit.f -- translated by f2c (version 20050501).
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

static real c_b39 = 1.f;

/* Subroutine */ int minfit_(integer *nm, integer *m, integer *n, real *a, 
	real *w, integer *ip, real *b, integer *ierr, real *rv1)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2, i__3;
    real r__1, r__2, r__3, r__4;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static real c__, f, g, h__;
    static integer i__, j, k, l;
    static real s, x, y, z__;
    static integer i1, k1, l1, m1, ii, kk, ll, its;
    static real tst1, tst2, scale;
    extern doublereal pythag_(real *, real *);



/*     this subroutine is a translation of the algol procedure minfit, */
/*     num. math. 14, 403-420(1970) by golub and reinsch. */
/*     handbook for auto. comp., vol ii-linear algebra, 134-151(1971). */

/*     this subroutine determines, towards the solution of the linear */
/*                                                        t */
/*     system ax=b, the singular value decomposition a=usv  of a real */
/*                                         t */
/*     m by n rectangular matrix, forming u b rather than u.  householder */
/*     bidiagonalization and a variant of the qr algorithm are used. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement.  note that nm must be at least */
/*          as large as the maximum of m and n. */

/*        m is the number of rows of a and b. */

/*        n is the number of columns of a and the order of v. */

/*        a contains the rectangular coefficient matrix of the system. */

/*        ip is the number of columns of b.  ip can be zero. */

/*        b contains the constant column matrix of the system */
/*          if ip is not zero.  otherwise b is not referenced. */

/*     on output */

/*        a has been overwritten by the matrix v (orthogonal) of the */
/*          decomposition in its first n rows and columns.  if an */
/*          error exit is made, the columns of v corresponding to */
/*          indices of correct singular values should be correct. */

/*        w contains the n (non-negative) singular values of a (the */
/*          diagonal elements of s).  they are unordered.  if an */
/*          error exit is made, the singular values should be correct */
/*          for indices ierr+1,ierr+2,...,n. */

/*                                   t */
/*        b has been overwritten by u b.  if an error exit is made, */
/*                       t */
/*          the rows of u b corresponding to indices of correct */
/*          singular values should be correct. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          k          if the k-th singular value has not been */
/*                     determined after 30 iterations. */

/*        rv1 is a temporary storage array. */

/*     calls pythag for  sqrt(a*a + b*b) . */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --rv1;
    --w;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    b_dim1 = *nm;
    b_offset = 1 + b_dim1;
    b -= b_offset;

    /* Function Body */
    *ierr = 0;
/*     .......... householder reduction to bidiagonal form .......... */
    g = 0.f;
    scale = 0.f;
    x = 0.f;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	l = i__ + 1;
	rv1[i__] = scale * g;
	g = 0.f;
	s = 0.f;
	scale = 0.f;
	if (i__ > *m) {
	    goto L210;
	}

	i__2 = *m;
	for (k = i__; k <= i__2; ++k) {
/* L120: */
	    scale += (r__1 = a[k + i__ * a_dim1], dabs(r__1));
	}

	if (scale == 0.f) {
	    goto L210;
	}

	i__2 = *m;
	for (k = i__; k <= i__2; ++k) {
	    a[k + i__ * a_dim1] /= scale;
/* Computing 2nd power */
	    r__1 = a[k + i__ * a_dim1];
	    s += r__1 * r__1;
/* L130: */
	}

	f = a[i__ + i__ * a_dim1];
	r__1 = sqrt(s);
	g = -r_sign(&r__1, &f);
	h__ = f * g - s;
	a[i__ + i__ * a_dim1] = f - g;
	if (i__ == *n) {
	    goto L160;
	}

	i__2 = *n;
	for (j = l; j <= i__2; ++j) {
	    s = 0.f;

	    i__3 = *m;
	    for (k = i__; k <= i__3; ++k) {
/* L140: */
		s += a[k + i__ * a_dim1] * a[k + j * a_dim1];
	    }

	    f = s / h__;

	    i__3 = *m;
	    for (k = i__; k <= i__3; ++k) {
		a[k + j * a_dim1] += f * a[k + i__ * a_dim1];
/* L150: */
	    }
	}

L160:
	if (*ip == 0) {
	    goto L190;
	}

	i__3 = *ip;
	for (j = 1; j <= i__3; ++j) {
	    s = 0.f;

	    i__2 = *m;
	    for (k = i__; k <= i__2; ++k) {
/* L170: */
		s += a[k + i__ * a_dim1] * b[k + j * b_dim1];
	    }

	    f = s / h__;

	    i__2 = *m;
	    for (k = i__; k <= i__2; ++k) {
		b[k + j * b_dim1] += f * a[k + i__ * a_dim1];
/* L180: */
	    }
	}

L190:
	i__2 = *m;
	for (k = i__; k <= i__2; ++k) {
/* L200: */
	    a[k + i__ * a_dim1] = scale * a[k + i__ * a_dim1];
	}

L210:
	w[i__] = scale * g;
	g = 0.f;
	s = 0.f;
	scale = 0.f;
	if (i__ > *m || i__ == *n) {
	    goto L290;
	}

	i__2 = *n;
	for (k = l; k <= i__2; ++k) {
/* L220: */
	    scale += (r__1 = a[i__ + k * a_dim1], dabs(r__1));
	}

	if (scale == 0.f) {
	    goto L290;
	}

	i__2 = *n;
	for (k = l; k <= i__2; ++k) {
	    a[i__ + k * a_dim1] /= scale;
/* Computing 2nd power */
	    r__1 = a[i__ + k * a_dim1];
	    s += r__1 * r__1;
/* L230: */
	}

	f = a[i__ + l * a_dim1];
	r__1 = sqrt(s);
	g = -r_sign(&r__1, &f);
	h__ = f * g - s;
	a[i__ + l * a_dim1] = f - g;

	i__2 = *n;
	for (k = l; k <= i__2; ++k) {
/* L240: */
	    rv1[k] = a[i__ + k * a_dim1] / h__;
	}

	if (i__ == *m) {
	    goto L270;
	}

	i__2 = *m;
	for (j = l; j <= i__2; ++j) {
	    s = 0.f;

	    i__3 = *n;
	    for (k = l; k <= i__3; ++k) {
/* L250: */
		s += a[j + k * a_dim1] * a[i__ + k * a_dim1];
	    }

	    i__3 = *n;
	    for (k = l; k <= i__3; ++k) {
		a[j + k * a_dim1] += s * rv1[k];
/* L260: */
	    }
	}

L270:
	i__3 = *n;
	for (k = l; k <= i__3; ++k) {
/* L280: */
	    a[i__ + k * a_dim1] = scale * a[i__ + k * a_dim1];
	}

L290:
/* Computing MAX */
	r__3 = x, r__4 = (r__1 = w[i__], dabs(r__1)) + (r__2 = rv1[i__], dabs(
		r__2));
	x = dmax(r__3,r__4);
/* L300: */
    }
/*     .......... accumulation of right-hand transformations. */
/*                for i=n step -1 until 1 do -- .......... */
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = *n + 1 - ii;
	if (i__ == *n) {
	    goto L390;
	}
	if (g == 0.f) {
	    goto L360;
	}

	i__3 = *n;
	for (j = l; j <= i__3; ++j) {
/*     .......... double division avoids possible underflow .......... */
/* L320: */
	    a[j + i__ * a_dim1] = a[i__ + j * a_dim1] / a[i__ + l * a_dim1] / 
		    g;
	}

	i__3 = *n;
	for (j = l; j <= i__3; ++j) {
	    s = 0.f;

	    i__2 = *n;
	    for (k = l; k <= i__2; ++k) {
/* L340: */
		s += a[i__ + k * a_dim1] * a[k + j * a_dim1];
	    }

	    i__2 = *n;
	    for (k = l; k <= i__2; ++k) {
		a[k + j * a_dim1] += s * a[k + i__ * a_dim1];
/* L350: */
	    }
	}

L360:
	i__2 = *n;
	for (j = l; j <= i__2; ++j) {
	    a[i__ + j * a_dim1] = 0.f;
	    a[j + i__ * a_dim1] = 0.f;
/* L380: */
	}

L390:
	a[i__ + i__ * a_dim1] = 1.f;
	g = rv1[i__];
	l = i__;
/* L400: */
    }

    if (*m >= *n || *ip == 0) {
	goto L510;
    }
    m1 = *m + 1;

    i__1 = *n;
    for (i__ = m1; i__ <= i__1; ++i__) {

	i__2 = *ip;
	for (j = 1; j <= i__2; ++j) {
	    b[i__ + j * b_dim1] = 0.f;
/* L500: */
	}
    }
/*     .......... diagonalization of the bidiagonal form .......... */
L510:
    tst1 = x;
/*     .......... for k=n step -1 until 1 do -- .......... */
    i__2 = *n;
    for (kk = 1; kk <= i__2; ++kk) {
	k1 = *n - kk;
	k = k1 + 1;
	its = 0;
/*     .......... test for splitting. */
/*                for l=k step -1 until 1 do -- .......... */
L520:
	i__1 = k;
	for (ll = 1; ll <= i__1; ++ll) {
	    l1 = k - ll;
	    l = l1 + 1;
	    tst2 = tst1 + (r__1 = rv1[l], dabs(r__1));
	    if (tst2 == tst1) {
		goto L565;
	    }
/*     .......... rv1(1) is always zero, so there is no exit */
/*                through the bottom of the loop .......... */
	    tst2 = tst1 + (r__1 = w[l1], dabs(r__1));
	    if (tst2 == tst1) {
		goto L540;
	    }
/* L530: */
	}
/*     .......... cancellation of rv1(l) if l greater than 1 .......... */
L540:
	c__ = 0.f;
	s = 1.f;

	i__1 = k;
	for (i__ = l; i__ <= i__1; ++i__) {
	    f = s * rv1[i__];
	    rv1[i__] = c__ * rv1[i__];
	    tst2 = tst1 + dabs(f);
	    if (tst2 == tst1) {
		goto L565;
	    }
	    g = w[i__];
	    h__ = pythag_(&f, &g);
	    w[i__] = h__;
	    c__ = g / h__;
	    s = -f / h__;
	    if (*ip == 0) {
		goto L560;
	    }

	    i__3 = *ip;
	    for (j = 1; j <= i__3; ++j) {
		y = b[l1 + j * b_dim1];
		z__ = b[i__ + j * b_dim1];
		b[l1 + j * b_dim1] = y * c__ + z__ * s;
		b[i__ + j * b_dim1] = -y * s + z__ * c__;
/* L550: */
	    }

L560:
	    ;
	}
/*     .......... test for convergence .......... */
L565:
	z__ = w[k];
	if (l == k) {
	    goto L650;
	}
/*     .......... shift from bottom 2 by 2 minor .......... */
	if (its == 30) {
	    goto L1000;
	}
	++its;
	x = w[l];
	y = w[k1];
	g = rv1[k1];
	h__ = rv1[k];
	f = ((g + z__) / h__ * ((g - z__) / y) + y / h__ - h__ / y) * .5f;
	g = pythag_(&f, &c_b39);
	f = x - z__ / x * z__ + h__ / x * (y / (f + r_sign(&g, &f)) - h__);
/*     .......... next qr transformation .......... */
	c__ = 1.f;
	s = 1.f;

	i__1 = k1;
	for (i1 = l; i1 <= i__1; ++i1) {
	    i__ = i1 + 1;
	    g = rv1[i__];
	    y = w[i__];
	    h__ = s * g;
	    g = c__ * g;
	    z__ = pythag_(&f, &h__);
	    rv1[i1] = z__;
	    c__ = f / z__;
	    s = h__ / z__;
	    f = x * c__ + g * s;
	    g = -x * s + g * c__;
	    h__ = y * s;
	    y *= c__;

	    i__3 = *n;
	    for (j = 1; j <= i__3; ++j) {
		x = a[j + i1 * a_dim1];
		z__ = a[j + i__ * a_dim1];
		a[j + i1 * a_dim1] = x * c__ + z__ * s;
		a[j + i__ * a_dim1] = -x * s + z__ * c__;
/* L570: */
	    }

	    z__ = pythag_(&f, &h__);
	    w[i1] = z__;
/*     .......... rotation can be arbitrary if z is zero .......... */
	    if (z__ == 0.f) {
		goto L580;
	    }
	    c__ = f / z__;
	    s = h__ / z__;
L580:
	    f = c__ * g + s * y;
	    x = -s * g + c__ * y;
	    if (*ip == 0) {
		goto L600;
	    }

	    i__3 = *ip;
	    for (j = 1; j <= i__3; ++j) {
		y = b[i1 + j * b_dim1];
		z__ = b[i__ + j * b_dim1];
		b[i1 + j * b_dim1] = y * c__ + z__ * s;
		b[i__ + j * b_dim1] = -y * s + z__ * c__;
/* L590: */
	    }

L600:
	    ;
	}

	rv1[l] = 0.f;
	rv1[k] = f;
	w[k] = x;
	goto L520;
/*     .......... convergence .......... */
L650:
	if (z__ >= 0.f) {
	    goto L700;
	}
/*     .......... w(k) is made non-negative .......... */
	w[k] = -z__;

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
/* L690: */
	    a[j + k * a_dim1] = -a[j + k * a_dim1];
	}

L700:
	;
    }

    goto L1001;
/*     .......... set error -- no convergence to a */
/*                singular value after 30 iterations .......... */
L1000:
    *ierr = k;
L1001:
    return 0;
} /* minfit_ */

