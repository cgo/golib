/* bqr.f -- translated by f2c (version 20050501).
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

static real c_b8 = 1.f;

/* Subroutine */ int bqr_(integer *nm, integer *n, integer *mb, real *a, real 
	*t, real *r__, integer *ierr, integer *nv, real *rv)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    real r__1;

    /* Builtin functions */
    double r_sign(real *, real *), sqrt(doublereal);

    /* Local variables */
    static real f, g;
    static integer i__, j, k, l, m;
    static real q, s;
    static integer m1, m2, m3, m4, m21, m31, ii, ik, jk, kj, jm, kk, km, ll, 
	    mk, mn, ni, mz, kj1, its;
    static real tst1, tst2, scale;
    static integer imult;
    extern doublereal pythag_(real *, real *);



/*     this subroutine is a translation of the algol procedure bqr, */
/*     num. math. 16, 85-92(1970) by martin, reinsch, and wilkinson. */
/*     handbook for auto. comp., vol ii-linear algebra, 266-272(1971). */

/*     this subroutine finds the eigenvalue of smallest (usually) */
/*     magnitude of a real symmetric band matrix using the */
/*     qr algorithm with shifts of origin.  consecutive calls */
/*     can be made to find further eigenvalues. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        mb is the (half) band width of the matrix, defined as the */
/*          number of adjacent diagonals, including the principal */
/*          diagonal, required to specify the non-zero portion of the */
/*          lower triangle of the matrix. */

/*        a contains the lower triangle of the symmetric band input */
/*          matrix stored as an n by mb array.  its lowest subdiagonal */
/*          is stored in the last n+1-mb positions of the first column, */
/*          its next subdiagonal in the last n+2-mb positions of the */
/*          second column, further subdiagonals similarly, and finally */
/*          its principal diagonal in the n positions of the last column. */
/*          contents of storages not part of the matrix are arbitrary. */
/*          on a subsequent call, its output contents from the previous */
/*          call should be passed. */

/*        t specifies the shift (of eigenvalues) applied to the diagonal */
/*          of a in forming the input matrix. what is actually determined */
/*          is the eigenvalue of a+ti (i is the identity matrix) nearest */
/*          to t.  on a subsequent call, the output value of t from the */
/*          previous call should be passed if the next nearest eigenvalue */
/*          is sought. */

/*        r should be specified as zero on the first call, and as its */
/*          output value from the previous call on a subsequent call. */
/*          it is used to determine when the last row and column of */
/*          the transformed band matrix can be regarded as negligible. */

/*        nv must be set to the dimension of the array parameter rv */
/*          as declared in the calling program dimension statement. */

/*     on output */

/*        a contains the transformed band matrix.  the matrix a+ti */
/*          derived from the output parameters is similar to the */
/*          input a+ti to within rounding errors.  its last row and */
/*          column are null (if ierr is zero). */

/*        t contains the computed eigenvalue of a+ti (if ierr is zero). */

/*        r contains the maximum of its input value and the norm of the */
/*          last column of the input matrix a. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          n          if the eigenvalue has not been */
/*                     determined after 30 iterations. */

/*        rv is a temporary storage array of dimension at least */
/*          (2*mb**2+4*mb-3).  the first (3*mb-2) locations correspond */
/*          to the algol array b, the next (2*mb-1) locations correspond */
/*          to the algol array h, and the final (2*mb**2-mb) locations */
/*          correspond to the mb by (2*mb-1) algol array u. */

/*     note. for a subsequent call, n should be replaced by n-1, but */
/*     mb should not be altered even when it exceeds the current n. */

/*     calls pythag for  sqrt(a*a + b*b) . */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --rv;

    /* Function Body */
    *ierr = 0;
    m1 = min(*mb,*n);
    m = m1 - 1;
    m2 = m + m;
    m21 = m2 + 1;
    m3 = m21 + m;
    m31 = m3 + 1;
    m4 = m31 + m2;
    mn = m + *n;
    mz = *mb - m1;
    its = 0;
/*     .......... test for convergence .......... */
L40:
    g = a[*n + *mb * a_dim1];
    if (m == 0) {
	goto L360;
    }
    f = 0.f;

    i__1 = m;
    for (k = 1; k <= i__1; ++k) {
	mk = k + mz;
	f += (r__1 = a[*n + mk * a_dim1], dabs(r__1));
/* L50: */
    }

    if (its == 0 && f > *r__) {
	*r__ = f;
    }
    tst1 = *r__;
    tst2 = tst1 + f;
    if (tst2 <= tst1) {
	goto L360;
    }
    if (its == 30) {
	goto L1000;
    }
    ++its;
/*     .......... form shift from bottom 2 by 2 minor .......... */
    if (f > *r__ * .25f && its < 5) {
	goto L90;
    }
    f = a[*n + (*mb - 1) * a_dim1];
    if (f == 0.f) {
	goto L70;
    }
    q = (a[*n - 1 + *mb * a_dim1] - g) / (f * 2.f);
    s = pythag_(&q, &c_b8);
    g -= f / (q + r_sign(&s, &q));
L70:
    *t += g;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L80: */
	a[i__ + *mb * a_dim1] -= g;
    }

L90:
    i__1 = m4;
    for (k = m31; k <= i__1; ++k) {
/* L100: */
	rv[k] = 0.f;
    }

    i__1 = mn;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = ii - m;
	ni = *n - ii;
	if (ni < 0) {
	    goto L230;
	}
/*     .......... form column of shifted matrix a-g*i .......... */
/* Computing MAX */
	i__2 = 1, i__3 = 2 - i__;
	l = max(i__2,i__3);

	i__2 = m3;
	for (k = 1; k <= i__2; ++k) {
/* L110: */
	    rv[k] = 0.f;
	}

	i__2 = m1;
	for (k = l; k <= i__2; ++k) {
	    km = k + m;
	    mk = k + mz;
	    rv[km] = a[ii + mk * a_dim1];
/* L120: */
	}

	ll = min(m,ni);
	if (ll == 0) {
	    goto L135;
	}

	i__2 = ll;
	for (k = 1; k <= i__2; ++k) {
	    km = k + m21;
	    ik = ii + k;
	    mk = *mb - k;
	    rv[km] = a[ik + mk * a_dim1];
/* L130: */
	}
/*     .......... pre-multiply with householder reflections .......... */
L135:
	ll = m2;
	imult = 0;
/*     .......... multiplication procedure .......... */
L140:
	kj = m4 - m1;

	i__2 = ll;
	for (j = 1; j <= i__2; ++j) {
	    kj += m1;
	    jm = j + m3;
	    if (rv[jm] == 0.f) {
		goto L170;
	    }
	    f = 0.f;

	    i__3 = m1;
	    for (k = 1; k <= i__3; ++k) {
		++kj;
		jk = j + k - 1;
		f += rv[kj] * rv[jk];
/* L150: */
	    }

	    f /= rv[jm];
	    kj -= m1;

	    i__3 = m1;
	    for (k = 1; k <= i__3; ++k) {
		++kj;
		jk = j + k - 1;
		rv[jk] -= rv[kj] * f;
/* L160: */
	    }

	    kj -= m1;
L170:
	    ;
	}

	if (imult != 0) {
	    goto L280;
	}
/*     .......... householder reflection .......... */
	f = rv[m21];
	s = 0.f;
	rv[m4] = 0.f;
	scale = 0.f;

	i__2 = m3;
	for (k = m21; k <= i__2; ++k) {
/* L180: */
	    scale += (r__1 = rv[k], dabs(r__1));
	}

	if (scale == 0.f) {
	    goto L210;
	}

	i__2 = m3;
	for (k = m21; k <= i__2; ++k) {
/* L190: */
/* Computing 2nd power */
	    r__1 = rv[k] / scale;
	    s += r__1 * r__1;
	}

	s = scale * scale * s;
	r__1 = sqrt(s);
	g = -r_sign(&r__1, &f);
	rv[m21] = g;
	rv[m4] = s - f * g;
	kj = m4 + m2 * m1 + 1;
	rv[kj] = f - g;

	i__2 = m1;
	for (k = 2; k <= i__2; ++k) {
	    ++kj;
	    km = k + m2;
	    rv[kj] = rv[km];
/* L200: */
	}
/*     .......... save column of triangular factor r .......... */
L210:
	i__2 = m1;
	for (k = l; k <= i__2; ++k) {
	    km = k + m;
	    mk = k + mz;
	    a[ii + mk * a_dim1] = rv[km];
/* L220: */
	}

L230:
/* Computing MAX */
	i__2 = 1, i__3 = m1 + 1 - i__;
	l = max(i__2,i__3);
	if (i__ <= 0) {
	    goto L300;
	}
/*     .......... perform additional steps .......... */
	i__2 = m21;
	for (k = 1; k <= i__2; ++k) {
/* L240: */
	    rv[k] = 0.f;
	}

/* Computing MIN */
	i__2 = m1, i__3 = ni + m1;
	ll = min(i__2,i__3);
/*     .......... get row of triangular factor r .......... */
	i__2 = ll;
	for (kk = 1; kk <= i__2; ++kk) {
	    k = kk - 1;
	    km = k + m1;
	    ik = i__ + k;
	    mk = *mb - k;
	    rv[km] = a[ik + mk * a_dim1];
/* L250: */
	}
/*     .......... post-multiply with householder reflections .......... */
	ll = m1;
	imult = 1;
	goto L140;
/*     .......... store column of new a matrix .......... */
L280:
	i__2 = m1;
	for (k = l; k <= i__2; ++k) {
	    mk = k + mz;
	    a[i__ + mk * a_dim1] = rv[k];
/* L290: */
	}
/*     .......... update householder reflections .......... */
L300:
	if (l > 1) {
	    --l;
	}
	kj1 = m4 + l * m1;

	i__2 = m2;
	for (j = l; j <= i__2; ++j) {
	    jm = j + m3;
	    rv[jm] = rv[jm + 1];

	    i__3 = m1;
	    for (k = 1; k <= i__3; ++k) {
		++kj1;
		kj = kj1 - m1;
		rv[kj] = rv[kj1];
/* L320: */
	    }
	}

/* L350: */
    }

    goto L40;
/*     .......... convergence .......... */
L360:
    *t += g;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L380: */
	a[i__ + *mb * a_dim1] -= g;
    }

    i__1 = m1;
    for (k = 1; k <= i__1; ++k) {
	mk = k + mz;
	a[*n + mk * a_dim1] = 0.f;
/* L400: */
    }

    goto L1001;
/*     .......... set error -- no convergence to */
/*                eigenvalue after 30 iterations .......... */
L1000:
    *ierr = *n;
L1001:
    return 0;
} /* bqr_ */

