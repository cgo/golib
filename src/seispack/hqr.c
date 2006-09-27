/* hqr.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int hqr_(integer *nm, integer *n, integer *low, integer *igh,
	 real *h__, real *wr, real *wi, integer *ierr)
{
    /* System generated locals */
    integer h_dim1, h_offset, i__1, i__2, i__3;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static integer i__, j, k, l, m;
    static real p, q, r__, s, t, w, x, y;
    static integer na, en, ll, mm;
    static real zz;
    static integer mp2, itn, its, enm2;
    static real tst1, tst2, norm;
    static logical notlas;

/*  RESTORED CORRECT INDICES OF LOOPS (200,210,230,240). (9/29/89 BSG) */


/*     this subroutine is a translation of the algol procedure hqr, */
/*     num. math. 14, 219-231(1970) by martin, peters, and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 359-371(1971). */

/*     this subroutine finds the eigenvalues of a real */
/*     upper hessenberg matrix by the qr method. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        low and igh are integers determined by the balancing */
/*          subroutine  balanc.  if  balanc  has not been used, */
/*          set low=1, igh=n. */

/*        h contains the upper hessenberg matrix.  information about */
/*          the transformations used in the reduction to hessenberg */
/*          form by  elmhes  or  orthes, if performed, is stored */
/*          in the remaining triangle under the hessenberg matrix. */

/*     on output */

/*        h has been destroyed.  therefore, it must be saved */
/*          before calling  hqr  if subsequent calculation and */
/*          back transformation of eigenvectors is to be performed. */

/*        wr and wi contain the real and imaginary parts, */
/*          respectively, of the eigenvalues.  the eigenvalues */
/*          are unordered except that complex conjugate pairs */
/*          of values appear consecutively with the eigenvalue */
/*          having the positive imaginary part first.  if an */
/*          error exit is made, the eigenvalues should be correct */
/*          for indices ierr+1,...,n. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          j          if the limit of 30*n iterations is exhausted */
/*                     while the j-th eigenvalue is being sought. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated september 1989. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --wi;
    --wr;
    h_dim1 = *nm;
    h_offset = 1 + h_dim1;
    h__ -= h_offset;

    /* Function Body */
    *ierr = 0;
    norm = 0.f;
    k = 1;
/*     .......... store roots isolated by balanc */
/*                and compute matrix norm .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
/* L40: */
	    norm += (r__1 = h__[i__ + j * h_dim1], dabs(r__1));
	}

	k = i__;
	if (i__ >= *low && i__ <= *igh) {
	    goto L50;
	}
	wr[i__] = h__[i__ + i__ * h_dim1];
	wi[i__] = 0.f;
L50:
	;
    }

    en = *igh;
    t = 0.f;
    itn = *n * 30;
/*     .......... search for next eigenvalues .......... */
L60:
    if (en < *low) {
	goto L1001;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
/*     .......... look for single small sub-diagonal element */
/*                for l=en step -1 until low do -- .......... */
L70:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L100;
	}
	s = (r__1 = h__[l - 1 + (l - 1) * h_dim1], dabs(r__1)) + (r__2 = h__[
		l + l * h_dim1], dabs(r__2));
	if (s == 0.f) {
	    s = norm;
	}
	tst1 = s;
	tst2 = tst1 + (r__1 = h__[l + (l - 1) * h_dim1], dabs(r__1));
	if (tst2 == tst1) {
	    goto L100;
	}
/* L80: */
    }
/*     .......... form shift .......... */
L100:
    x = h__[en + en * h_dim1];
    if (l == en) {
	goto L270;
    }
    y = h__[na + na * h_dim1];
    w = h__[en + na * h_dim1] * h__[na + en * h_dim1];
    if (l == na) {
	goto L280;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its != 10 && its != 20) {
	goto L130;
    }
/*     .......... form exceptional shift .......... */
    t += x;

    i__1 = en;
    for (i__ = *low; i__ <= i__1; ++i__) {
/* L120: */
	h__[i__ + i__ * h_dim1] -= x;
    }

    s = (r__1 = h__[en + na * h_dim1], dabs(r__1)) + (r__2 = h__[na + enm2 * 
	    h_dim1], dabs(r__2));
    x = s * .75f;
    y = x;
    w = s * -.4375f * s;
L130:
    ++its;
    --itn;
/*     .......... look for two consecutive small */
/*                sub-diagonal elements. */
/*                for m=en-2 step -1 until l do -- .......... */
    i__1 = enm2;
    for (mm = l; mm <= i__1; ++mm) {
	m = enm2 + l - mm;
	zz = h__[m + m * h_dim1];
	r__ = x - zz;
	s = y - zz;
	p = (r__ * s - w) / h__[m + 1 + m * h_dim1] + h__[m + (m + 1) * 
		h_dim1];
	q = h__[m + 1 + (m + 1) * h_dim1] - zz - r__ - s;
	r__ = h__[m + 2 + (m + 1) * h_dim1];
	s = dabs(p) + dabs(q) + dabs(r__);
	p /= s;
	q /= s;
	r__ /= s;
	if (m == l) {
	    goto L150;
	}
	tst1 = dabs(p) * ((r__1 = h__[m - 1 + (m - 1) * h_dim1], dabs(r__1)) 
		+ dabs(zz) + (r__2 = h__[m + 1 + (m + 1) * h_dim1], dabs(r__2)
		));
	tst2 = tst1 + (r__1 = h__[m + (m - 1) * h_dim1], dabs(r__1)) * (dabs(
		q) + dabs(r__));
	if (tst2 == tst1) {
	    goto L150;
	}
/* L140: */
    }

L150:
    mp2 = m + 2;

    i__1 = en;
    for (i__ = mp2; i__ <= i__1; ++i__) {
	h__[i__ + (i__ - 2) * h_dim1] = 0.f;
	if (i__ == mp2) {
	    goto L160;
	}
	h__[i__ + (i__ - 3) * h_dim1] = 0.f;
L160:
	;
    }
/*     .......... double qr step involving rows l to en and */
/*                columns m to en .......... */
    i__1 = na;
    for (k = m; k <= i__1; ++k) {
	notlas = k != na;
	if (k == m) {
	    goto L170;
	}
	p = h__[k + (k - 1) * h_dim1];
	q = h__[k + 1 + (k - 1) * h_dim1];
	r__ = 0.f;
	if (notlas) {
	    r__ = h__[k + 2 + (k - 1) * h_dim1];
	}
	x = dabs(p) + dabs(q) + dabs(r__);
	if (x == 0.f) {
	    goto L260;
	}
	p /= x;
	q /= x;
	r__ /= x;
L170:
	r__1 = sqrt(p * p + q * q + r__ * r__);
	s = r_sign(&r__1, &p);
	if (k == m) {
	    goto L180;
	}
	h__[k + (k - 1) * h_dim1] = -s * x;
	goto L190;
L180:
	if (l != m) {
	    h__[k + (k - 1) * h_dim1] = -h__[k + (k - 1) * h_dim1];
	}
L190:
	p += s;
	x = p / s;
	y = q / s;
	zz = r__ / s;
	q /= p;
	r__ /= p;
	if (notlas) {
	    goto L225;
	}
/*     .......... row modification .......... */
	i__2 = en;
	for (j = k; j <= i__2; ++j) {
	    p = h__[k + j * h_dim1] + q * h__[k + 1 + j * h_dim1];
	    h__[k + j * h_dim1] -= p * x;
	    h__[k + 1 + j * h_dim1] -= p * y;
/* L200: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... column modification .......... */
	i__2 = j;
	for (i__ = l; i__ <= i__2; ++i__) {
	    p = x * h__[i__ + k * h_dim1] + y * h__[i__ + (k + 1) * h_dim1];
	    h__[i__ + k * h_dim1] -= p;
	    h__[i__ + (k + 1) * h_dim1] -= p * q;
/* L210: */
	}
	goto L255;
L225:
/*     .......... row modification .......... */
	i__2 = en;
	for (j = k; j <= i__2; ++j) {
	    p = h__[k + j * h_dim1] + q * h__[k + 1 + j * h_dim1] + r__ * h__[
		    k + 2 + j * h_dim1];
	    h__[k + j * h_dim1] -= p * x;
	    h__[k + 1 + j * h_dim1] -= p * y;
	    h__[k + 2 + j * h_dim1] -= p * zz;
/* L230: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... column modification .......... */
	i__2 = j;
	for (i__ = l; i__ <= i__2; ++i__) {
	    p = x * h__[i__ + k * h_dim1] + y * h__[i__ + (k + 1) * h_dim1] + 
		    zz * h__[i__ + (k + 2) * h_dim1];
	    h__[i__ + k * h_dim1] -= p;
	    h__[i__ + (k + 1) * h_dim1] -= p * q;
	    h__[i__ + (k + 2) * h_dim1] -= p * r__;
/* L240: */
	}
L255:

L260:
	;
    }

    goto L70;
/*     .......... one root found .......... */
L270:
    wr[en] = x + t;
    wi[en] = 0.f;
    en = na;
    goto L60;
/*     .......... two roots found .......... */
L280:
    p = (y - x) / 2.f;
    q = p * p + w;
    zz = sqrt((dabs(q)));
    x += t;
    if (q < 0.f) {
	goto L320;
    }
/*     .......... real pair .......... */
    zz = p + r_sign(&zz, &p);
    wr[na] = x + zz;
    wr[en] = wr[na];
    if (zz != 0.f) {
	wr[en] = x - w / zz;
    }
    wi[na] = 0.f;
    wi[en] = 0.f;
    goto L330;
/*     .......... complex pair .......... */
L320:
    wr[na] = x + p;
    wr[en] = x + p;
    wi[na] = zz;
    wi[en] = -zz;
L330:
    en = enm2;
    goto L60;
/*     .......... set error -- all eigenvalues have not */
/*                converged after 30*n iterations .......... */
L1000:
    *ierr = en;
L1001:
    return 0;
} /* hqr_ */

