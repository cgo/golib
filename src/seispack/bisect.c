/* bisect.f -- translated by f2c (version 20050501).
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

static real c_b26 = 1.f;

/* Subroutine */ int bisect_(integer *n, real *eps1, real *d__, real *e, real 
	*e2, real *lb, real *ub, integer *mm, integer *m, real *w, integer *
	ind, integer *ierr, real *rv4, real *rv5)
{
    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2, r__3;

    /* Local variables */
    static integer i__, j, k, l, p, q, r__, s;
    static real u, v;
    static integer m1, m2;
    static real t1, t2, x0, x1;
    static integer ii;
    static real xu;
    static integer tag;
    static real tst1, tst2;
    extern doublereal epslon_(real *);
    static integer isturm;



/*     this subroutine is a translation of the bisection technique */
/*     in the algol procedure tristurm by peters and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 418-439(1971). */

/*     this subroutine finds those eigenvalues of a tridiagonal */
/*     symmetric matrix which lie in a specified interval, */
/*     using bisection. */

/*     on input */

/*        n is the order of the matrix. */

/*        eps1 is an absolute error tolerance for the computed */
/*          eigenvalues.  if the input eps1 is non-positive, */
/*          it is reset for each submatrix to a default value, */
/*          namely, minus the product of the relative machine */
/*          precision and the 1-norm of the submatrix. */

/*        d contains the diagonal elements of the input matrix. */

/*        e contains the subdiagonal elements of the input matrix */
/*          in its last n-1 positions.  e(1) is arbitrary. */

/*        e2 contains the squares of the corresponding elements of e. */
/*          e2(1) is arbitrary. */

/*        lb and ub define the interval to be searched for eigenvalues. */
/*          if lb is not less than ub, no eigenvalues will be found. */

/*        mm should be set to an upper bound for the number of */
/*          eigenvalues in the interval.  warning. if more than */
/*          mm eigenvalues are determined to lie in the interval, */
/*          an error return is made with no eigenvalues found. */

/*     on output */

/*        eps1 is unaltered unless it has been reset to its */
/*          (last) default value. */

/*        d and e are unaltered. */

/*        elements of e2, corresponding to elements of e regarded */
/*          as negligible, have been replaced by zero causing the */
/*          matrix to split into a direct sum of submatrices. */
/*          e2(1) is also set to zero. */

/*        m is the number of eigenvalues determined to lie in (lb,ub). */

/*        w contains the m eigenvalues in ascending order. */

/*        ind contains in its first m positions the submatrix indices */
/*          associated with the corresponding eigenvalues in w -- */
/*          1 for eigenvalues belonging to the first submatrix from */
/*          the top, 2 for those belonging to the second submatrix, etc.. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          3*n+1      if m exceeds mm. */

/*        rv4 and rv5 are temporary storage arrays. */

/*     the algol procedure sturmcnt contained in tristurm */
/*     appears in bisect in-line. */

/*     note that subroutine tql1 or imtql1 is generally faster than */
/*     bisect, if more than n/4 eigenvalues are to be found. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --rv5;
    --rv4;
    --e2;
    --e;
    --d__;
    --ind;
    --w;

    /* Function Body */
    *ierr = 0;
    tag = 0;
    t1 = *lb;
    t2 = *ub;
/*     .......... look for small sub-diagonal entries .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ == 1) {
	    goto L20;
	}
	tst1 = (r__1 = d__[i__], dabs(r__1)) + (r__2 = d__[i__ - 1], dabs(
		r__2));
	tst2 = tst1 + (r__1 = e[i__], dabs(r__1));
	if (tst2 > tst1) {
	    goto L40;
	}
L20:
	e2[i__] = 0.f;
L40:
	;
    }
/*     .......... determine the number of eigenvalues */
/*                in the interval .......... */
    p = 1;
    q = *n;
    x1 = *ub;
    isturm = 1;
    goto L320;
L60:
    *m = s;
    x1 = *lb;
    isturm = 2;
    goto L320;
L80:
    *m -= s;
    if (*m > *mm) {
	goto L980;
    }
    q = 0;
    r__ = 0;
/*     .......... establish and process next submatrix, refining */
/*                interval by the gerschgorin bounds .......... */
L100:
    if (r__ == *m) {
	goto L1001;
    }
    ++tag;
    p = q + 1;
    xu = d__[p];
    x0 = d__[p];
    u = 0.f;

    i__1 = *n;
    for (q = p; q <= i__1; ++q) {
	x1 = u;
	u = 0.f;
	v = 0.f;
	if (q == *n) {
	    goto L110;
	}
	u = (r__1 = e[q + 1], dabs(r__1));
	v = e2[q + 1];
L110:
/* Computing MIN */
	r__1 = d__[q] - (x1 + u);
	xu = dmin(r__1,xu);
/* Computing MAX */
	r__1 = d__[q] + (x1 + u);
	x0 = dmax(r__1,x0);
	if (v == 0.f) {
	    goto L140;
	}
/* L120: */
    }

L140:
/* Computing MAX */
    r__2 = dabs(xu), r__3 = dabs(x0);
    r__1 = dmax(r__2,r__3);
    x1 = epslon_(&r__1);
    if (*eps1 <= 0.f) {
	*eps1 = -x1;
    }
    if (p != q) {
	goto L180;
    }
/*     .......... check for isolated root within interval .......... */
    if (t1 > d__[p] || d__[p] >= t2) {
	goto L940;
    }
    m1 = p;
    m2 = p;
    rv5[p] = d__[p];
    goto L900;
L180:
    x1 *= q - p + 1;
/* Computing MAX */
    r__1 = t1, r__2 = xu - x1;
    *lb = dmax(r__1,r__2);
/* Computing MIN */
    r__1 = t2, r__2 = x0 + x1;
    *ub = dmin(r__1,r__2);
    x1 = *lb;
    isturm = 3;
    goto L320;
L200:
    m1 = s + 1;
    x1 = *ub;
    isturm = 4;
    goto L320;
L220:
    m2 = s;
    if (m1 > m2) {
	goto L940;
    }
/*     .......... find roots by bisection .......... */
    x0 = *ub;
    isturm = 5;

    i__1 = m2;
    for (i__ = m1; i__ <= i__1; ++i__) {
	rv5[i__] = *ub;
	rv4[i__] = *lb;
/* L240: */
    }
/*     .......... loop for k-th eigenvalue */
/*                for k=m2 step -1 until m1 do -- */
/*                (-do- not used to legalize -computed go to-) .......... */
    k = m2;
L250:
    xu = *lb;
/*     .......... for i=k step -1 until m1 do -- .......... */
    i__1 = k;
    for (ii = m1; ii <= i__1; ++ii) {
	i__ = m1 + k - ii;
	if (xu >= rv4[i__]) {
	    goto L260;
	}
	xu = rv4[i__];
	goto L280;
L260:
	;
    }

L280:
    if (x0 > rv5[k]) {
	x0 = rv5[k];
    }
/*     .......... next bisection step .......... */
L300:
    x1 = (xu + x0) * .5f;
    if (x0 - xu <= dabs(*eps1)) {
	goto L420;
    }
    tst1 = (dabs(xu) + dabs(x0)) * 2.f;
    tst2 = tst1 + (x0 - xu);
    if (tst2 == tst1) {
	goto L420;
    }
/*     .......... in-line procedure for sturm sequence .......... */
L320:
    s = p - 1;
    u = 1.f;

    i__1 = q;
    for (i__ = p; i__ <= i__1; ++i__) {
	if (u != 0.f) {
	    goto L325;
	}
	v = (r__1 = e[i__], dabs(r__1)) / epslon_(&c_b26);
	if (e2[i__] == 0.f) {
	    v = 0.f;
	}
	goto L330;
L325:
	v = e2[i__] / u;
L330:
	u = d__[i__] - x1 - v;
	if (u < 0.f) {
	    ++s;
	}
/* L340: */
    }

    switch (isturm) {
	case 1:  goto L60;
	case 2:  goto L80;
	case 3:  goto L200;
	case 4:  goto L220;
	case 5:  goto L360;
    }
/*     .......... refine intervals .......... */
L360:
    if (s >= k) {
	goto L400;
    }
    xu = x1;
    if (s >= m1) {
	goto L380;
    }
    rv4[m1] = x1;
    goto L300;
L380:
    rv4[s + 1] = x1;
    if (rv5[s] > x1) {
	rv5[s] = x1;
    }
    goto L300;
L400:
    x0 = x1;
    goto L300;
/*     .......... k-th eigenvalue found .......... */
L420:
    rv5[k] = x1;
    --k;
    if (k >= m1) {
	goto L250;
    }
/*     .......... order eigenvalues tagged with their */
/*                submatrix associations .......... */
L900:
    s = r__;
    r__ = r__ + m2 - m1 + 1;
    j = 1;
    k = m1;

    i__1 = r__;
    for (l = 1; l <= i__1; ++l) {
	if (j > s) {
	    goto L910;
	}
	if (k > m2) {
	    goto L940;
	}
	if (rv5[k] >= w[l]) {
	    goto L915;
	}

	i__2 = s;
	for (ii = j; ii <= i__2; ++ii) {
	    i__ = l + s - ii;
	    w[i__ + 1] = w[i__];
	    ind[i__ + 1] = ind[i__];
/* L905: */
	}

L910:
	w[l] = rv5[k];
	ind[l] = tag;
	++k;
	goto L920;
L915:
	++j;
L920:
	;
    }

L940:
    if (q < *n) {
	goto L100;
    }
    goto L1001;
/*     .......... set error -- underestimate of number of */
/*                eigenvalues in interval .......... */
L980:
    *ierr = *n * 3 + 1;
L1001:
    *lb = t1;
    *ub = t2;
    return 0;
} /* bisect_ */

