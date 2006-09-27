/* cinvit.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int cinvit_(integer *nm, integer *n, real *ar, real *ai, 
	real *wr, real *wi, logical *select, integer *mm, integer *m, real *
	zr, real *zi, integer *ierr, real *rm1, real *rm2, real *rv1, real *
	rv2)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset, rm1_dim1, rm1_offset, rm2_dim1, rm2_offset, 
	    i__1, i__2, i__3;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer i__, j, k, s;
    static real x, y;
    static integer ii, mp, uk, km1, ip1, its;
    static real eps3;
    extern /* Subroutine */ int cdiv_(real *, real *, real *, real *, real *, 
	    real *);
    static real norm, normv, ilambd, rlambd;
    extern doublereal pythag_(real *, real *), epslon_(real *);
    static real growto, ukroot;



/*     this subroutine is a translation of the algol procedure cx invit */
/*     by peters and wilkinson. */
/*     handbook for auto. comp. vol.ii-linear algebra, 418-439(1971). */

/*     this subroutine finds those eigenvectors of a complex upper */
/*     hessenberg matrix corresponding to specified eigenvalues, */
/*     using inverse iteration. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        ar and ai contain the real and imaginary parts, */
/*          respectively, of the hessenberg matrix. */

/*        wr and wi contain the real and imaginary parts, respectively, */
/*          of the eigenvalues of the matrix.  the eigenvalues must be */
/*          stored in a manner identical to that of subroutine  comlr, */
/*          which recognizes possible splitting of the matrix. */

/*        select specifies the eigenvectors to be found.  the */
/*          eigenvector corresponding to the j-th eigenvalue is */
/*          specified by setting select(j) to .true.. */

/*        mm should be set to an upper bound for the number of */
/*          eigenvectors to be found. */

/*     on output */

/*        ar, ai, wi, and select are unaltered. */

/*        wr may have been altered since close eigenvalues are perturbed */
/*          slightly in searching for independent eigenvectors. */

/*        m is the number of eigenvectors actually found. */

/*        zr and zi contain the real and imaginary parts, respectively, */
/*          of the eigenvectors.  the eigenvectors are normalized */
/*          so that the component of largest magnitude is 1. */
/*          any vector which fails the acceptance test is set to zero. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          -(2*n+1)   if more than mm eigenvectors have been specified, */
/*          -k         if the iteration corresponding to the k-th */
/*                     value fails, */
/*          -(n+k)     if both error situations occur. */

/*        rm1, rm2, rv1, and rv2 are temporary storage arrays. */

/*     the algol procedure guessvec appears in cinvit in line. */

/*     calls cdiv for complex division. */
/*     calls pythag for  sqrt(a*a + b*b) . */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --rv2;
    --rv1;
    rm2_dim1 = *n;
    rm2_offset = 1 + rm2_dim1;
    rm2 -= rm2_offset;
    rm1_dim1 = *n;
    rm1_offset = 1 + rm1_dim1;
    rm1 -= rm1_offset;
    --select;
    --wi;
    --wr;
    ai_dim1 = *nm;
    ai_offset = 1 + ai_dim1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = 1 + ar_dim1;
    ar -= ar_offset;
    zi_dim1 = *nm;
    zi_offset = 1 + zi_dim1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = 1 + zr_dim1;
    zr -= zr_offset;

    /* Function Body */
    *ierr = 0;
    uk = 0;
    s = 1;

    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	if (! select[k]) {
	    goto L980;
	}
	if (s > *mm) {
	    goto L1000;
	}
	if (uk >= k) {
	    goto L200;
	}
/*     .......... check for possible splitting .......... */
	i__2 = *n;
	for (uk = k; uk <= i__2; ++uk) {
	    if (uk == *n) {
		goto L140;
	    }
	    if (ar[uk + 1 + uk * ar_dim1] == 0.f && ai[uk + 1 + uk * ai_dim1] 
		    == 0.f) {
		goto L140;
	    }
/* L120: */
	}
/*     .......... compute infinity norm of leading uk by uk */
/*                (hessenberg) matrix .......... */
L140:
	norm = 0.f;
	mp = 1;

	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    x = 0.f;

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
/* L160: */
		x += pythag_(&ar[i__ + j * ar_dim1], &ai[i__ + j * ai_dim1]);
	    }

	    if (x > norm) {
		norm = x;
	    }
	    mp = i__;
/* L180: */
	}
/*     .......... eps3 replaces zero pivot in decomposition */
/*                and close roots are modified by eps3 .......... */
	if (norm == 0.f) {
	    norm = 1.f;
	}
	eps3 = epslon_(&norm);
/*     .......... growto is the criterion for growth .......... */
	ukroot = (real) uk;
	ukroot = sqrt(ukroot);
	growto = .1f / ukroot;
L200:
	rlambd = wr[k];
	ilambd = wi[k];
	if (k == 1) {
	    goto L280;
	}
	km1 = k - 1;
	goto L240;
/*     .......... perturb eigenvalue if it is close */
/*                to any previous eigenvalue .......... */
L220:
	rlambd += eps3;
/*     .......... for i=k-1 step -1 until 1 do -- .......... */
L240:
	i__2 = km1;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = k - ii;
	    if (select[i__] && (r__1 = wr[i__] - rlambd, dabs(r__1)) < eps3 &&
		     (r__2 = wi[i__] - ilambd, dabs(r__2)) < eps3) {
		goto L220;
	    }
/* L260: */
	}

	wr[k] = rlambd;
/*     .......... form upper hessenberg (ar,ai)-(rlambd,ilambd)*i */
/*                and initial complex vector .......... */
L280:
	mp = 1;

	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
		rm1[i__ + j * rm1_dim1] = ar[i__ + j * ar_dim1];
		rm2[i__ + j * rm2_dim1] = ai[i__ + j * ai_dim1];
/* L300: */
	    }

	    rm1[i__ + i__ * rm1_dim1] -= rlambd;
	    rm2[i__ + i__ * rm2_dim1] -= ilambd;
	    mp = i__;
	    rv1[i__] = eps3;
/* L320: */
	}
/*     .......... triangular decomposition with interchanges, */
/*                replacing zero pivots by eps3 .......... */
	if (uk == 1) {
	    goto L420;
	}

	i__2 = uk;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    mp = i__ - 1;
	    if (pythag_(&rm1[i__ + mp * rm1_dim1], &rm2[i__ + mp * rm2_dim1]) 
		    <= pythag_(&rm1[mp + mp * rm1_dim1], &rm2[mp + mp * 
		    rm2_dim1])) {
		goto L360;
	    }

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
		y = rm1[i__ + j * rm1_dim1];
		rm1[i__ + j * rm1_dim1] = rm1[mp + j * rm1_dim1];
		rm1[mp + j * rm1_dim1] = y;
		y = rm2[i__ + j * rm2_dim1];
		rm2[i__ + j * rm2_dim1] = rm2[mp + j * rm2_dim1];
		rm2[mp + j * rm2_dim1] = y;
/* L340: */
	    }

L360:
	    if (rm1[mp + mp * rm1_dim1] == 0.f && rm2[mp + mp * rm2_dim1] == 
		    0.f) {
		rm1[mp + mp * rm1_dim1] = eps3;
	    }
	    cdiv_(&rm1[i__ + mp * rm1_dim1], &rm2[i__ + mp * rm2_dim1], &rm1[
		    mp + mp * rm1_dim1], &rm2[mp + mp * rm2_dim1], &x, &y);
	    if (x == 0.f && y == 0.f) {
		goto L400;
	    }

	    i__3 = uk;
	    for (j = i__; j <= i__3; ++j) {
		rm1[i__ + j * rm1_dim1] = rm1[i__ + j * rm1_dim1] - x * rm1[
			mp + j * rm1_dim1] + y * rm2[mp + j * rm2_dim1];
		rm2[i__ + j * rm2_dim1] = rm2[i__ + j * rm2_dim1] - x * rm2[
			mp + j * rm2_dim1] - y * rm1[mp + j * rm1_dim1];
/* L380: */
	    }

L400:
	    ;
	}

L420:
	if (rm1[uk + uk * rm1_dim1] == 0.f && rm2[uk + uk * rm2_dim1] == 0.f) 
		{
	    rm1[uk + uk * rm1_dim1] = eps3;
	}
	its = 0;
/*     .......... back substitution */
/*                for i=uk step -1 until 1 do -- .......... */
L660:
	i__2 = uk;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = uk + 1 - ii;
	    x = rv1[i__];
	    y = 0.f;
	    if (i__ == uk) {
		goto L700;
	    }
	    ip1 = i__ + 1;

	    i__3 = uk;
	    for (j = ip1; j <= i__3; ++j) {
		x = x - rm1[i__ + j * rm1_dim1] * rv1[j] + rm2[i__ + j * 
			rm2_dim1] * rv2[j];
		y = y - rm1[i__ + j * rm1_dim1] * rv2[j] - rm2[i__ + j * 
			rm2_dim1] * rv1[j];
/* L680: */
	    }

L700:
	    cdiv_(&x, &y, &rm1[i__ + i__ * rm1_dim1], &rm2[i__ + i__ * 
		    rm2_dim1], &rv1[i__], &rv2[i__]);
/* L720: */
	}
/*     .......... acceptance test for eigenvector */
/*                and normalization .......... */
	++its;
	norm = 0.f;
	normv = 0.f;

	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    x = pythag_(&rv1[i__], &rv2[i__]);
	    if (normv >= x) {
		goto L760;
	    }
	    normv = x;
	    j = i__;
L760:
	    norm += x;
/* L780: */
	}

	if (norm < growto) {
	    goto L840;
	}
/*     .......... accept vector .......... */
	x = rv1[j];
	y = rv2[j];

	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    cdiv_(&rv1[i__], &rv2[i__], &x, &y, &zr[i__ + s * zr_dim1], &zi[
		    i__ + s * zi_dim1]);
/* L820: */
	}

	if (uk == *n) {
	    goto L940;
	}
	j = uk + 1;
	goto L900;
/*     .......... in-line procedure for choosing */
/*                a new starting vector .......... */
L840:
	if (its >= uk) {
	    goto L880;
	}
	x = ukroot;
	y = eps3 / (x + 1.f);
	rv1[1] = eps3;

	i__2 = uk;
	for (i__ = 2; i__ <= i__2; ++i__) {
/* L860: */
	    rv1[i__] = y;
	}

	j = uk - its + 1;
	rv1[j] -= eps3 * x;
	goto L660;
/*     .......... set error -- unaccepted eigenvector .......... */
L880:
	j = 1;
	*ierr = -k;
/*     .......... set remaining vector components to zero .......... */
L900:
	i__2 = *n;
	for (i__ = j; i__ <= i__2; ++i__) {
	    zr[i__ + s * zr_dim1] = 0.f;
	    zi[i__ + s * zi_dim1] = 0.f;
/* L920: */
	}

L940:
	++s;
L980:
	;
    }

    goto L1001;
/*     .......... set error -- underestimate of eigenvector */
/*                space required .......... */
L1000:
    if (*ierr != 0) {
	*ierr -= *n;
    }
    if (*ierr == 0) {
	*ierr = -((*n << 1) + 1);
    }
L1001:
    *m = s - 1;
    return 0;
} /* cinvit_ */

