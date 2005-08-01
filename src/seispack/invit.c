/* invit.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int invit_(integer *nm, integer *n, real *a, real *wr, real *
	wi, logical *select, integer *mm, integer *m, real *z__, integer *
	ierr, real *rm1, real *rv1, real *rv2)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, rm1_dim1, rm1_offset, i__1, 
	    i__2, i__3;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer i__, j, k, l, s;
    static real t, w, x, y;
    static integer n1, ii, ip, mp, ns, uk, km1, ip1, its;
    static real eps3;
    extern /* Subroutine */ int cdiv_(real *, real *, real *, real *, real *, 
	    real *);
    static real norm, normv, ilambd, rlambd;
    extern doublereal pythag_(real *, real *), epslon_(real *);
    static real growto, ukroot;



/*     this subroutine is a translation of the algol procedure invit */
/*     by peters and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 418-439(1971). */

/*     this subroutine finds those eigenvectors of a real upper */
/*     hessenberg matrix corresponding to specified eigenvalues, */
/*     using inverse iteration. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        a contains the hessenberg matrix. */

/*        wr and wi contain the real and imaginary parts, respectively, */
/*          of the eigenvalues of the matrix.  the eigenvalues must be */
/*          stored in a manner identical to that of subroutine  hqr, */
/*          which recognizes possible splitting of the matrix. */

/*        select specifies the eigenvectors to be found. the */
/*          eigenvector corresponding to the j-th eigenvalue is */
/*          specified by setting select(j) to .true.. */

/*        mm should be set to an upper bound for the number of */
/*          columns required to store the eigenvectors to be found. */
/*          note that two columns are required to store the */
/*          eigenvector corresponding to a complex eigenvalue. */

/*     on output */

/*        a and wi are unaltered. */

/*        wr may have been altered since close eigenvalues are perturbed */
/*          slightly in searching for independent eigenvectors. */

/*        select may have been altered.  if the elements corresponding */
/*          to a pair of conjugate complex eigenvalues were each */
/*          initially set to .true., the program resets the second of */
/*          the two elements to .false.. */

/*        m is the number of columns actually used to store */
/*          the eigenvectors. */

/*        z contains the real and imaginary parts of the eigenvectors. */
/*          if the next selected eigenvalue is real, the next column */
/*          of z contains its eigenvector.  if the eigenvalue is */
/*          complex, the next two columns of z contain the real and */
/*          imaginary parts of its eigenvector.  the eigenvectors are */
/*          normalized so that the component of largest magnitude is 1. */
/*          any vector which fails the acceptance test is set to zero. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          -(2*n+1)   if more than mm columns of z are necessary */
/*                     to store the eigenvectors corresponding to */
/*                     the specified eigenvalues. */
/*          -k         if the iteration corresponding to the k-th */
/*                     value fails, */
/*          -(n+k)     if both error situations occur. */

/*        rm1, rv1, and rv2 are temporary storage arrays.  note that rm1 */
/*          is square of dimension n by n and, augmented by two columns */
/*          of z, is the transpose of the corresponding algol b array. */

/*     the algol procedure guessvec appears in invit in line. */

/*     calls cdiv for complex division. */
/*     calls pythag for  sqrt(a*a + b*b) . */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --rv2;
    --rv1;
    rm1_dim1 = *n;
    rm1_offset = 1 + rm1_dim1;
    rm1 -= rm1_offset;
    --select;
    --wi;
    --wr;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;

    /* Function Body */
    *ierr = 0;
    uk = 0;
    s = 1;
/*     .......... ip = 0, real eigenvalue */
/*                     1, first of conjugate complex pair */
/*                    -1, second of conjugate complex pair .......... */
    ip = 0;
    n1 = *n - 1;

    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	if (wi[k] == 0.f || ip < 0) {
	    goto L100;
	}
	ip = 1;
	if (select[k] && select[k + 1]) {
	    select[k + 1] = FALSE_;
	}
L100:
	if (! select[k]) {
	    goto L960;
	}
	if (wi[k] != 0.f) {
	    ++s;
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
	    if (a[uk + 1 + uk * a_dim1] == 0.f) {
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
		x += (r__1 = a[i__ + j * a_dim1], dabs(r__1));
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
/*     .......... growto is the criterion for the growth .......... */
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
/*     .......... perturb conjugate eigenvalue to match .......... */
	ip1 = k + ip;
	wr[ip1] = rlambd;
/*     .......... form upper hessenberg a-rlambd*i (transposed) */
/*                and initial real vector .......... */
L280:
	mp = 1;

	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
/* L300: */
		rm1[j + i__ * rm1_dim1] = a[i__ + j * a_dim1];
	    }

	    rm1[i__ + i__ * rm1_dim1] -= rlambd;
	    mp = i__;
	    rv1[i__] = eps3;
/* L320: */
	}

	its = 0;
	if (ilambd != 0.f) {
	    goto L520;
	}
/*     .......... real eigenvalue. */
/*                triangular decomposition with interchanges, */
/*                replacing zero pivots by eps3 .......... */
	if (uk == 1) {
	    goto L420;
	}

	i__2 = uk;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    mp = i__ - 1;
	    if ((r__1 = rm1[mp + i__ * rm1_dim1], dabs(r__1)) <= (r__2 = rm1[
		    mp + mp * rm1_dim1], dabs(r__2))) {
		goto L360;
	    }

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
		y = rm1[j + i__ * rm1_dim1];
		rm1[j + i__ * rm1_dim1] = rm1[j + mp * rm1_dim1];
		rm1[j + mp * rm1_dim1] = y;
/* L340: */
	    }

L360:
	    if (rm1[mp + mp * rm1_dim1] == 0.f) {
		rm1[mp + mp * rm1_dim1] = eps3;
	    }
	    x = rm1[mp + i__ * rm1_dim1] / rm1[mp + mp * rm1_dim1];
	    if (x == 0.f) {
		goto L400;
	    }

	    i__3 = uk;
	    for (j = i__; j <= i__3; ++j) {
/* L380: */
		rm1[j + i__ * rm1_dim1] -= x * rm1[j + mp * rm1_dim1];
	    }

L400:
	    ;
	}

L420:
	if (rm1[uk + uk * rm1_dim1] == 0.f) {
	    rm1[uk + uk * rm1_dim1] = eps3;
	}
/*     .......... back substitution for real vector */
/*                for i=uk step -1 until 1 do -- .......... */
L440:
	i__2 = uk;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = uk + 1 - ii;
	    y = rv1[i__];
	    if (i__ == uk) {
		goto L480;
	    }
	    ip1 = i__ + 1;

	    i__3 = uk;
	    for (j = ip1; j <= i__3; ++j) {
/* L460: */
		y -= rm1[j + i__ * rm1_dim1] * rv1[j];
	    }

L480:
	    rv1[i__] = y / rm1[i__ + i__ * rm1_dim1];
/* L500: */
	}

	goto L740;
/*     .......... complex eigenvalue. */
/*                triangular decomposition with interchanges, */
/*                replacing zero pivots by eps3.  store imaginary */
/*                parts in upper triangle starting at (1,3) .......... */
L520:
	ns = *n - s;
	z__[(s - 1) * z_dim1 + 1] = -ilambd;
	z__[s * z_dim1 + 1] = 0.f;
	if (*n == 2) {
	    goto L550;
	}
	rm1[rm1_dim1 * 3 + 1] = -ilambd;
	z__[(s - 1) * z_dim1 + 1] = 0.f;
	if (*n == 3) {
	    goto L550;
	}

	i__2 = *n;
	for (i__ = 4; i__ <= i__2; ++i__) {
/* L540: */
	    rm1[i__ * rm1_dim1 + 1] = 0.f;
	}

L550:
	i__2 = uk;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    mp = i__ - 1;
	    w = rm1[mp + i__ * rm1_dim1];
	    if (i__ < *n) {
		t = rm1[mp + (i__ + 1) * rm1_dim1];
	    }
	    if (i__ == *n) {
		t = z__[mp + (s - 1) * z_dim1];
	    }
	    x = rm1[mp + mp * rm1_dim1] * rm1[mp + mp * rm1_dim1] + t * t;
	    if (w * w <= x) {
		goto L580;
	    }
	    x = rm1[mp + mp * rm1_dim1] / w;
	    y = t / w;
	    rm1[mp + mp * rm1_dim1] = w;
	    if (i__ < *n) {
		rm1[mp + (i__ + 1) * rm1_dim1] = 0.f;
	    }
	    if (i__ == *n) {
		z__[mp + (s - 1) * z_dim1] = 0.f;
	    }

	    i__3 = uk;
	    for (j = i__; j <= i__3; ++j) {
		w = rm1[j + i__ * rm1_dim1];
		rm1[j + i__ * rm1_dim1] = rm1[j + mp * rm1_dim1] - x * w;
		rm1[j + mp * rm1_dim1] = w;
		if (j < n1) {
		    goto L555;
		}
		l = j - ns;
		z__[i__ + l * z_dim1] = z__[mp + l * z_dim1] - y * w;
		z__[mp + l * z_dim1] = 0.f;
		goto L560;
L555:
		rm1[i__ + (j + 2) * rm1_dim1] = rm1[mp + (j + 2) * rm1_dim1] 
			- y * w;
		rm1[mp + (j + 2) * rm1_dim1] = 0.f;
L560:
		;
	    }

	    rm1[i__ + i__ * rm1_dim1] -= y * ilambd;
	    if (i__ < n1) {
		goto L570;
	    }
	    l = i__ - ns;
	    z__[mp + l * z_dim1] = -ilambd;
	    z__[i__ + l * z_dim1] += x * ilambd;
	    goto L640;
L570:
	    rm1[mp + (i__ + 2) * rm1_dim1] = -ilambd;
	    rm1[i__ + (i__ + 2) * rm1_dim1] += x * ilambd;
	    goto L640;
L580:
	    if (x != 0.f) {
		goto L600;
	    }
	    rm1[mp + mp * rm1_dim1] = eps3;
	    if (i__ < *n) {
		rm1[mp + (i__ + 1) * rm1_dim1] = 0.f;
	    }
	    if (i__ == *n) {
		z__[mp + (s - 1) * z_dim1] = 0.f;
	    }
	    t = 0.f;
	    x = eps3 * eps3;
L600:
	    w /= x;
	    x = rm1[mp + mp * rm1_dim1] * w;
	    y = -t * w;

	    i__3 = uk;
	    for (j = i__; j <= i__3; ++j) {
		if (j < n1) {
		    goto L610;
		}
		l = j - ns;
		t = z__[mp + l * z_dim1];
		z__[i__ + l * z_dim1] = -x * t - y * rm1[j + mp * rm1_dim1];
		goto L615;
L610:
		t = rm1[mp + (j + 2) * rm1_dim1];
		rm1[i__ + (j + 2) * rm1_dim1] = -x * t - y * rm1[j + mp * 
			rm1_dim1];
L615:
		rm1[j + i__ * rm1_dim1] = rm1[j + i__ * rm1_dim1] - x * rm1[j 
			+ mp * rm1_dim1] + y * t;
/* L620: */
	    }

	    if (i__ < n1) {
		goto L630;
	    }
	    l = i__ - ns;
	    z__[i__ + l * z_dim1] -= ilambd;
	    goto L640;
L630:
	    rm1[i__ + (i__ + 2) * rm1_dim1] -= ilambd;
L640:
	    ;
	}

	if (uk < n1) {
	    goto L650;
	}
	l = uk - ns;
	t = z__[uk + l * z_dim1];
	goto L655;
L650:
	t = rm1[uk + (uk + 2) * rm1_dim1];
L655:
	if (rm1[uk + uk * rm1_dim1] == 0.f && t == 0.f) {
	    rm1[uk + uk * rm1_dim1] = eps3;
	}
/*     .......... back substitution for complex vector */
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
		if (j < n1) {
		    goto L670;
		}
		l = j - ns;
		t = z__[i__ + l * z_dim1];
		goto L675;
L670:
		t = rm1[i__ + (j + 2) * rm1_dim1];
L675:
		x = x - rm1[j + i__ * rm1_dim1] * rv1[j] + t * rv2[j];
		y = y - rm1[j + i__ * rm1_dim1] * rv2[j] - t * rv1[j];
/* L680: */
	    }

L700:
	    if (i__ < n1) {
		goto L710;
	    }
	    l = i__ - ns;
	    t = z__[i__ + l * z_dim1];
	    goto L715;
L710:
	    t = rm1[i__ + (i__ + 2) * rm1_dim1];
L715:
	    cdiv_(&x, &y, &rm1[i__ + i__ * rm1_dim1], &t, &rv1[i__], &rv2[i__]
		    );
/* L720: */
	}
/*     .......... acceptance test for real or complex */
/*                eigenvector and normalization .......... */
L740:
	++its;
	norm = 0.f;
	normv = 0.f;

	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    if (ilambd == 0.f) {
		x = (r__1 = rv1[i__], dabs(r__1));
	    }
	    if (ilambd != 0.f) {
		x = pythag_(&rv1[i__], &rv2[i__]);
	    }
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
	if (ilambd == 0.f) {
	    x = 1.f / x;
	}
	if (ilambd != 0.f) {
	    y = rv2[j];
	}

	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    if (ilambd != 0.f) {
		goto L800;
	    }
	    z__[i__ + s * z_dim1] = rv1[i__] * x;
	    goto L820;
L800:
	    cdiv_(&rv1[i__], &rv2[i__], &x, &y, &z__[i__ + (s - 1) * z_dim1], 
		    &z__[i__ + s * z_dim1]);
L820:
	    ;
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
	if (ilambd == 0.f) {
	    goto L440;
	}
	goto L660;
/*     .......... set error -- unaccepted eigenvector .......... */
L880:
	j = 1;
	*ierr = -k;
/*     .......... set remaining vector components to zero .......... */
L900:
	i__2 = *n;
	for (i__ = j; i__ <= i__2; ++i__) {
	    z__[i__ + s * z_dim1] = 0.f;
	    if (ilambd != 0.f) {
		z__[i__ + (s - 1) * z_dim1] = 0.f;
	    }
/* L920: */
	}

L940:
	++s;
L960:
	if (ip == -1) {
	    ip = 0;
	}
	if (ip == 1) {
	    ip = -1;
	}
/* L980: */
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
    *m = s - 1 - abs(ip);
    return 0;
} /* invit_ */

