/* bandv.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int bandv_(integer *nm, integer *n, integer *mbw, real *a, 
	real *e21, integer *m, real *w, real *z__, integer *ierr, integer *nv,
	 real *rv, real *rv6)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2, i__3, i__4, i__5;
    real r__1;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static integer i__, j, k, r__;
    static real u, v;
    static integer m1;
    static real x0, x1;
    static integer mb, m21, ii, ij, jj, kj;
    static real uk, xu;
    static integer ij1, kj1, its;
    static real eps2, eps3, eps4;
    static integer maxj, maxk;
    static real norm, order;
    static integer group;
    extern doublereal pythag_(real *, real *), epslon_(real *);



/*     this subroutine finds those eigenvectors of a real symmetric */
/*     band matrix corresponding to specified eigenvalues, using inverse */
/*     iteration.  the subroutine may also be used to solve systems */
/*     of linear equations with a symmetric or non-symmetric band */
/*     coefficient matrix. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        mbw is the number of columns of the array a used to store the */
/*          band matrix.  if the matrix is symmetric, mbw is its (half) */
/*          band width, denoted mb and defined as the number of adjacent */
/*          diagonals, including the principal diagonal, required to */
/*          specify the non-zero portion of the lower triangle of the */
/*          matrix.  if the subroutine is being used to solve systems */
/*          of linear equations and the coefficient matrix is not */
/*          symmetric, it must however have the same number of adjacent */
/*          diagonals above the main diagonal as below, and in this */
/*          case, mbw=2*mb-1. */

/*        a contains the lower triangle of the symmetric band input */
/*          matrix stored as an n by mb array.  its lowest subdiagonal */
/*          is stored in the last n+1-mb positions of the first column, */
/*          its next subdiagonal in the last n+2-mb positions of the */
/*          second column, further subdiagonals similarly, and finally */
/*          its principal diagonal in the n positions of column mb. */
/*          if the subroutine is being used to solve systems of linear */
/*          equations and the coefficient matrix is not symmetric, a is */
/*          n by 2*mb-1 instead with lower triangle as above and with */
/*          its first superdiagonal stored in the first n-1 positions of */
/*          column mb+1, its second superdiagonal in the first n-2 */
/*          positions of column mb+2, further superdiagonals similarly, */
/*          and finally its highest superdiagonal in the first n+1-mb */
/*          positions of the last column. */
/*          contents of storages not part of the matrix are arbitrary. */

/*        e21 specifies the ordering of the eigenvalues and contains */
/*            0.0e0 if the eigenvalues are in ascending order, or */
/*            2.0e0 if the eigenvalues are in descending order. */
/*          if the subroutine is being used to solve systems of linear */
/*          equations, e21 should be set to 1.0e0 if the coefficient */
/*          matrix is symmetric and to -1.0e0 if not. */

/*        m is the number of specified eigenvalues or the number of */
/*          systems of linear equations. */

/*        w contains the m eigenvalues in ascending or descending order. */
/*          if the subroutine is being used to solve systems of linear */
/*          equations (a-w(r)*i)*x(r)=b(r), where i is the identity */
/*          matrix, w(r) should be set accordingly, for r=1,2,...,m. */

/*        z contains the constant matrix columns (b(r),r=1,2,...,m), if */
/*          the subroutine is used to solve systems of linear equations. */

/*        nv must be set to the dimension of the array parameter rv */
/*          as declared in the calling program dimension statement. */

/*     on output */

/*        a and w are unaltered. */

/*        z contains the associated set of orthogonal eigenvectors. */
/*          any vector which fails to converge is set to zero.  if the */
/*          subroutine is used to solve systems of linear equations, */
/*          z contains the solution matrix columns (x(r),r=1,2,...,m). */

/*        ierr is set to */
/*          zero       for normal return, */
/*          -r         if the eigenvector corresponding to the r-th */
/*                     eigenvalue fails to converge, or if the r-th */
/*                     system of linear equations is nearly singular. */

/*        rv and rv6 are temporary storage arrays.  note that rv is */
/*          of dimension at least n*(2*mb-1).  if the subroutine */
/*          is being used to solve systems of linear equations, the */
/*          determinant (up to sign) of a-w(m)*i is available, upon */
/*          return, as the product of the first n elements of rv. */

/*     calls pythag for  sqrt(a*a + b*b) . */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --rv6;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --w;
    --rv;

    /* Function Body */
    *ierr = 0;
    if (*m == 0) {
	goto L1001;
    }
    mb = *mbw;
    if (*e21 < 0.f) {
	mb = (*mbw + 1) / 2;
    }
    m1 = mb - 1;
    m21 = m1 + mb;
    order = 1.f - dabs(*e21);
/*     .......... find vectors by inverse iteration .......... */
    i__1 = *m;
    for (r__ = 1; r__ <= i__1; ++r__) {
	its = 1;
	x1 = w[r__];
	if (r__ != 1) {
	    goto L100;
	}
/*     .......... compute norm of matrix .......... */
	norm = 0.f;

	i__2 = mb;
	for (j = 1; j <= i__2; ++j) {
	    jj = mb + 1 - j;
	    kj = jj + m1;
	    ij = 1;
	    v = 0.f;

	    i__3 = *n;
	    for (i__ = jj; i__ <= i__3; ++i__) {
		v += (r__1 = a[i__ + j * a_dim1], dabs(r__1));
		if (*e21 >= 0.f) {
		    goto L40;
		}
		v += (r__1 = a[ij + kj * a_dim1], dabs(r__1));
		++ij;
L40:
		;
	    }

	    norm = dmax(norm,v);
/* L60: */
	}

	if (*e21 < 0.f) {
	    norm *= .5f;
	}
/*     .......... eps2 is the criterion for grouping, */
/*                eps3 replaces zero pivots and equal */
/*                roots are modified by eps3, */
/*                eps4 is taken very small to avoid overflow .......... */
	if (norm == 0.f) {
	    norm = 1.f;
	}
	eps2 = norm * .001f * dabs(order);
	eps3 = epslon_(&norm);
	uk = (real) (*n);
	uk = sqrt(uk);
	eps4 = uk * eps3;
L80:
	group = 0;
	goto L120;
/*     .......... look for close or coincident roots .......... */
L100:
	if ((r__1 = x1 - x0, dabs(r__1)) >= eps2) {
	    goto L80;
	}
	++group;
	if (order * (x1 - x0) <= 0.f) {
	    x1 = x0 + order * eps3;
	}
/*     .......... expand matrix, subtract eigenvalue, */
/*                and initialize vector .......... */
L120:
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* Computing MIN */
	    i__3 = 0, i__4 = i__ - m1;
	    ij = i__ + min(i__3,i__4) * *n;
	    kj = ij + mb * *n;
	    ij1 = kj + m1 * *n;
	    if (m1 == 0) {
		goto L180;
	    }

	    i__3 = m1;
	    for (j = 1; j <= i__3; ++j) {
		if (ij > m1) {
		    goto L125;
		}
		if (ij > 0) {
		    goto L130;
		}
		rv[ij1] = 0.f;
		ij1 += *n;
		goto L130;
L125:
		rv[ij] = a[i__ + j * a_dim1];
L130:
		ij += *n;
		ii = i__ + j;
		if (ii > *n) {
		    goto L150;
		}
		jj = mb - j;
		if (*e21 >= 0.f) {
		    goto L140;
		}
		ii = i__;
		jj = mb + j;
L140:
		rv[kj] = a[ii + jj * a_dim1];
		kj += *n;
L150:
		;
	    }

L180:
	    rv[ij] = a[i__ + mb * a_dim1] - x1;
	    rv6[i__] = eps4;
	    if (order == 0.f) {
		rv6[i__] = z__[i__ + r__ * z_dim1];
	    }
/* L200: */
	}

	if (m1 == 0) {
	    goto L600;
	}
/*     .......... elimination with interchanges .......... */
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    ii = i__ + 1;
/* Computing MIN */
	    i__3 = i__ + m1 - 1;
	    maxk = min(i__3,*n);
/* Computing MIN */
	    i__3 = *n - i__, i__4 = m21 - 2;
	    maxj = min(i__3,i__4) * *n;

	    i__3 = maxk;
	    for (k = i__; k <= i__3; ++k) {
		kj1 = k;
		j = kj1 + *n;
		jj = j + maxj;

		i__4 = jj;
		i__5 = *n;
		for (kj = j; i__5 < 0 ? kj >= i__4 : kj <= i__4; kj += i__5) {
		    rv[kj1] = rv[kj];
		    kj1 = kj;
/* L340: */
		}

		rv[kj1] = 0.f;
/* L360: */
	    }

	    if (i__ == *n) {
		goto L580;
	    }
	    u = 0.f;
/* Computing MIN */
	    i__3 = i__ + m1;
	    maxk = min(i__3,*n);
/* Computing MIN */
	    i__3 = *n - ii, i__5 = m21 - 2;
	    maxj = min(i__3,i__5) * *n;

	    i__3 = maxk;
	    for (j = i__; j <= i__3; ++j) {
		if ((r__1 = rv[j], dabs(r__1)) < dabs(u)) {
		    goto L450;
		}
		u = rv[j];
		k = j;
L450:
		;
	    }

	    j = i__ + *n;
	    jj = j + maxj;
	    if (k == i__) {
		goto L520;
	    }
	    kj = k;

	    i__3 = jj;
	    i__5 = *n;
	    for (ij = i__; i__5 < 0 ? ij >= i__3 : ij <= i__3; ij += i__5) {
		v = rv[ij];
		rv[ij] = rv[kj];
		rv[kj] = v;
		kj += *n;
/* L500: */
	    }

	    if (order != 0.f) {
		goto L520;
	    }
	    v = rv6[i__];
	    rv6[i__] = rv6[k];
	    rv6[k] = v;
L520:
	    if (u == 0.f) {
		goto L580;
	    }

	    i__5 = maxk;
	    for (k = ii; k <= i__5; ++k) {
		v = rv[k] / u;
		kj = k;

		i__3 = jj;
		i__4 = *n;
		for (ij = j; i__4 < 0 ? ij >= i__3 : ij <= i__3; ij += i__4) {
		    kj += *n;
		    rv[kj] -= v * rv[ij];
/* L540: */
		}

		if (order == 0.f) {
		    rv6[k] -= v * rv6[i__];
		}
/* L560: */
	    }

L580:
	    ;
	}
/*     .......... back substitution */
/*                for i=n step -1 until 1 do -- .......... */
L600:
	i__2 = *n;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = *n + 1 - ii;
	    maxj = min(ii,m21);
	    if (maxj == 1) {
		goto L620;
	    }
	    ij1 = i__;
	    j = ij1 + *n;
	    jj = j + (maxj - 2) * *n;

	    i__5 = jj;
	    i__4 = *n;
	    for (ij = j; i__4 < 0 ? ij >= i__5 : ij <= i__5; ij += i__4) {
		++ij1;
		rv6[i__] -= rv[ij] * rv6[ij1];
/* L610: */
	    }

L620:
	    v = rv[i__];
	    if (dabs(v) >= eps3) {
		goto L625;
	    }
/*     .......... set error -- nearly singular linear system .......... */
	    if (order == 0.f) {
		*ierr = -r__;
	    }
	    v = r_sign(&eps3, &v);
L625:
	    rv6[i__] /= v;
/* L630: */
	}

	xu = 1.f;
	if (order == 0.f) {
	    goto L870;
	}
/*     .......... orthogonalize with respect to previous */
/*                members of group .......... */
	if (group == 0) {
	    goto L700;
	}

	i__2 = group;
	for (jj = 1; jj <= i__2; ++jj) {
	    j = r__ - group - 1 + jj;
	    xu = 0.f;

	    i__4 = *n;
	    for (i__ = 1; i__ <= i__4; ++i__) {
/* L640: */
		xu += rv6[i__] * z__[i__ + j * z_dim1];
	    }

	    i__4 = *n;
	    for (i__ = 1; i__ <= i__4; ++i__) {
/* L660: */
		rv6[i__] -= xu * z__[i__ + j * z_dim1];
	    }

/* L680: */
	}

L700:
	norm = 0.f;

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L720: */
	    norm += (r__1 = rv6[i__], dabs(r__1));
	}

	if (norm >= .1f) {
	    goto L840;
	}
/*     .......... in-line procedure for choosing */
/*                a new starting vector .......... */
	if (its >= *n) {
	    goto L830;
	}
	++its;
	xu = eps4 / (uk + 1.f);
	rv6[1] = eps4;

	i__2 = *n;
	for (i__ = 2; i__ <= i__2; ++i__) {
/* L760: */
	    rv6[i__] = xu;
	}

	rv6[its] -= eps4 * uk;
	goto L600;
/*     .......... set error -- non-converged eigenvector .......... */
L830:
	*ierr = -r__;
	xu = 0.f;
	goto L870;
/*     .......... normalize so that sum of squares is */
/*                1 and expand to full order .......... */
L840:
	u = 0.f;

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L860: */
	    u = pythag_(&u, &rv6[i__]);
	}

	xu = 1.f / u;

L870:
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L900: */
	    z__[i__ + r__ * z_dim1] = rv6[i__] * xu;
	}

	x0 = x1;
/* L920: */
    }

L1001:
    return 0;
} /* bandv_ */

