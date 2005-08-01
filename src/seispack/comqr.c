/* comqr.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int comqr_(integer *nm, integer *n, integer *low, integer *
	igh, real *hr, real *hi, real *wr, real *wi, integer *ierr)
{
    /* System generated locals */
    integer hr_dim1, hr_offset, hi_dim1, hi_offset, i__1, i__2;
    real r__1, r__2, r__3, r__4;

    /* Local variables */
    static integer i__, j, l, en, ll;
    static real si, ti, xi, yi, sr, tr, xr, yr;
    static integer lp1, itn, its;
    static real zzi, zzr;
    static integer enm1;
    static real tst1, tst2;
    extern /* Subroutine */ int cdiv_(real *, real *, real *, real *, real *, 
	    real *);
    static real norm;
    extern doublereal pythag_(real *, real *);
    extern /* Subroutine */ int csroot_(real *, real *, real *, real *);



/*     this subroutine is a translation of a unitary analogue of the */
/*     algol procedure  comlr, num. math. 12, 369-376(1968) by martin */
/*     and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 396-403(1971). */
/*     the unitary analogue substitutes the qr algorithm of francis */
/*     (comp. jour. 4, 332-345(1962)) for the lr algorithm. */

/*     this subroutine finds the eigenvalues of a complex */
/*     upper hessenberg matrix by the qr method. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        low and igh are integers determined by the balancing */
/*          subroutine  cbal.  if  cbal  has not been used, */
/*          set low=1, igh=n. */

/*        hr and hi contain the real and imaginary parts, */
/*          respectively, of the complex upper hessenberg matrix. */
/*          their lower triangles below the subdiagonal contain */
/*          information about the unitary transformations used in */
/*          the reduction by  corth, if performed. */

/*     on output */

/*        the upper hessenberg portions of hr and hi have been */
/*          destroyed.  therefore, they must be saved before */
/*          calling  comqr  if subsequent calculation of */
/*          eigenvectors is to be performed. */

/*        wr and wi contain the real and imaginary parts, */
/*          respectively, of the eigenvalues.  if an error */
/*          exit is made, the eigenvalues should be correct */
/*          for indices ierr+1,...,n. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          j          if the limit of 30*n iterations is exhausted */
/*                     while the j-th eigenvalue is being sought. */

/*     calls cdiv for complex division. */
/*     calls csroot for complex square root. */
/*     calls pythag for  sqrt(a*a + b*b) . */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --wi;
    --wr;
    hi_dim1 = *nm;
    hi_offset = 1 + hi_dim1;
    hi -= hi_offset;
    hr_dim1 = *nm;
    hr_offset = 1 + hr_dim1;
    hr -= hr_offset;

    /* Function Body */
    *ierr = 0;
    if (*low == *igh) {
	goto L180;
    }
/*     .......... create real subdiagonal elements .......... */
    l = *low + 1;

    i__1 = *igh;
    for (i__ = l; i__ <= i__1; ++i__) {
/* Computing MIN */
	i__2 = i__ + 1;
	ll = min(i__2,*igh);
	if (hi[i__ + (i__ - 1) * hi_dim1] == 0.f) {
	    goto L170;
	}
	norm = pythag_(&hr[i__ + (i__ - 1) * hr_dim1], &hi[i__ + (i__ - 1) * 
		hi_dim1]);
	yr = hr[i__ + (i__ - 1) * hr_dim1] / norm;
	yi = hi[i__ + (i__ - 1) * hi_dim1] / norm;
	hr[i__ + (i__ - 1) * hr_dim1] = norm;
	hi[i__ + (i__ - 1) * hi_dim1] = 0.f;

	i__2 = *igh;
	for (j = i__; j <= i__2; ++j) {
	    si = yr * hi[i__ + j * hi_dim1] - yi * hr[i__ + j * hr_dim1];
	    hr[i__ + j * hr_dim1] = yr * hr[i__ + j * hr_dim1] + yi * hi[i__ 
		    + j * hi_dim1];
	    hi[i__ + j * hi_dim1] = si;
/* L155: */
	}

	i__2 = ll;
	for (j = *low; j <= i__2; ++j) {
	    si = yr * hi[j + i__ * hi_dim1] + yi * hr[j + i__ * hr_dim1];
	    hr[j + i__ * hr_dim1] = yr * hr[j + i__ * hr_dim1] - yi * hi[j + 
		    i__ * hi_dim1];
	    hi[j + i__ * hi_dim1] = si;
/* L160: */
	}

L170:
	;
    }
/*     .......... store roots isolated by cbal .......... */
L180:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ >= *low && i__ <= *igh) {
	    goto L200;
	}
	wr[i__] = hr[i__ + i__ * hr_dim1];
	wi[i__] = hi[i__ + i__ * hi_dim1];
L200:
	;
    }

    en = *igh;
    tr = 0.f;
    ti = 0.f;
    itn = *n * 30;
/*     .......... search for next eigenvalue .......... */
L220:
    if (en < *low) {
	goto L1001;
    }
    its = 0;
    enm1 = en - 1;
/*     .......... look for single small sub-diagonal element */
/*                for l=en step -1 until low e0 -- .......... */
L240:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L300;
	}
	tst1 = (r__1 = hr[l - 1 + (l - 1) * hr_dim1], dabs(r__1)) + (r__2 = 
		hi[l - 1 + (l - 1) * hi_dim1], dabs(r__2)) + (r__3 = hr[l + l 
		* hr_dim1], dabs(r__3)) + (r__4 = hi[l + l * hi_dim1], dabs(
		r__4));
	tst2 = tst1 + (r__1 = hr[l + (l - 1) * hr_dim1], dabs(r__1));
	if (tst2 == tst1) {
	    goto L300;
	}
/* L260: */
    }
/*     .......... form shift .......... */
L300:
    if (l == en) {
	goto L660;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its == 10 || its == 20) {
	goto L320;
    }
    sr = hr[en + en * hr_dim1];
    si = hi[en + en * hi_dim1];
    xr = hr[enm1 + en * hr_dim1] * hr[en + enm1 * hr_dim1];
    xi = hi[enm1 + en * hi_dim1] * hr[en + enm1 * hr_dim1];
    if (xr == 0.f && xi == 0.f) {
	goto L340;
    }
    yr = (hr[enm1 + enm1 * hr_dim1] - sr) / 2.f;
    yi = (hi[enm1 + enm1 * hi_dim1] - si) / 2.f;
/* Computing 2nd power */
    r__2 = yr;
/* Computing 2nd power */
    r__3 = yi;
    r__1 = r__2 * r__2 - r__3 * r__3 + xr;
    r__4 = yr * 2.f * yi + xi;
    csroot_(&r__1, &r__4, &zzr, &zzi);
    if (yr * zzr + yi * zzi >= 0.f) {
	goto L310;
    }
    zzr = -zzr;
    zzi = -zzi;
L310:
    r__1 = yr + zzr;
    r__2 = yi + zzi;
    cdiv_(&xr, &xi, &r__1, &r__2, &xr, &xi);
    sr -= xr;
    si -= xi;
    goto L340;
/*     .......... form exceptional shift .......... */
L320:
    sr = (r__1 = hr[en + enm1 * hr_dim1], dabs(r__1)) + (r__2 = hr[enm1 + (en 
	    - 2) * hr_dim1], dabs(r__2));
    si = 0.f;

L340:
    i__1 = en;
    for (i__ = *low; i__ <= i__1; ++i__) {
	hr[i__ + i__ * hr_dim1] -= sr;
	hi[i__ + i__ * hi_dim1] -= si;
/* L360: */
    }

    tr += sr;
    ti += si;
    ++its;
    --itn;
/*     .......... reduce to triangle (rows) .......... */
    lp1 = l + 1;

    i__1 = en;
    for (i__ = lp1; i__ <= i__1; ++i__) {
	sr = hr[i__ + (i__ - 1) * hr_dim1];
	hr[i__ + (i__ - 1) * hr_dim1] = 0.f;
	r__1 = pythag_(&hr[i__ - 1 + (i__ - 1) * hr_dim1], &hi[i__ - 1 + (i__ 
		- 1) * hi_dim1]);
	norm = pythag_(&r__1, &sr);
	xr = hr[i__ - 1 + (i__ - 1) * hr_dim1] / norm;
	wr[i__ - 1] = xr;
	xi = hi[i__ - 1 + (i__ - 1) * hi_dim1] / norm;
	wi[i__ - 1] = xi;
	hr[i__ - 1 + (i__ - 1) * hr_dim1] = norm;
	hi[i__ - 1 + (i__ - 1) * hi_dim1] = 0.f;
	hi[i__ + (i__ - 1) * hi_dim1] = sr / norm;

	i__2 = en;
	for (j = i__; j <= i__2; ++j) {
	    yr = hr[i__ - 1 + j * hr_dim1];
	    yi = hi[i__ - 1 + j * hi_dim1];
	    zzr = hr[i__ + j * hr_dim1];
	    zzi = hi[i__ + j * hi_dim1];
	    hr[i__ - 1 + j * hr_dim1] = xr * yr + xi * yi + hi[i__ + (i__ - 1)
		     * hi_dim1] * zzr;
	    hi[i__ - 1 + j * hi_dim1] = xr * yi - xi * yr + hi[i__ + (i__ - 1)
		     * hi_dim1] * zzi;
	    hr[i__ + j * hr_dim1] = xr * zzr - xi * zzi - hi[i__ + (i__ - 1) *
		     hi_dim1] * yr;
	    hi[i__ + j * hi_dim1] = xr * zzi + xi * zzr - hi[i__ + (i__ - 1) *
		     hi_dim1] * yi;
/* L490: */
	}

/* L500: */
    }

    si = hi[en + en * hi_dim1];
    if (si == 0.f) {
	goto L540;
    }
    norm = pythag_(&hr[en + en * hr_dim1], &si);
    sr = hr[en + en * hr_dim1] / norm;
    si /= norm;
    hr[en + en * hr_dim1] = norm;
    hi[en + en * hi_dim1] = 0.f;
/*     .......... inverse operation (columns) .......... */
L540:
    i__1 = en;
    for (j = lp1; j <= i__1; ++j) {
	xr = wr[j - 1];
	xi = wi[j - 1];

	i__2 = j;
	for (i__ = l; i__ <= i__2; ++i__) {
	    yr = hr[i__ + (j - 1) * hr_dim1];
	    yi = 0.f;
	    zzr = hr[i__ + j * hr_dim1];
	    zzi = hi[i__ + j * hi_dim1];
	    if (i__ == j) {
		goto L560;
	    }
	    yi = hi[i__ + (j - 1) * hi_dim1];
	    hi[i__ + (j - 1) * hi_dim1] = xr * yi + xi * yr + hi[j + (j - 1) *
		     hi_dim1] * zzi;
L560:
	    hr[i__ + (j - 1) * hr_dim1] = xr * yr - xi * yi + hi[j + (j - 1) *
		     hi_dim1] * zzr;
	    hr[i__ + j * hr_dim1] = xr * zzr + xi * zzi - hi[j + (j - 1) * 
		    hi_dim1] * yr;
	    hi[i__ + j * hi_dim1] = xr * zzi - xi * zzr - hi[j + (j - 1) * 
		    hi_dim1] * yi;
/* L580: */
	}

/* L600: */
    }

    if (si == 0.f) {
	goto L240;
    }

    i__1 = en;
    for (i__ = l; i__ <= i__1; ++i__) {
	yr = hr[i__ + en * hr_dim1];
	yi = hi[i__ + en * hi_dim1];
	hr[i__ + en * hr_dim1] = sr * yr - si * yi;
	hi[i__ + en * hi_dim1] = sr * yi + si * yr;
/* L630: */
    }

    goto L240;
/*     .......... a root found .......... */
L660:
    wr[en] = hr[en + en * hr_dim1] + tr;
    wi[en] = hi[en + en * hi_dim1] + ti;
    en = enm1;
    goto L220;
/*     .......... set error -- all eigenvalues have not */
/*                converged after 30*n iterations .......... */
L1000:
    *ierr = en;
L1001:
    return 0;
} /* comqr_ */

