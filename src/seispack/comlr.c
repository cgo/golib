/* comlr.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int comlr_(integer *nm, integer *n, integer *low, integer *
	igh, real *hr, real *hi, real *wr, real *wi, integer *ierr)
{
    /* System generated locals */
    integer hr_dim1, hr_offset, hi_dim1, hi_offset, i__1, i__2;
    real r__1, r__2, r__3, r__4;

    /* Local variables */
    static integer i__, j, l, m, en, ll, mm;
    static real si, ti, xi, yi, sr, tr, xr, yr;
    static integer im1, mp1, itn, its;
    static real zzi, zzr;
    static integer enm1;
    static real tst1, tst2;
    extern /* Subroutine */ int cdiv_(real *, real *, real *, real *, real *, 
	    real *), csroot_(real *, real *, real *, real *);



/*     this subroutine is a translation of the algol procedure comlr, */
/*     num. math. 12, 369-376(1968) by martin and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 396-403(1971). */

/*     this subroutine finds the eigenvalues of a complex */
/*     upper hessenberg matrix by the modified lr method. */

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
/*          their lower triangles below the subdiagonal contain the */
/*          multipliers which were used in the reduction by  comhes, */
/*          if performed. */

/*     on output */

/*        the upper hessenberg portions of hr and hi have been */
/*          destroyed.  therefore, they must be saved before */
/*          calling  comlr  if subsequent calculation of */
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
/*     .......... store roots isolated by cbal .......... */
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
	tst2 = tst1 + (r__1 = hr[l + (l - 1) * hr_dim1], dabs(r__1)) + (r__2 =
		 hi[l + (l - 1) * hi_dim1], dabs(r__2));
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
    xr = hr[enm1 + en * hr_dim1] * hr[en + enm1 * hr_dim1] - hi[enm1 + en * 
	    hi_dim1] * hi[en + enm1 * hi_dim1];
    xi = hr[enm1 + en * hr_dim1] * hi[en + enm1 * hi_dim1] + hi[enm1 + en * 
	    hi_dim1] * hr[en + enm1 * hr_dim1];
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
    si = (r__1 = hi[en + enm1 * hi_dim1], dabs(r__1)) + (r__2 = hi[enm1 + (en 
	    - 2) * hi_dim1], dabs(r__2));

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
/*     .......... look for two consecutive small */
/*                sub-diagonal elements .......... */
    xr = (r__1 = hr[enm1 + enm1 * hr_dim1], dabs(r__1)) + (r__2 = hi[enm1 + 
	    enm1 * hi_dim1], dabs(r__2));
    yr = (r__1 = hr[en + enm1 * hr_dim1], dabs(r__1)) + (r__2 = hi[en + enm1 *
	     hi_dim1], dabs(r__2));
    zzr = (r__1 = hr[en + en * hr_dim1], dabs(r__1)) + (r__2 = hi[en + en * 
	    hi_dim1], dabs(r__2));
/*     .......... for m=en-1 step -1 until l do -- .......... */
    i__1 = enm1;
    for (mm = l; mm <= i__1; ++mm) {
	m = enm1 + l - mm;
	if (m == l) {
	    goto L420;
	}
	yi = yr;
	yr = (r__1 = hr[m + (m - 1) * hr_dim1], dabs(r__1)) + (r__2 = hi[m + (
		m - 1) * hi_dim1], dabs(r__2));
	xi = zzr;
	zzr = xr;
	xr = (r__1 = hr[m - 1 + (m - 1) * hr_dim1], dabs(r__1)) + (r__2 = hi[
		m - 1 + (m - 1) * hi_dim1], dabs(r__2));
	tst1 = zzr / yi * (zzr + xr + xi);
	tst2 = tst1 + yr;
	if (tst2 == tst1) {
	    goto L420;
	}
/* L380: */
    }
/*     .......... triangular decomposition h=l*r .......... */
L420:
    mp1 = m + 1;

    i__1 = en;
    for (i__ = mp1; i__ <= i__1; ++i__) {
	im1 = i__ - 1;
	xr = hr[im1 + im1 * hr_dim1];
	xi = hi[im1 + im1 * hi_dim1];
	yr = hr[i__ + im1 * hr_dim1];
	yi = hi[i__ + im1 * hi_dim1];
	if (dabs(xr) + dabs(xi) >= dabs(yr) + dabs(yi)) {
	    goto L460;
	}
/*     .......... interchange rows of hr and hi .......... */
	i__2 = en;
	for (j = im1; j <= i__2; ++j) {
	    zzr = hr[im1 + j * hr_dim1];
	    hr[im1 + j * hr_dim1] = hr[i__ + j * hr_dim1];
	    hr[i__ + j * hr_dim1] = zzr;
	    zzi = hi[im1 + j * hi_dim1];
	    hi[im1 + j * hi_dim1] = hi[i__ + j * hi_dim1];
	    hi[i__ + j * hi_dim1] = zzi;
/* L440: */
	}

	cdiv_(&xr, &xi, &yr, &yi, &zzr, &zzi);
	wr[i__] = 1.f;
	goto L480;
L460:
	cdiv_(&yr, &yi, &xr, &xi, &zzr, &zzi);
	wr[i__] = -1.f;
L480:
	hr[i__ + im1 * hr_dim1] = zzr;
	hi[i__ + im1 * hi_dim1] = zzi;

	i__2 = en;
	for (j = i__; j <= i__2; ++j) {
	    hr[i__ + j * hr_dim1] = hr[i__ + j * hr_dim1] - zzr * hr[im1 + j *
		     hr_dim1] + zzi * hi[im1 + j * hi_dim1];
	    hi[i__ + j * hi_dim1] = hi[i__ + j * hi_dim1] - zzr * hi[im1 + j *
		     hi_dim1] - zzi * hr[im1 + j * hr_dim1];
/* L500: */
	}

/* L520: */
    }
/*     .......... composition r*l=h .......... */
    i__1 = en;
    for (j = mp1; j <= i__1; ++j) {
	xr = hr[j + (j - 1) * hr_dim1];
	xi = hi[j + (j - 1) * hi_dim1];
	hr[j + (j - 1) * hr_dim1] = 0.f;
	hi[j + (j - 1) * hi_dim1] = 0.f;
/*     .......... interchange columns of hr and hi, */
/*                if necessary .......... */
	if (wr[j] <= 0.f) {
	    goto L580;
	}

	i__2 = j;
	for (i__ = l; i__ <= i__2; ++i__) {
	    zzr = hr[i__ + (j - 1) * hr_dim1];
	    hr[i__ + (j - 1) * hr_dim1] = hr[i__ + j * hr_dim1];
	    hr[i__ + j * hr_dim1] = zzr;
	    zzi = hi[i__ + (j - 1) * hi_dim1];
	    hi[i__ + (j - 1) * hi_dim1] = hi[i__ + j * hi_dim1];
	    hi[i__ + j * hi_dim1] = zzi;
/* L540: */
	}

L580:
	i__2 = j;
	for (i__ = l; i__ <= i__2; ++i__) {
	    hr[i__ + (j - 1) * hr_dim1] = hr[i__ + (j - 1) * hr_dim1] + xr * 
		    hr[i__ + j * hr_dim1] - xi * hi[i__ + j * hi_dim1];
	    hi[i__ + (j - 1) * hi_dim1] = hi[i__ + (j - 1) * hi_dim1] + xr * 
		    hi[i__ + j * hi_dim1] + xi * hr[i__ + j * hr_dim1];
/* L600: */
	}

/* L640: */
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
} /* comlr_ */

