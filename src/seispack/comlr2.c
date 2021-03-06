/* comlr2.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int comlr2_(integer *nm, integer *n, integer *low, integer *
	igh, integer *int__, real *hr, real *hi, real *wr, real *wi, real *zr,
	 real *zi, integer *ierr)
{
    /* System generated locals */
    integer hr_dim1, hr_offset, hi_dim1, hi_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset, i__1, i__2, i__3;
    real r__1, r__2, r__3, r__4;

    /* Local variables */
    static integer i__, j, k, l, m, ii, en, jj, ll, mm, nn;
    static real si, ti, xi, yi, sr, tr, xr, yr;
    static integer im1, ip1, mp1, itn, its;
    static real zzi, zzr;
    static integer enm1;
    static real tst1, tst2;
    static integer iend;
    extern /* Subroutine */ int cdiv_(real *, real *, real *, real *, real *, 
	    real *);
    static real norm;
    extern /* Subroutine */ int csroot_(real *, real *, real *, real *);

/*  MESHED overflow control WITH vectors of isolated roots (10/19/89 BSG) */
/*  MESHED overflow control WITH triangular multiply (10/30/89 BSG) */


/*     this subroutine is a translation of the algol procedure comlr2, */
/*     num. math. 16, 181-204(1970) by peters and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 372-395(1971). */

/*     this subroutine finds the eigenvalues and eigenvectors */
/*     of a complex upper hessenberg matrix by the modified lr */
/*     method.  the eigenvectors of a complex general matrix */
/*     can also be found if  comhes  has been used to reduce */
/*     this general matrix to hessenberg form. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        low and igh are integers determined by the balancing */
/*          subroutine  cbal.  if  cbal  has not been used, */
/*          set low=1, igh=n. */

/*        int contains information on the rows and columns interchanged */
/*          in the reduction by  comhes, if performed.  only elements */
/*          low through igh are used.  if the eigenvectors of the hessen- */
/*          berg matrix are desired, set int(j)=j for these elements. */

/*        hr and hi contain the real and imaginary parts, */
/*          respectively, of the complex upper hessenberg matrix. */
/*          their lower triangles below the subdiagonal contain the */
/*          multipliers which were used in the reduction by  comhes, */
/*          if performed.  if the eigenvectors of the hessenberg */
/*          matrix are desired, these elements must be set to zero. */

/*     on output */

/*        the upper hessenberg portions of hr and hi have been */
/*          destroyed, but the location hr(1,1) contains the norm */
/*          of the triangularized matrix. */

/*        wr and wi contain the real and imaginary parts, */
/*          respectively, of the eigenvalues.  if an error */
/*          exit is made, the eigenvalues should be correct */
/*          for indices ierr+1,...,n. */

/*        zr and zi contain the real and imaginary parts, */
/*          respectively, of the eigenvectors.  the eigenvectors */
/*          are unnormalized.  if an error exit is made, none of */
/*          the eigenvectors has been found. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          j          if the limit of 30*n iterations is exhausted */
/*                     while the j-th eigenvalue is being sought. */


/*     calls cdiv for complex division. */
/*     calls csroot for complex square root. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated october 1989. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    zi_dim1 = *nm;
    zi_offset = 1 + zi_dim1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = 1 + zr_dim1;
    zr -= zr_offset;
    --wi;
    --wr;
    hi_dim1 = *nm;
    hi_offset = 1 + hi_dim1;
    hi -= hi_offset;
    hr_dim1 = *nm;
    hr_offset = 1 + hr_dim1;
    hr -= hr_offset;
    --int__;

    /* Function Body */
    *ierr = 0;
/*     .......... initialize eigenvector matrix .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    zr[i__ + j * zr_dim1] = 0.f;
	    zi[i__ + j * zi_dim1] = 0.f;
	    if (i__ == j) {
		zr[i__ + j * zr_dim1] = 1.f;
	    }
/* L100: */
	}
    }
/*     .......... form the matrix of accumulated transformations */
/*                from the information left by comhes .......... */
    iend = *igh - *low - 1;
    if (iend <= 0) {
	goto L180;
    }
/*     .......... for i=igh-1 step -1 until low+1 do -- .......... */
    i__2 = iend;
    for (ii = 1; ii <= i__2; ++ii) {
	i__ = *igh - ii;
	ip1 = i__ + 1;

	i__1 = *igh;
	for (k = ip1; k <= i__1; ++k) {
	    zr[k + i__ * zr_dim1] = hr[k + (i__ - 1) * hr_dim1];
	    zi[k + i__ * zi_dim1] = hi[k + (i__ - 1) * hi_dim1];
/* L120: */
	}

	j = int__[i__];
	if (i__ == j) {
	    goto L160;
	}

	i__1 = *igh;
	for (k = i__; k <= i__1; ++k) {
	    zr[i__ + k * zr_dim1] = zr[j + k * zr_dim1];
	    zi[i__ + k * zi_dim1] = zi[j + k * zi_dim1];
	    zr[j + k * zr_dim1] = 0.f;
	    zi[j + k * zi_dim1] = 0.f;
/* L140: */
	}

	zr[j + i__ * zr_dim1] = 1.f;
L160:
	;
    }
/*     .......... store roots isolated by cbal .......... */
L180:
    i__2 = *n;
    for (i__ = 1; i__ <= i__2; ++i__) {
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
	goto L680;
    }
    its = 0;
    enm1 = en - 1;
/*     .......... look for single small sub-diagonal element */
/*                for l=en step -1 until low do -- .......... */
L240:
    i__2 = en;
    for (ll = *low; ll <= i__2; ++ll) {
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
    i__2 = en;
    for (i__ = *low; i__ <= i__2; ++i__) {
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
    i__2 = enm1;
    for (mm = l; mm <= i__2; ++mm) {
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

    i__2 = en;
    for (i__ = mp1; i__ <= i__2; ++i__) {
	im1 = i__ - 1;
	xr = hr[im1 + im1 * hr_dim1];
	xi = hi[im1 + im1 * hi_dim1];
	yr = hr[i__ + im1 * hr_dim1];
	yi = hi[i__ + im1 * hi_dim1];
	if (dabs(xr) + dabs(xi) >= dabs(yr) + dabs(yi)) {
	    goto L460;
	}
/*     .......... interchange rows of hr and hi .......... */
	i__1 = *n;
	for (j = im1; j <= i__1; ++j) {
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

	i__1 = *n;
	for (j = i__; j <= i__1; ++j) {
	    hr[i__ + j * hr_dim1] = hr[i__ + j * hr_dim1] - zzr * hr[im1 + j *
		     hr_dim1] + zzi * hi[im1 + j * hi_dim1];
	    hi[i__ + j * hi_dim1] = hi[i__ + j * hi_dim1] - zzr * hi[im1 + j *
		     hi_dim1] - zzi * hr[im1 + j * hr_dim1];
/* L500: */
	}

/* L520: */
    }
/*     .......... composition r*l=h .......... */
    i__2 = en;
    for (j = mp1; j <= i__2; ++j) {
	xr = hr[j + (j - 1) * hr_dim1];
	xi = hi[j + (j - 1) * hi_dim1];
	hr[j + (j - 1) * hr_dim1] = 0.f;
	hi[j + (j - 1) * hi_dim1] = 0.f;
/*     .......... interchange columns of hr, hi, zr, and zi, */
/*                if necessary .......... */
	if (wr[j] <= 0.f) {
	    goto L580;
	}

	i__1 = j;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    zzr = hr[i__ + (j - 1) * hr_dim1];
	    hr[i__ + (j - 1) * hr_dim1] = hr[i__ + j * hr_dim1];
	    hr[i__ + j * hr_dim1] = zzr;
	    zzi = hi[i__ + (j - 1) * hi_dim1];
	    hi[i__ + (j - 1) * hi_dim1] = hi[i__ + j * hi_dim1];
	    hi[i__ + j * hi_dim1] = zzi;
/* L540: */
	}

	i__1 = *igh;
	for (i__ = *low; i__ <= i__1; ++i__) {
	    zzr = zr[i__ + (j - 1) * zr_dim1];
	    zr[i__ + (j - 1) * zr_dim1] = zr[i__ + j * zr_dim1];
	    zr[i__ + j * zr_dim1] = zzr;
	    zzi = zi[i__ + (j - 1) * zi_dim1];
	    zi[i__ + (j - 1) * zi_dim1] = zi[i__ + j * zi_dim1];
	    zi[i__ + j * zi_dim1] = zzi;
/* L560: */
	}

L580:
	i__1 = j;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    hr[i__ + (j - 1) * hr_dim1] = hr[i__ + (j - 1) * hr_dim1] + xr * 
		    hr[i__ + j * hr_dim1] - xi * hi[i__ + j * hi_dim1];
	    hi[i__ + (j - 1) * hi_dim1] = hi[i__ + (j - 1) * hi_dim1] + xr * 
		    hi[i__ + j * hi_dim1] + xi * hr[i__ + j * hr_dim1];
/* L600: */
	}
/*     .......... accumulate transformations .......... */
	i__1 = *igh;
	for (i__ = *low; i__ <= i__1; ++i__) {
	    zr[i__ + (j - 1) * zr_dim1] = zr[i__ + (j - 1) * zr_dim1] + xr * 
		    zr[i__ + j * zr_dim1] - xi * zi[i__ + j * zi_dim1];
	    zi[i__ + (j - 1) * zi_dim1] = zi[i__ + (j - 1) * zi_dim1] + xr * 
		    zi[i__ + j * zi_dim1] + xi * zr[i__ + j * zr_dim1];
/* L620: */
	}

/* L640: */
    }

    goto L240;
/*     .......... a root found .......... */
L660:
    hr[en + en * hr_dim1] += tr;
    wr[en] = hr[en + en * hr_dim1];
    hi[en + en * hi_dim1] += ti;
    wi[en] = hi[en + en * hi_dim1];
    en = enm1;
    goto L220;
/*     .......... all roots found.  backsubstitute to find */
/*                vectors of upper triangular form .......... */
L680:
    norm = 0.f;

    i__2 = *n;
    for (i__ = 1; i__ <= i__2; ++i__) {

	i__1 = *n;
	for (j = i__; j <= i__1; ++j) {
	    tr = (r__1 = hr[i__ + j * hr_dim1], dabs(r__1)) + (r__2 = hi[i__ 
		    + j * hi_dim1], dabs(r__2));
	    if (tr > norm) {
		norm = tr;
	    }
/* L720: */
	}
    }

    hr[hr_dim1 + 1] = norm;
    if (*n == 1 || norm == 0.f) {
	goto L1001;
    }
/*     .......... for en=n step -1 until 2 do -- .......... */
    i__1 = *n;
    for (nn = 2; nn <= i__1; ++nn) {
	en = *n + 2 - nn;
	xr = wr[en];
	xi = wi[en];
	hr[en + en * hr_dim1] = 1.f;
	hi[en + en * hi_dim1] = 0.f;
	enm1 = en - 1;
/*     .......... for i=en-1 step -1 until 1 do -- .......... */
	i__2 = enm1;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = en - ii;
	    zzr = 0.f;
	    zzi = 0.f;
	    ip1 = i__ + 1;

	    i__3 = en;
	    for (j = ip1; j <= i__3; ++j) {
		zzr = zzr + hr[i__ + j * hr_dim1] * hr[j + en * hr_dim1] - hi[
			i__ + j * hi_dim1] * hi[j + en * hi_dim1];
		zzi = zzi + hr[i__ + j * hr_dim1] * hi[j + en * hi_dim1] + hi[
			i__ + j * hi_dim1] * hr[j + en * hr_dim1];
/* L740: */
	    }

	    yr = xr - wr[i__];
	    yi = xi - wi[i__];
	    if (yr != 0.f || yi != 0.f) {
		goto L765;
	    }
	    tst1 = norm;
	    yr = tst1;
L760:
	    yr *= .01f;
	    tst2 = norm + yr;
	    if (tst2 > tst1) {
		goto L760;
	    }
L765:
	    cdiv_(&zzr, &zzi, &yr, &yi, &hr[i__ + en * hr_dim1], &hi[i__ + en 
		    * hi_dim1]);
/*     .......... overflow control .......... */
	    tr = (r__1 = hr[i__ + en * hr_dim1], dabs(r__1)) + (r__2 = hi[i__ 
		    + en * hi_dim1], dabs(r__2));
	    if (tr == 0.f) {
		goto L780;
	    }
	    tst1 = tr;
	    tst2 = tst1 + 1.f / tst1;
	    if (tst2 > tst1) {
		goto L780;
	    }
	    i__3 = en;
	    for (j = i__; j <= i__3; ++j) {
		hr[j + en * hr_dim1] /= tr;
		hi[j + en * hi_dim1] /= tr;
/* L770: */
	    }

L780:
	    ;
	}

/* L800: */
    }
/*     .......... end backsubstitution .......... */
/*     .......... vectors of isolated roots .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ >= *low && i__ <= *igh) {
	    goto L840;
	}

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
	    zr[i__ + j * zr_dim1] = hr[i__ + j * hr_dim1];
	    zi[i__ + j * zi_dim1] = hi[i__ + j * hi_dim1];
/* L820: */
	}

L840:
	;
    }
/*     .......... multiply by transformation matrix to give */
/*                vectors of original full matrix. */
/*                for j=n step -1 until low do -- .......... */
    i__1 = *n;
    for (jj = *low; jj <= i__1; ++jj) {
	j = *n + *low - jj;
	m = min(j,*igh);

	i__2 = *igh;
	for (i__ = *low; i__ <= i__2; ++i__) {
	    zzr = 0.f;
	    zzi = 0.f;

	    i__3 = m;
	    for (k = *low; k <= i__3; ++k) {
		zzr = zzr + zr[i__ + k * zr_dim1] * hr[k + j * hr_dim1] - zi[
			i__ + k * zi_dim1] * hi[k + j * hi_dim1];
		zzi = zzi + zr[i__ + k * zr_dim1] * hi[k + j * hi_dim1] + zi[
			i__ + k * zi_dim1] * hr[k + j * hr_dim1];
/* L860: */
	    }

	    zr[i__ + j * zr_dim1] = zzr;
	    zi[i__ + j * zi_dim1] = zzi;
/* L880: */
	}
    }

    goto L1001;
/*     .......... set error -- all eigenvalues have not */
/*                converged after 30*n iterations .......... */
L1000:
    *ierr = en;
L1001:
    return 0;
} /* comlr2_ */

