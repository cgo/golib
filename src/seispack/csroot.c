/* csroot.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int csroot_(real *xr, real *xi, real *yr, real *yi)
{
    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static real s, ti, tr;
    extern doublereal pythag_(real *, real *);


/*     (yr,yi) = complex sqrt(xr,xi) */
/*     branch chosen so that yr .ge. 0.0 and sign(yi) .eq. sign(xi) */

    tr = *xr;
    ti = *xi;
    s = sqrt((pythag_(&tr, &ti) + dabs(tr)) * .5f);
    if (tr >= 0.f) {
	*yr = s;
    }
    if (ti < 0.f) {
	s = -s;
    }
    if (tr <= 0.f) {
	*yi = s;
    }
    if (tr < 0.f) {
	*yr = ti / *yi * .5f;
    }
    if (tr > 0.f) {
	*yi = ti / *yr * .5f;
    }
    return 0;
} /* csroot_ */

