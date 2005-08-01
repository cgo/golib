/* pythag.f -- translated by f2c (version 20050501).
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

doublereal pythag_(real *a, real *b)
{
    /* System generated locals */
    real ret_val, r__1, r__2, r__3;

    /* Local variables */
    static real p, r__, s, t, u;


/*     finds sqrt(a**2+b**2) without overflow or destructive underflow */

/* Computing MAX */
    r__1 = dabs(*a), r__2 = dabs(*b);
    p = dmax(r__1,r__2);
    if (p == 0.f) {
	goto L20;
    }
/* Computing MIN */
    r__2 = dabs(*a), r__3 = dabs(*b);
/* Computing 2nd power */
    r__1 = dmin(r__2,r__3) / p;
    r__ = r__1 * r__1;
L10:
    t = r__ + 4.f;
    if (t == 4.f) {
	goto L20;
    }
    s = r__ / t;
    u = s * 2.f + 1.f;
    p = u * p;
/* Computing 2nd power */
    r__1 = s / u;
    r__ = r__1 * r__1 * r__;
    goto L10;
L20:
    ret_val = p;
    return ret_val;
} /* pythag_ */

