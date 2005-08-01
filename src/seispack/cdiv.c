/* cdiv.f -- translated by f2c (version 20050501).
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

/* Subroutine */ int cdiv_(real *ar, real *ai, real *br, real *bi, real *cr, 
	real *ci)
{
    /* System generated locals */
    real r__1, r__2;

    /* Local variables */
    static real s, ais, bis, ars, brs;


/*     complex division, (cr,ci) = (ar,ai)/(br,bi) */

    s = dabs(*br) + dabs(*bi);
    ars = *ar / s;
    ais = *ai / s;
    brs = *br / s;
    bis = *bi / s;
/* Computing 2nd power */
    r__1 = brs;
/* Computing 2nd power */
    r__2 = bis;
    s = r__1 * r__1 + r__2 * r__2;
    *cr = (ars * brs + ais * bis) / s;
    *ci = (ais * brs - ars * bis) / s;
    return 0;
} /* cdiv_ */

