/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef golib_clapack_h
#define golib_clapack_h

#include <goconfig.h>

#ifdef OSX
#define LAPACK_COMPLEX_STRUCTURE
#include "lapacke_config.h"
 #include "lapacke.h"
// Search for that file and include it from here on Apple platforms.
#include "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.12.sdk/System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/Headers/cblas.h"
  // #include <Accelerate/Accelerate.h>
#else
extern "C"
{
 #include <cblas.h>
 #include <lapacke.h>
}

//=
//= These are from the clapack.h header file from netlib's clapack (not ATLAS),
//= which you can find at http://www.netlib.org/clapack/clapack.h
extern "C" {
/* Subroutine */ int sgesvd_(char *jobu, char *jobvt, integer *m, integer *n,
	real *a, integer *lda, real *s, real *u, integer *ldu, real *vt,
	integer *ldvt, real *work, integer *lwork, integer *info);
/* Subroutine */ int dgesvd_(char *jobu, char *jobvt, integer *m, integer *n,
	doublereal *a, integer *lda, doublereal *s, doublereal *u, integer *
	ldu, doublereal *vt, integer *ldvt, doublereal *work, integer *lwork,
	integer *info);
/* Subroutine */ int sgels_(char *trans, integer *m, integer *n, integer *
	nrhs, real *a, integer *lda, real *b, integer *ldb, real *work,
	integer *lwork, integer *info);
/* Subroutine */ int dgels_(char *trans, integer *m, integer *n, integer *
	nrhs, doublereal *a, integer *lda, doublereal *b, integer *ldb,
	doublereal *work, integer *lwork, integer *info);

/* Subroutine */ int sgelss_(integer *m, integer *n, integer *nrhs, real *a,
	integer *lda, real *b, integer *ldb, real *s, real *rcond, integer *
	rank, real *work, integer *lwork, integer *info);

/* Subroutine */ int dgelss_(integer *m, integer *n, integer *nrhs,
	doublereal *a, integer *lda, doublereal *b, integer *ldb, doublereal *
	s, doublereal *rcond, integer *rank, doublereal *work, integer *lwork,
	 integer *info);

/* Subroutine */ int sposv_(char *uplo, integer *n, integer *nrhs, real *a,
	integer *lda, real *b, integer *ldb, integer *info);
/* Subroutine */ int dposv_(char *uplo, integer *n, integer *nrhs, doublereal *a,
	integer *lda, doublereal *b, integer *ldb, integer *info);
}
#endif

#endif
