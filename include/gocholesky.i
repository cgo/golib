#ifndef GOCHOLESKY_I
#define GOCHOLESKY_I

namespace goMath
{

template <class Real>
goCholesky<Real>::goCholesky() : L_(0,0), isspd(0) {}

/**
	@return 1, if original matrix to be factored was symmetric 
		positive-definite (SPD).
*/
template <class Real>
int goCholesky<Real>::is_spd() const
{
	return isspd;
}

/**
	@return the lower triangular factor, L, such that L*L'=A.
*/
template <class Real>
goMatrix<Real> goCholesky<Real>::getL() const
{
	return L_;
}

/**
	Constructs a lower triangular matrix L, such that L*L'= A.
	If A is not symmetric positive-definite (SPD), only a
	partial factorization is performed.  If is_spd()
	evalutate true (1) then the factorizaiton was successful.
*/
template <class Real>
goCholesky<Real>::goCholesky(const goMatrix<Real> &A)
{
   	int m = A.dim1();
	int n = A.dim2();
	
	isspd = (m == n);

	if (m != n)
	{
		L_ = goMatrix<Real>(0,0);
		return;
	}

	L_ = goMatrix<Real>(n,n);


      // Main loop.
     for (int j = 0; j < n; j++) 
	 {
        double d = 0.0;
        for (int k = 0; k < j; k++) 
		{
            Real s = 0.0;
            for (int i = 0; i < k; i++) 
			{
               s += L_[k][i]*L_[j][i];
            }
            L_[j][k] = s = (A[j][k] - s)/L_[k][k];
            d = d + s*s;
            isspd = isspd && (A[k][j] == A[j][k]); 
         }
         d = A[j][j] - d;
         isspd = isspd && (d > 0.0);
         L_[j][j] = sqrt(d > 0.0 ? d : 0.0);
         for (int k = j+1; k < n; k++) 
		 {
            L_[j][k] = 0.0;
         }
	}
}

/**

	Solve a linear system A*x = b, using the previously computed
	cholesky factorization of A: L*L'.

   @param  B   A Matrix with as many rows as A and any number of columns.
   @return     x so that L*L'*x = b.  If b is nonconformat, or if A
   				was not symmetric posidtive definite, a null (0x0)
   						array is returned.
*/
template <class Real>
goArray<Real> goCholesky<Real>::solve(const goArray<Real> &b)
{
	int n = L_.dim1();
	if (b.dim1() != n)
		return goArray<Real>();


	goArray<Real> x = b.copy();


      // Solve L*y = b;
      for (int k = 0; k < n; k++) 
	  {
         for (int i = 0; i < k; i++) 
               x[k] -= x[i]*L_[k][i];
		 x[k] /= L_[k][k];
		
      }

      // Solve L'*X = Y;
      for (int k = n-1; k >= 0; k--) 
	  {
         for (int i = k+1; i < n; i++) 
               x[k] -= x[i]*L_[i][k];
         x[k] /= L_[k][k];
      }

	return x;
}


/**

	Solve a linear system A*X = B, using the previously computed
	cholesky factorization of A: L*L'.

   @param  B   A Matrix with as many rows as A and any number of columns.
   @return     X so that L*L'*X = B.  If B is nonconformat, or if A
   				was not symmetric posidtive definite, a null (0x0)
   						array is returned.
*/
template <class Real>
goMatrix<Real> goCholesky<Real>::solve(const goMatrix<Real> &B)
{
	int n = L_.dim1();
	if (B.dim1() != n)
		return goMatrix<Real>();


	goMatrix<Real> X = B.copy();
	int nx = B.dim2();

      // Solve L*y = b;
  	  for (int j=0; j< nx; j++)
	  {
      	for (int k = 0; k < n; k++) 
		{
			for (int i = 0; i < k; i++) 
               X[k][j] -= X[i][j]*L_[k][i];
		    X[k][j] /= L_[k][k];
		 }
      }

      // Solve L'*X = Y;
     for (int j=0; j<nx; j++)
	 {
      	for (int k = n-1; k >= 0; k--) 
	  	{
         	for (int i = k+1; i < n; i++) 
               X[k][j] -= X[i][j]*L_[i][k];
         	X[k][j] /= L_[k][k];
		}
      }



	return X;
}

}

#endif
