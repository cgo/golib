
/*
 * 14.11.2001
 * This file contains code from "Numerical Recipes in C".
 * It is not intended for open public and not for sale or anything similar.
 * 	--Christian
 */

#include <golinearalgebra.h>
#include <gonvector.h>
#include <gomatrix.h>
#include <gomatrix.i>
#include <gotypes.h>

#define TINY 1.0e-20;
namespace goMath {
template<class T>
void
LUDecomp(goMatrix<T>& a, goNVector<int>& indx, double *d)
{
	if (a.getSizeX() != a.getSizeY())
	{
        std::cout << "Matrix is not quadratic\n";
		return;
	}
	int i,imax,j,k;
	double big,dum,sum,temp;
	
	
	int n = a.getSizeX();
	goNVector<double> vv(n);

	*d=1.0;
	for (i=0;i<n;i++) {
		big=0.0;
		for (j=0;j<n;j++)
			if ((temp=fabs((double)a.elem(i,j))) > big) big=temp;
		if (big == 0.0) 
		{ 
            std::cout << "Singular matrix\n";
			return;
		}
		vv[i]=1.0/big;
	}
	for (j=0;j<n;j++) {
		for (i=0;i<j;i++) {
			sum=a.elem(i,j);
			for (k=0;k<i;k++) sum -= (double)(a.elem(i,k)*a.elem(k,j));
			a.elem(i,j)=sum;
		}
		big=0.0;
		for (i=j;i<n;i++) {
			sum=(double)a.elem(i,j);
			for (k=0;k<j;k++)
				sum -= (double)(a.elem(i,k)*a.elem(k,j));
			a.elem(i,j)=(T)sum;
			if ( (dum=vv[i]*fabs(sum)) >= big) {
				big=dum;
				imax=i;
			}
		}
		if (j != imax) {
			for (k=0;k<n;k++) {
				dum=a.elem(imax,k);
				a.elem(imax,k)=a.elem(j,k);
				a.elem(j,k)=dum;
			}
			*d = -(*d);
			vv[imax]=vv[j];
		}
		indx[j]=imax;
		if (a.elem(j,j) == 0.0) a.elem(j,j)=TINY;
		if (j != n) {
			dum=1.0/(a.elem(j,j));
			for (i=j+1;i<n;i++) a.elem(i,j) *= dum;
		}
	}
}

template<class T>
void LUBackSubst(goMatrix<T>& a, goNVector<int>& indx, goNVector<T>& b)
{
	int i,ii=-1,ip,j;
	double sum;
	int n = a.getSizeX();

	for (i=0;i<n;i++) {
		ip=indx[i];
		sum=(double)b[ip];
		b[ip]=b[i];
		if (ii >= 0)
			for (j=ii;j<=i-1;j++) sum -= (double)(a.elem(i,j)*b[j]);
		else if (sum) ii=i;
		b[i]=(T)sum;
	}
	for (i=n-1;i>-1;i--) {
		sum=b[i];
		for (j=i+1;j<n;j++) sum -= (double)(a.elem(i,j)*b[j]);
		b[i]=(T)(sum/a.elem(i,i));
	}
}

template<class T>
void matrixInversion(goMatrix<T>& a)
{
	if (a.getSizeX() != a.getSizeY())
	{
        std::cout << "Matrix inversion: Matrix must be quadratic." << std::endl;
		return;
	}
	int n = a.getSizeX();
	goMatrix<T> y(n,n);
	double d;
	goNVector<int> index(n);
	goNVector<T>   column(n);
	LUDecomp<T>(a,index,&d);
	int j,i;
	for (j = 0; j < n; j++)
	{
		for (i = 0; i < n; i++)
			column[i] = 0;
		column[j] = 1;
		LUBackSubst<T>(a,index,column);
		for (i = 0; i < n; i++)
			y.elem(i,j) = column[i];	
	}
	a = y;
}
template void LUDecomp(goMatrix<double>&,goNVector<int>&,double*);
template void LUBackSubst(goMatrix<double>&,goNVector<int>&,goNVector<double>&);
template void matrixInversion(goMatrix<double>&);
template void LUDecomp(goMatrix<float>&,goNVector<int>&,double*);
template void LUBackSubst(goMatrix<float>&,goNVector<int>&,goNVector<float>&);
template void matrixInversion(goMatrix<float>&);
};

#undef TINY

