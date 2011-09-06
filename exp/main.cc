/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <golinearalgebra.h> 
#include <gomatrix.h>
#include <gotypes.h>

using namespace goMath;

int main()
{
	goMatrix<goDouble> 	m(3,3);
	goNVector<int>		indx(3);
	double d;
	int i,j;
	
	for (i = 0; i < m.getSizeY(); i++)
	{
		for (j = 0; j < m.getSizeX(); j++)
		{
			m[i][j] = j * i;
		}
		cout << "\n";
	}
//	m[0][0] = 1; m[0][1] = 2; m[0][2] = 1;
//	m[1][0] = 2; m[1][1] = 1; m[1][2] = 2;
//	m[2][0] = 0; m[2][1] = 3; m[2][2] = 1;

	m[0][0] = 1; m[0][1] = 0; m[0][2] = 0;
	m[1][0] = 0; m[1][1] = 1; m[1][2] = 0;
	m[2][0] = 0; m[2][1] = 0; m[2][2] = 1;
	LUDecomp<double>(m,indx,&d);
	goNVector<double> b(3);
	b[0] = 2; b[1] = 3; b[2] = 1;
	LUBackSubst<double>(m,indx,b);
	for (i = 0; i < b.getSize(); i++)
	{
		cout << b[i] << " ";
	}
	cout << endl;
	
	for (i = 0; i < m.getSizeY(); i++)
	{
		for (j = 0; j < m.getSizeX(); j++)
		{
			cout << m[i][j] << " ";
		}
		cout << "\n";
	}
	cout << endl;

#if 1
	m[0][0] = 1; m[0][1] = 2; m[0][2] = 0;
	m[1][0] = 2; m[1][1] = 0; m[1][2] = 2;
	m[2][0] = 0; m[2][1] = 3; m[2][2] = 1;
#endif
#if 0
	m.unity();
#endif
	cout << "Inversion:" << endl;
	goMatrix<double> m2(m.getSizeX(),m.getSizeY());
	goMatrix<double> m3(m.getSizeX(),m.getSizeY());
	m2 = m;
	matrixInversion<double>(m);
	
	for (i = 0; i < m.getSizeY(); i++)
	{
		for (j = 0; j < m.getSizeX(); j++)
		{
			cout << m[i][j] << " ";
		}
		cout << "\n";
	}
	cout << endl;
	for (i = 0; i < m.getSizeY(); i++)
	{
		for (j = 0; j < m.getSizeX(); j++)
		{
			cout << m2[i][j] << " ";
		}
		cout << "\n";
	}
	cout << endl;
	
	cout << "Test:" << endl;	
	m3 = m * m2;
	for (i = 0; i < m.getSizeY(); i++)
	{
		for (j = 0; j < m.getSizeX(); j++)
		{
			cout << m3[i][j] << " ";
		}
		cout << "\n";
	}
	cout << endl;
	
	
}

