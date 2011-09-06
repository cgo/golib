/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include "kalman.h"
#include <gotypes.h>
#include <gorandom.h>

goKalmanJoy::goKalmanJoy()
	: goKalman(4,2)
{
}

goKalmanJoy::~goKalmanJoy()
{
}

void
goKalmanJoy::init()
{
	H->fill(0);
	(*H)[0][2] = 1;
	(*H)[1][3] = 1;
	A->unity();
	(*A)[0][2] = 1;    // timestep
	(*A)[1][3] = 1;    // timestep
	setInitialR();
	setInitialQ();
}

void
goKalmanJoy::setInitialQ()
{
	goRandom(true);
	kMatrix er(Q->getSizeX(),1);
	kMatrix er_T(Q->getSizeX(),1);
	goSize_t j;
	kFloat   bereich = 2;
	for (j = 0; j < er.getSizeY(); j++)
	{
		er[j][0] = (goRandom() - 0.5) * bereich;
	}
	er_T = er;
	er_T.transpose();
	(*Q) = er * er_T;
	cout << "Q initialized with\n";
	Q->print();
}

void
goKalmanJoy::setInitialR()
{
	goRandom(true);
	kMatrix er(R->getSizeX(),1);
	kMatrix er_T(R->getSizeX(),1);
	goSize_t j;
	kFloat   bereich = 2;
	for (j = 0; j < er.getSizeY(); j++)
	{
		er[j][0] = (goRandom() - 0.5) * bereich;
	}
	er_T = er;
	er_T.transpose();
	(*R) = er * er_T;
	cout << "R initialized with\n";
	R->print();
}

void
goKalmanJoy::setInitialError(kFloat dx1, kFloat dx2, kFloat dx3, kFloat dx4)
{
	kMatrix er(4,1);
	kMatrix er_T(4,1);
	er[0][0] = dx1;
	er[1][0] = dx2;
	er[2][0] = dx3;
	er[3][0] = dx4;
	er_T = er;
	er_T.transpose();
	(*P) = er * er_T;
	cout << "Initial error covariance set to\n";
	P->print();
}
