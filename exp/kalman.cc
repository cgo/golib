#include "kalman.h"
#include <gorandom.h>

goKalman::goKalman(int dimX, int dimY)
{
	x 		= new kMatrix(dimX,1);
	y 		= new kMatrix(dimY,1);
	x_pri  	= new kMatrix(dimX,1);
	x_post 	= new kMatrix(dimX,1);
	A 		= new kMatrix(dimX,dimX);
	H 		= new kMatrix(dimY,dimX);
	P 		= new kMatrix(dimX,dimX);
	P_pri 	= new kMatrix(dimX,dimX);
	Q		= new kMatrix(dimX,dimX);
	R 		= new kMatrix(dimY,dimY);
	K		= new kMatrix(dimX,dimY);
}

goKalman::~goKalman()
{
	delete x;
	delete y;
	delete x_pri;
	delete x_post;
	delete A;
	delete H;
	delete P;
	delete R;
	delete K;
}

void
goKalman::init()
{
	
}

void
goKalman::timeUpdate()
{
	kMatrix A_T(*A);
	A_T.transpose();
	
	(*x_pri) = (*A) * (*x_post);    		// Neue a priori Schätzwerte des
											// Zustandsvektors x
	(*P_pri) = (*A) * (*P) * (A_T) + (*Q);  // Neue a priori Fehlerkovarianz

	//cout << "x_pri after time update:\n";
	//x_pri->print();
}

void
goKalman::measure(kMatrix *_y)
{
	*y = *_y;
	//cout << "Updated measurement with\n";
	//y->print();
}

void
goKalman::measurementUpdate()
{
	kMatrix H_T(*H);
	H_T.transpose();
	kMatrix C(R->getSizeY(),R->getSizeX());
	C = (*H) * (*P_pri) * H_T + (*R);
	goMath::matrixInversion(C);
	
	kMatrix in(y->getSizeY(),y->getSizeX());	// innovation
	in 		  = (*y) - ((*H) * (*x_pri));
	(*K) 	  = (*P_pri) * H_T * C;    // Kalman Gain
	(*x_post) = (*x_pri) + ( (*K) * in );

	// Q update -- experimental
	goSize_t j;
	kMatrix er(Q->getSizeX(),1);
	kMatrix er_T(Q->getSizeX(),1);
	kFloat maxIn = in[0][0];
	for (j = 0; j < in.getSizeY(); j++)
	{
		if (maxIn > in[j][0])
			maxIn = in[j][0]; 
	}
	kFloat   bereich = maxIn;
	for (j = 0; j < er.getSizeY(); j++)
	{
		er[j][0] = (goRandom() - 0.5) * bereich;
	}
	er_T = er;
	er_T.transpose();
	// (*Q) = er * er_T;

	// Debugging innovations sequence
	/*
	cout << "y: ";
	for (int i = 0; i < y->getSizeY(); i++)
		cout << (*y)[i][0] << ",";
	cout << "\n";	
	cout << "x_pri: ";
	for (int i = 0; i < x_pri->getSizeY(); i++)
		cout << (*x_pri)[i][0] << ",";
	cout << "\n";	
	cout << "Innovation: ";
	for (int i = 0; i < in.getSizeY(); i++)
		cout << in[i][0] << ",";
	cout << "\n";	
	*/
	
	kMatrix I(P->getSizeY(),P->getSizeX());
	I.unity();
	(*P)	  = (I - ((*K) * (*H))) * (*P_pri);

}



goKalman3DMotion::goKalman3DMotion()
	: goKalman(6,6)
{
	
}

goKalman3DMotion::~goKalman3DMotion()
{
}

void
goKalman3DMotion::init()
{
	// Messoperator = I
	H->unity();
	x->fill(0);
	x_pri->fill(0);
	// x_post muss mit Schätzwerden initialisiert werden
		
	y->fill(0);
	// Initialize timestep
	setTimeStep(1); 

	// Prozessfehlerkovarianz "irgendwie" abschätzen ....
	
}

void
goKalman3DMotion::setInitialQ()
{
	goRandom(true);
	kMatrix er(Q->getSizeX(),1);
	kMatrix er_T(Q->getSizeX(),1);
	goSize_t j;
	kFloat   bereich = 0.022;
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
goKalman3DMotion::setInitialR()
{
	goRandom(true);
	kMatrix er(R->getSizeX(),1);
	kMatrix er_T(R->getSizeX(),1);
	goSize_t j;
	kFloat   bereich = 0.1;
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
goKalman3DMotion::setInitialState(kFloat sx, kFloat sy, kFloat sz,
								  kFloat vx, kFloat vy, kFloat vz)
{
	(*x_post)[0][0] = sx;
	(*x_post)[1][0] = sy;
	(*x_post)[2][0] = sz;
	(*x_post)[3][0] = vx;
	(*x_post)[4][0] = vy;
	(*x_post)[5][0] = vz;
	cout << "Initial state vector set to\n";
	x_post->print();
}

void
goKalman3DMotion::setInitialError(kFloat dx, kFloat dy, kFloat dz,
								  kFloat dvx, kFloat dvy, kFloat dvz)
{
	kMatrix er(6,1);
	kMatrix er_T(6,1);
	er[0][0] = dx;
	er[1][0] = dy;
	er[2][0] = dz;
	er[3][0] = dvx;
	er[4][0] = dvy;
	er[5][0] = dvz;
	er_T = er;
	er_T.transpose();
	(*P) = er * er_T;
	cout << "Initial error covariance set to\n";
	P->print();
}

void
goKalman3DMotion::setTimeStep(kFloat t)
{
	timeStep = t;
	A->unity();
	(*A)[0][3] = t;
	(*A)[1][4] = t;
	(*A)[2][5] = t;
	cout << "Timestep set system matrix A to\n";
	A->print();
}

void goKalman3DMotion::errorUpdate()
{
	// Q neu berechnen -- kein weisses Rauschen, daher eher falsch :(
	kMatrix er(6,1);
	kMatrix er_T(6,1);
	er = *y;
	er -= *x_post;
	er_T = er;
	er_T.transpose();
	*Q = er * er_T;
	cout << "Q updated to\n";
	Q->print();
}
