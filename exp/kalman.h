#ifndef GO_KALMAN_H
#define GO_KALMAN_H

#include <golinearalgebra.h>
#include <gotypes.h>
#include <gomatrix.h>
#include <gonvector.h>

class goKalman
{
	public:
	typedef goDouble kFloat; 		// define the accuracy. goFloat leads to
									// very early inaccuracies.
	typedef goMatrix<kFloat>  kMatrix;  // define a matrix
		/*!
		 * @param dimX Dimension of the x state vector
		 * @param dimY Dimension of the y measurement vector
		 */
		goKalman(int dimX, int dimY);
		virtual ~goKalman();
		// Must be overwritten by a subclass to initialize the
		// system according to the needs of the model.
		virtual void init();	
		virtual void timeUpdate();
		// Measurement. _y Must have the same dimensions as the matrix y.
		virtual void measure(kMatrix *_y);
		virtual void measurementUpdate();
		
	    // Häufig verwendete Notation in der Literatur	
		kMatrix *x;		// Zustandsvektor	
		kMatrix *y;		// Messung
		kMatrix *x_pri;	// A priori Schätzung
		kMatrix *x_post;	// A posteriori Schätzung
		kMatrix  *A;		// Zustandsübergangsmatrix
		kMatrix  *H;		// Messoperator
		kMatrix  *P;		// Vorhersagefehlerkovarianz
		kMatrix	 *P_pri;	// A priori Vorhersagefehlerkovarianz
		kMatrix  *Q;		// Prozessfehlerkovarianz (Prozessrauschen)
		kMatrix  *R;		// Messfehlerkovarianz
		kMatrix  *K;		// Kalman Gain
	protected:
		void calcErrors();
	private:	
	
};

class goKalmanJoy : public goKalman
{
	public:
		goKalmanJoy();
		virtual ~goKalmanJoy();

	void init();
	void setInitialError(kFloat dx1, kFloat dx2, kFloat dx3, kFloat dx4);

	void setInitialR();
	void setInitialQ();
			
	protected:
		
	private:	
};

class goKalman3DMotion : public goKalman
{
	public:
		goKalman3DMotion();
		virtual ~goKalman3DMotion();
		virtual void init();
		/*
		 * Sets initial state variables.
		 * @param s[xyz] Position
		 * @param v[xyz] Velocity
		 */
		void setInitialState(kFloat sx, kFloat sy, kFloat sz,
							 kFloat vx, kFloat vy, kFloat vz);
		void setInitialError(kFloat dx, kFloat dy, kFloat dz,
				   		     kFloat dvx, kFloat dvy, kFloat dvz);
		void setInitialR();
		void setInitialQ();
		void errorUpdate();
		void setTimeStep(kFloat t);
	private:
		kFloat timeStep;	
};

#endif
