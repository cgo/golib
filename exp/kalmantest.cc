/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include "kalman.h"
#include <gotypes.h>
#include <gomatrix.h>
#include <gorandom.h>
#include <SDL/SDL.h>
#include <SDL_gfxPrimitives.h>
#include <time.h>

namespace goVideo {

    SDL_Surface *surface;
    int width;
    int height;
	int bytesPP;
    
    void initVideo (int w, int h, int bpp, unsigned int additional_modes)
    {
	if( (SDL_Init(SDL_INIT_VIDEO | additional_modes) == -1) ) 
	    { 
		printf("Could not initialize SDL: %s.\n", SDL_GetError());
		exit(-1);
	    }
	surface = SDL_SetVideoMode (w, h, bpp, SDL_HWSURFACE|SDL_DOUBLEBUF);
	if (surface == NULL)
	    {
		cout << "SDL fucked up setting the video mode." << endl;
		exit(-1);
	    }
	width = w;
	height = h;
	bytesPP = surface->format->BytesPerPixel;
    }
    
    void drawLine (int x1, int y1, int x2, int y2, unsigned int color)
    {
		SDL_LockSurface (surface);
	 	lineColor(surface,x1,y1,x2,y2,color);
		SDL_UnlockSurface (surface);
		SDL_Flip(surface);
    }
    
    void finishVideo ()
    {
		SDL_Quit();
    }
};
int main()
{
	// init sdl
	goVideo::initVideo(256,256,32,SDL_INIT_JOYSTICK);
	cout << SDL_NumJoysticks() << " SDL joysticks detected.\n";
	int i;
	for (i = 0; i < SDL_NumJoysticks(); i++)
		cout << "Joystick " << i << " is " << SDL_JoystickName(i) << "\n";
	SDL_Joystick *joy;
	joy = SDL_JoystickOpen(0);
	if (!joy)
	{
		cout << "Error opening joystick\n";
		return(2);
	}
	cout << "Number of axes: " << SDL_JoystickNumAxes(joy) << "\n";
	cout << "Number of buttons: " << SDL_JoystickNumButtons(joy) << "\n";
	SDL_Event event;
	SDL_JoystickEventState(SDL_IGNORE);
	
	
	goKalmanJoy k;
    //k.setTimeStep(0.1);
	(*k.x_post)[0][0] = 0;
	(*k.x_post)[1][0] = 0;
	(*k.x_post)[2][0] = 0;
	(*k.x_post)[3][0] = 0;
	
	k.setInitialError(0.20,0.20,0.20,0.20);
					  
	k.setInitialR();
	k.setInitialQ();
	k.init();	

	goKalman::kMatrix y(2,1);
	
	// k.timeUpdate();
	y[0][0] = 0;
	y[1][0] = 0;
	float x1 = 0;
	float y1 = 0;
	float x2 = 0;
	float y2 = 0;
	float x_real1 = 0;
	float y_real1 = 0;
	float x_real2 = 0;
	float y_real2 = 0;
	goRandom(true);
	int pollCounter = 0;
	int nanos = 100000000;
	struct timespec nanostruct;
	struct timespec ns2;
	nanostruct.tv_sec = 0;
	nanostruct.tv_nsec = nanos;
	while (true)	
	{
#if 0
		nanosleep(&nanostruct,&ns2);
	 	if(SDL_PollEvent(&event) == 1)
		{
			switch(event.type)
			{
				case SDL_JOYAXISMOTION:
					//cout << "Axis " << (int)event.jaxis.axis << ": " << event.jaxis.value << endl;
					if (event.jaxis.axis == 0)
					{
						y[0][0] = -event.jaxis.value / (float)16000.0f;
						y[0][0] *= 1 + (0.01 * (goRandom() - 0.5));
					}
					if (event.jaxis.axis == 1)
					{
						y[1][0] = -event.jaxis.value / (float)16000.0f;
						y[1][0] *= 1 + (0.01 * (goRandom() - 0.5));
					}
					break;
				case SDL_JOYBUTTONDOWN:
					if (event.jbutton.state == SDL_PRESSED)
					{
						cout << "Button " << (int)event.jbutton.button << " pressed" << endl; 
						return(1);
					}
					break;
				case SDL_JOYBUTTONUP:
					if (event.jbutton.state == SDL_RELEASED)
						cout << "Button " << (int)event.jbutton.button << " released" << endl; 
					break;
				case SDL_KEYDOWN:
				// if (event.key.keysym.sym == SDLK_q)
					return (1);
					break;		
			}
		}
#endif
		SDL_JoystickUpdate();
		//if (pollCounter == 0)
		{
			y[0][0] = -SDL_JoystickGetAxis(joy,0) / (float)16000.0f;
			y[1][0] = -SDL_JoystickGetAxis(joy,1) / (float)16000.0f;
		
			pollCounter = 100;
			if (SDL_JoystickGetButton(joy,0) == 1)
				return(1);
		}
		pollCounter--;
		k.timeUpdate();
		// k.measurementUpdate();
		x1 = x2; y1 = y2;
		x2 = (*k.x_pri)[0][0];
		y2 = (*k.x_pri)[1][0];
		x_real1 = x_real2;
		y_real1 = y_real2;
		x_real2 += y[0][0] * 1;
		y_real2 += y[1][0] * 1;
		//cout << x1 << "," << y1 << " -> " << x2 << "," << y2 << "\n";
		//cout << x_real1 << "," << y_real1 << " -> " << x_real2 << "," << y_real2 << "\n";
		
		goVideo::drawLine((goVideo::width >> 1) - (int)x1,
						  (goVideo::height >> 1) - (int)y1, 	
						  (goVideo::width >> 1) - (int)x2,
						  (goVideo::height >> 1) - (int)y2,
						  0x0000ff80);
		goVideo::drawLine((goVideo::width >> 1) - (int)x_real1,
						  (goVideo::height >> 1) - (int)y_real1, 	
						  (goVideo::width >> 1) - (int)x_real2,
						  (goVideo::height >> 1) - (int)y_real2,
						  0x00ff0080);
		cout << "Measured : " << y[0][0] << "," << y[1][0] << "\n";
		cout << "Predicted: " << (*k.x_pri)[2][0] << "," << (*k.x_pri)[3][0] << "\n";
		k.measure(&y);
		k.measurementUpdate();
	}
	goVideo::finishVideo();

	char c;
	do {
		k.measure(&y);
		k.measurementUpdate();
		// k.errorUpdate();
		k.timeUpdate();
		// y[3][0] += 1; 
		// y[4][0] += 1;
		// y[5][0] += 1;
		// y[0][0] += 1;
		// y[1][0] += 1;
		cin >> c;
	} while (c != 'q');
	return 1;
}
