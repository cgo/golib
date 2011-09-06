/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gojoystick.h>
#include <SDL/SDL.h>
#include <gothread.h>
#include <golog.h>

goJoystick::goJoystick(int joyIndex)
	: goNavDevice()
{
	stick = SDL_JoystickOpen(joyIndex);
	if (!stick)
	{
		goLog::warning("goJoystick::goJoystick(): Could not open joystick");
	}
	else
	{
		// remove joystick events from SDL event queue, so we can call update() at will.
		SDL_JoystickEventState(SDL_ENABLE);
		axisScale.resize(SDL_JoystickNumAxes(stick));
		int i;
		for (i = 0; i < axisScale.getSize(); i++)
		{
			axisScale[i] = 1 / (float)32767;
		}
	}
}

goJoystick::~goJoystick()
{
	if (stick)
	{
		SDL_JoystickClose(stick);
	}
}

static 
void*
gojoystick_thread(void* p)
{
	goJoystick* js;
	js = (goJoystick*)p;
	js->runThread();
	return 0;
}

void 
goJoystick::run()
{
	if (stick)
		thread.create(gojoystick_thread,(void*)this,1);
	else
		goLog::warning("goJoystick::run(): Joystick initialization error. No thread started.");	
}

void
goJoystick::runThread()
{
	while(true)
	{
		SDL_Event event;
		if (SDL_WaitEvent(&event) == 0)
		{
			goLog::warning("goJoystick::runThread(): There was an error waiting for an SDL event.. returning");
			return;
		}
		// cout << "joystickthread returned from waitevent\n";
		// int numEvents;
		// threadsafe laut SDL Dokumentation
		// Problem: Andere events werden nicht von der queue genommen. So kann dieser Thread dauernd
		// laufen, da WaitEvent immer gleich zurueckkommt, wenn die anderen events nicht von einem
		// anderen thread aus der queue entfernt werden.
		// numEvents = SDL_PeepEvents(&event,1,SDL_GETEVENT,SDL_JOYAXISMOTION | SDL_JOYBUTTONDOWN | SDL_JOYBUTTONUP);
		if ( (event.type == SDL_JOYAXISMOTION) ||
			 (event.type == SDL_JOYBUTTONDOWN) ||
			 (event.type == SDL_JOYBUTTONUP) )
		{
			SDL_Event e;
			while (SDL_PeepEvents(&e,1,SDL_GETEVENT,SDL_JOYAXISMOTION |
				SDL_JOYBUTTONDOWN | SDL_JOYBUTTONUP) > 0);
			navDeviceUpdate();
		}
	}
}

void
goJoystick::navDeviceUpdate()
{
	// SDL_JoystickUpdate();
	double a0,a1;
	a0 = SDL_JoystickGetAxis(stick,0) * axisScale[0];
	a1 = SDL_JoystickGetAxis(stick,1) * axisScale[1];
	int b0,b1,b2;
	b0 = SDL_JoystickGetButton(stick,0);
	b1 = SDL_JoystickGetButton(stick,1);
	b2 = SDL_JoystickGetButton(stick,2);
	go3Vector<volFloat> vr;   // Geschwindigkeit [-1,+1] Rotation
	go3Vector<volFloat> vt;   // Geschwindigkeit [-1,+1] Translation
	switch(b0)
	{	// released
		case 0:
			// Interpretiere a0/a1 als Maß für die Geschwindigkeit der Bewegung.
			vt.x = (volFloat)a0;  // Translation in X Richtung
			vt.y = 0.0f;
			vt.z = -(volFloat)a1;  // Translation in Z Richtung
			vr.x = 0.0f;
			vr.y = 0.0f; // (volFloat)a0;  // Rotation um die y-Achse
			vr.z = 0.0f;
			break;
		// pressed	
		case 1:
			vt.x = (volFloat)a0;   // Translation in x- und y-Richtung
			vt.y = (volFloat)a1;
			vt.z = 0.0f;
			vr.x = 0.0f;			 // Keine Rotation
			vr.y = 0.0f;
			vr.z = 0.0f;
			break;	
		default: break;	
	}
	switch(b1)
	{
		// released
		case 0:
			break;
		// pressed	
		case 1:
			vt.x = 0.0f;
			vt.y = 0.0f;
			vt.z = 0.0f;
			vr.x = -(volFloat)a1;			 // Rotation 
			vr.y = (volFloat)a0;
			vr.z = 0.0f;
		 	break;
		default: break;			
	}
	switch(b2)
	{
		// released
		case 0:
			break;
		// pressed	
		case 1:
			vt.x = 0.0f;
			vt.y = 0.0f;
			vt.z = 0.0f;
			vr.x = 0.0f;			 // Rotation 
			vr.y = 0.0f;
			vr.z = (volFloat)a0;
		 	break;
		default: break;			
	}
	getNavSlot()->motion(goNavSlot::TRANSLATION,(void*)&vt);
	getNavSlot()->motion(goNavSlot::ROTATION,(void*)&vr);
}
