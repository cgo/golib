/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gonavkeyboard.h>
#include <SDL/SDL.h>
#include <golog.h>
#include <gothread.h>

goNavKeyboard::goNavKeyboard()
	: goNavDevice()
{
	SDL_EventState(SDL_KEYDOWN,SDL_ENABLE);
	SDL_EventState(SDL_KEYUP,SDL_ENABLE);
	SDL_EnableKeyRepeat(50,50);
	transX = 0;
	transY = 0;
	transZ = 0;
	rotX = 0;
	rotY = 0;
	rotZ = 0;
}

goNavKeyboard::~goNavKeyboard()
{
	thread.kill();
}

static 
void*
gokeyboard_thread(void* p)
{
	goNavKeyboard* js;
	js = (goNavKeyboard*)p;
	js->runThread();
	return 0;
}

void 
goNavKeyboard::run()
{
	cout << "goNavKeyboard initializing\n";
	thread.create(gokeyboard_thread,(void*)this,1);
}

void
goNavKeyboard::runThread()
{
	while(true)
	{
		SDL_Event event;
		if (SDL_WaitEvent(&event) == 0)
		{
			goLog::warning("goNavKeyboard::runThread() There was an error waiting for an SDL event.. returning");
			return;
		}
		// cout << "joystickthread returned from waitevent\n";
		// int numEvents;
		// threadsafe laut SDL Dokumentation
		// Problem: Andere events werden nicht von der queue genommen. So kann dieser Thread dauernd
		// laufen, da WaitEvent immer gleich zurueckkommt, wenn die anderen events nicht von einem
		// anderen thread aus der queue entfernt werden.
		// numEvents = SDL_PeepEvents(&event,1,SDL_GETEVENT,SDL_JOYAXISMOTION | SDL_JOYBUTTONDOWN | SDL_JOYBUTTONUP);
		if ( (event.type == SDL_KEYDOWN) )
		{
			cout << "Have keydown event!\n";
			SDL_Event e;
			while (SDL_PeepEvents(&e,1,SDL_GETEVENT,SDL_KEYDOWN) > 0);
			switch(event.key.keysym.mod)
			{
				case KMOD_LSHIFT:
					switch(event.key.keysym.sym)
					{
						case SDLK_w: if(rotX < 1.0) rotX += 0.05; break;
						case SDLK_s: if(rotX > -1.0) rotX -= 0.05; break;
						case SDLK_d: if(rotY < 1.0) rotY += 0.05; break;
						case SDLK_a: if(rotY > -1.0) rotY -= 0.05; break;
						case SDLK_r: if(rotZ > -1.0) rotZ -= 0.05; break;
						case SDLK_f: if(rotZ < 1.0) rotZ += 0.05; break;
						default: break; // ignore the rest
					}
					break;
				default:	
					switch(event.key.keysym.sym)
					{
						case SDLK_w: if(transZ < 1.0) transZ += 0.05; break;
						case SDLK_s: if(transZ > -1.0) transZ -= 0.05; break;
						case SDLK_d: if(transX < 1.0) transX += 0.05; break;
						case SDLK_a: if(transX > -1.0) transX -= 0.05; break;
						case SDLK_r: if(transY > -1.0) transY -= 0.05; break;
						case SDLK_f: if(transY < 1.0) transY += 0.05; break;
						default: break; // ignore the rest
					}
					break;
			}
			navDeviceUpdate();
		}
		if ( (event.type == SDL_KEYUP) )
		{
			cout << "Have keyup event!\n";
			SDL_Event e;
			while (SDL_PeepEvents(&e,1,SDL_GETEVENT,SDL_KEYUP) > 0);
			switch(event.key.keysym.mod)
			{
				case KMOD_LSHIFT:
					switch(event.key.keysym.sym)
					{
						case SDLK_w: rotX = 0.0; break;
						case SDLK_s: rotX = 0.0; break;
						case SDLK_a: rotY = 0.0; break;
						case SDLK_d: rotY = 0.0; break;
						case SDLK_r: rotZ = 0.0; break;
						case SDLK_f: rotZ = 0.0; break;
						default: break; // ignore the rest
					}
					break;
				default:	
					switch(event.key.keysym.sym)
					{
						case SDLK_w: transZ = 0.0; break;
						case SDLK_s: transZ = 0.0; break;
						case SDLK_a: transX = 0.0; break;
						case SDLK_d: transX = 0.0; break;
						case SDLK_r: transY = 0.0; break;
						case SDLK_f: transY = 0.0; break;
						default: break; // ignore the rest
					}
					break;
			}
			navDeviceUpdate();
		}
	}
}

void
goNavKeyboard::navDeviceUpdate()
{
	go3Vector<volFloat> vr;   // Geschwindigkeit [-1,+1] Rotation
	go3Vector<volFloat> vt;   // Geschwindigkeit [-1,+1] Translation
	
	vt.x = (volFloat)transX;  // Translation in X Richtung
	vt.y = (volFloat)transY;
	vt.z = (volFloat)transZ;  // Translation in Z Richtung
	getNavSlot()->motion(goNavSlot::TRANSLATION,(void*)&vt);
	vr.x = (volFloat)rotX;
	vr.y = (volFloat)rotY; // (volFloat)a0;  // Rotation um die y-Achse
	vr.z = (volFloat)rotZ;
	getNavSlot()->motion(goNavSlot::ROTATION,(void*)&vr);
}
