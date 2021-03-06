/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GO_NAVKEYBOARD_H
#define GO_NAVKEYBOARD_H

#include <gonavdevice.h>
#include <SDL/SDL.h>
#include <goarray.h>
#include <gothread.h>

namespace Vol {

/*!
 * Joystick is a goNavDevice using an SDL joystick.
 * Works only with SDL. 
 * Expects a readily initialized SDL, with SDL_INIT_JOYSTICK in the init call.
 * goNavKeyboard runs a thread to poll SDL events if you call the run() method.
 * The thread calls the motion() method of the connected goNavSlot object
 * via the navDeviceUpdate() method if there is change in the joystick state.
 * This class was made to work together with goVolumeNavigator (which is 
 * a goNavSlot object).
 * @date 20.11.2001
 * @author Christian Gosch
 * @see goNavDevice
 * @see goNavSlot
 * @see goVolumeNavigator
 */
class goNavKeyboard : public goNavDevice
{
	public:
		goNavKeyboard();
		virtual ~goNavKeyboard();
		
		/*!
		 * Updates the joystick state, calls navStore's motion() method accordingly.
		 */
		virtual void navDeviceUpdate();
		/*!
		 * Starts a thread that peeks SDL events and runs navDeviceUpdate() accordingly.
		 */
		void run();
		/*!
		 * The actual thread function.
		 */
		void runThread();
	private:	
		goFloat			transX, transY, transZ;
		goFloat			rotX, rotY, rotZ;
		goThread		thread;
};
};
#endif
