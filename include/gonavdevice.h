#ifndef GO_NAVDEVICE_H
#define GO_NAVDEVICE_H

#include <gotypes.h>

class goNavSlot;
/*!
 * Navigation device interface. This class is considered abstract.
 * Provides an interface for navigation devices such as joysticks, data gloves,
 * mice, you name it.
 * A goNavDevice is connected using the connect() method to a 
 * goNavSlot. connect() is implemented in the base class and simply sets
 * an internal pointer to a given goNavSlot object.
 * The navDeviceUpdate() must be reimplemented by a subclass. 
 * This method would typically be called when an application wants the 
 * device to update some measured values from an input device (such as the axis
 * position or button state of a joystick). navDeviceUpdate() then typically
 * calls the goNavSlot::motion() method of the goNavSlot set by 
 * connect() with some appropriate parameters. The parameters depend on the 
 * application you want to use the device for and what kind of device it
 * is.
 * @date 20.11.2001
 * @author Christian Gosch
 * @see goNavSlot
 */
class goNavDevice
{
	public:
		goNavDevice();
		virtual ~goNavDevice();
	
		/*!
		 * Sets the goNavSlot to tell any motion about.
		 */
		void connect(goNavSlot* slot);
		/*!
		 * Queries any device information and calls the appropriate methods in the
		 * goNavSlot set with connect().
		 */
		virtual void navDeviceUpdate();
	    goNavSlot* getNavSlot() { return navSlot; }	
	private:
		goNavSlot *navSlot;
};

/*!
 * Interface for a navigation slot used by a 
 * goNavDevice navigation device to issue motion commands.
 * A subclass must reimplement the motion() method. For details see
 * the documentation for goNavDevice.
 * @see goNavDevice
 * @date 20.11.2001
 * @author Christian Gosch
 */
class goNavSlot
{
	
	public:
		/// Motion types
		enum motionType {
			ROTATION = 0,
			TRANSLATION,
			POSITIONSET
		};		
		goNavSlot();
		virtual ~goNavSlot();
		
		/// Must be reimplemented by a subclass
		virtual void motion(goNavSlot::motionType t, void* arg);
};

#endif
