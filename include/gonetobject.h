/*
 * This file and the programs contained in it and in associated files
 * are copyright 2002 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 */

#ifndef GONETOBJECT_H
#define GONETOBJECT_H

#include <goserver.h>
#include <gothreadobject.h>

namespace goNet {

/*!
 * \addtogroup net
 * @{
 */
/**
 * @brief Networked object.
 *
 * \todo Add documentation
**/
class
goNetObject : public goThreadObject
{
	public:
		goNetObject(int listenPort);
		virtual ~goNetObject();

		virtual void threadMethod(); 
	protected:
			
	private:
		goServer server;
};
/*! @} */
};
#endif
