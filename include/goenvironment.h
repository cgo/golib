/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id: goenvironment.h,v 1.1.1.1 2006/04/19 15:27:04 gosch Exp $
 */

#ifndef GOENVIRONMENT_H
#define GOENVIRONMENT_H

#ifndef GOSTRING_H
# include <gostring.h>
#endif

bool goGetEnv (const char* variable, goString& retVal);

#endif
