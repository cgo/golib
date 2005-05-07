/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 */

#include <goconfig.h>
#include <goenvironment.h>

bool goGetEnv (const char* name, goString& ret)
{
#ifndef HAVE_GETENV
    return false;
#else
    const char* value = 0;
    value = ::getenv (name);
    if (!value)
    {
        ret = "";
    }
    else
    {
        ret = value;
    }
    return true;
#endif
}
