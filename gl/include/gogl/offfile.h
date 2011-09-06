/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGL_OFFFILE_H
#define GOGL_OFFFILE_H

#include <goofffile.h>
#ifndef GOVECTOR_H
# include <govector.h>
#endif

namespace goGL
{
    // class OFFFilePrivate;

    class OFFFilePrivate;

/** @addtogroup gl
 * @{
 */
    class OFFFile : public goOFFFile
    {
        public:
            OFFFile ();
            virtual ~OFFFile ();
            
            virtual bool read (const char* filename);

            bool toList (int listName);
            bool draw ();

        private:
            OFFFile (OFFFile&);
            OFFFile& operator= (OFFFile&);

            OFFFilePrivate* myPrivate;
            // goGL::OFFFilePrivate* myPrivate;
    };
/** 
 * @}
 */
};

#endif
