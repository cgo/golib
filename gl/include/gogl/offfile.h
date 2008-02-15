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
