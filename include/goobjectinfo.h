#ifndef GOOBJECTINFO_H
#define GOOBJECTINFO_H

#include <gotypes.h>

/*!
 * Interface for object information such as size in bytes.
 * @author Christian Gosch
 * @date 3.9.2001
 */
class goObjectInfo
{
 public:
    goObjectInfo();
    virtual ~goObjectInfo();
    /*!
     * @return Size in bytes of this object.
     */
    virtual goSize_t memoryUsage();
};

#endif
