#ifndef GOCACHE_H
#define GOCACHE_H

#include <gotypes.h>
#include <goarray.h>

class 
goCache
{
 public:
    goCache();
    virtual ~goCache();
    
    // Max size in bytes
    void cacheSetMaxSize (goSize_t max);
    // Add sz bytes to currentSize
    void cacheAddSize (goSize_t sz);
    // Subtract sz from currentSize
    void cacheSubSize (goSize_t sz);
    // Add identifier with size sz to internal bookkeeping structures. Return true if successful, false if
    // not enough memory is available. In that case, cacheRemoveElement and free the appropriate memory
    // until you get true back.
    bool cacheAddElement (goSize_t identifier, goSize_t sz);
    // Remove the next appropriate element from internal bookkeeping and return the id
    goSize_t cacheRemoveElement ();
    // Remove the element with the given identifier
    void cacheRemoveElement (goSize_t identifier);
    // Keep track of one access to element "identifier"
    void cacheAccessElement (goSize_t identifier);
    
 private:
    // Usage counter for each id entry
    goArray<goSize_t> usage;
    // IDs of the cached elements
    goArray<goSize_t> id;
    // free indices in id/usage arrays
    goArray<goSize_t> freeIndices;
    // Size of each elemend in id
    goArray<goSize_t> size;

    goSize_t maxSize;
    goSize_t currentSize;
};



#endif
