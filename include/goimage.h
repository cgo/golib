#ifndef GOIMAGE_H
#define GOIMAGE_H

#include <goimagebuffer.h>

class goImagePrivate;

class goImage : goObjectBase 
{
    public:
        goImage  ();
        virtual ~goImage ();
    
    private:
        goImagePrivate* myPrivate;
};

#endif /* GOIMAGE_H */
