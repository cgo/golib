#ifndef GOSIGNAL3DREF_H
#define GOSIGNAL3DREF_H

#include <gosignal3dbase.h>

class goSignal3DRef : public goSignal3DBase<void>
{
    public:
        goSignal3DRef ();
        goSignal3DRef (void* data_ptr, 
                       goTypeEnum data_type, 
                       const goSize3D& size, 
                       const goSize3D& blockSize, 
                       const goSize3D& borderSize, 
                       goSize_t channelCount);
        virtual ~goSignal3DRef ();
        void ref (void* data_ptr, 
                  goTypeEnum data_type, 
                  const goSize3D& size, 
                  const goSize3D& blockSize, 
                  const goSize3D& borderSize, 
                  goSize_t channelCount);
        void ref (goSignal3DBase<void>& s);
        void ref (goSignal3DBase<void>& s, const goSize3D& borderSize);
};

#endif
