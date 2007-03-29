#include <gosignal3dref.h>

goSignal3DRef::goSignal3DRef ()
    : goSignal3DBase<void> ()
{
}

goSignal3DRef::goSignal3DRef (void* data_ptr, 
                       goTypeEnum data_type, 
                       const goSize3D& size, 
                       const goSize3D& blockSize, 
                       const goSize3D& borderSize, 
                       goSize_t channelCount)
    : goSignal3DBase<void> ()
{
    this->ref (data_ptr, data_type, size, blockSize, borderSize, channelCount);
}

goSignal3DRef::~goSignal3DRef ()
{
}

void goSignal3DRef::ref (void* data_ptr, 
                       goTypeEnum data_type, 
                       const goSize3D& size, 
                       const goSize3D& blockSize, 
                       const goSize3D& borderSize, 
                       goSize_t channelCount)
{
    this->setDataType (data_type);
    this->initialize (data_ptr, size.x, size.y, size.z,
                      blockSize.x, blockSize.y, blockSize.z, 
                      borderSize.x, borderSize.y, borderSize.z,
                      channelCount);
}

void goSignal3DRef::ref (goSignal3DBase<void>& s)
{
    this->ref (s.getPtr(), s.getDataType().getID(), s.getSize(), s.getBlockSize(), s.getBorderSize(), s.getChannelCount());
}

void goSignal3DRef::ref (goSignal3DBase<void>& s, const goSize3D& borderSize)
{
    this->ref (s.getPtr(), s.getDataType().getID(), s.getSize(), s.getBlockSize(), borderSize, s.getChannelCount());
}
