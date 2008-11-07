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

/** 
 * @brief Construct reference using data linearly stored in memory.
 * 
 * @param data_ptr   Pointer to linearly stored data, first x direction, then y, then z.
 * @param data_type  Data type.
 * @param sx         X size
 * @param sy         Y size (default: 1)
 * @param sz         Z size (default: 1)
 * @param channelCount  Channel count (default: 1)
 */
goSignal3DRef::goSignal3DRef (void* data_ptr,
                       goTypeEnum data_type,
                       goSize_t sx, goSize_t sy, goSize_t sz,
                       goSize_t channelCount )
{
    goSize3D s (sx, sy, sz);
    this->ref (data_ptr, data_type, s, s, goSize3D (0, 0, 0), channelCount);
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
