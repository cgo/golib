#ifndef GOVIDEOCAPTURE_H
#define GOVIDEOCAPTURE_H

//#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
//#endif
#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif
#ifndef GOSIGNAL3D_H
# include <gosignal3d.h>
#endif

class goVideoCapturePrivate;

/*!
 * @example videocapture/videocapture.cpp
 */
/**
 * \addtogroup video
 * @{ 
 */
/** 
 * @brief Video capture class.
 *
 * This class allows video capture, currently only from video4linux (version 1) devices. 
 * Version 2 will follow when the drivers for our cameras support it and there is need.
 *
 * @note Please report when a camera/driver works witht this class, it's fairly new and
 * my first time using v4l.
 *
 * @author Christian Gosch
 */
class goVideoCapture : public goObjectBase
{
    public:
        
        enum ColourMode
        {
            RGB24,          // 24 bits RGB (8 bits each channel)
            YUV422P,        // YUV 4:2:2 planar
            YUV420P,        // YUV 4:2:0 planar
            GREY            // 8 bits grey
        };
        
        goVideoCapture ();
        goVideoCapture (const char* devname, goSize_t width, goSize_t height);
        virtual ~goVideoCapture ();

        bool setDevice         (const char* name);
        void setFileDescriptor (int fd);
        int  getFileDescriptor () const;
        bool setCaptureSize    (goSize_t width, goSize_t height);
        goSize_t getCaptureWidth  () const;
        goSize_t getCaptureHeight () const;
        void setColourMode     (enum ColourMode vm);
        enum ColourMode getColourMode () const;
        bool open              ();
        void close             ();
        bool grab              (goSignal3DBase<void>& signal);
        bool grab              (goSignal3D<void>& signal);
        bool grab              (void* buffer, goSize_t size);

        bool checkCapture      () const;
        
        bool setSettings       ();
        void getSettings       ();

    protected:
        void getCapabilities   ();
        void getCaptureWindow  ();
        
    private:
        goVideoCapturePrivate* myPrivate;
};
/** @} */

#endif
