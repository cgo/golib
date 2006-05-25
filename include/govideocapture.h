#ifndef GOVIDEOCAPTURE_H
#define GOVIDEOCAPTURE_H

#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif

class goVideoCapturePrivate;

class goVideoCapture : public goObjectBase
{
    public:
        
        enum ColourMode
        {
            RGB,
            GREY
        };
        
        goVideoCapture();
        virtual ~goVideoCapture();

        bool setDevice         (const char* name);
        void setFileDescriptor (int fd);
        int  getFileDescriptor () const;
        bool setCaptureSize    (goSize_t width, goSize_t height);
        goSize_t getCaptureWidth  () const;
        goSize_t getCaptureHeight () const;
        void setColourMode     (enum ColourMode vm);
        bool open              ();
        void close             ();
        bool initDevice        ();
        bool grab              (goSignal3DBase<void>& signal);
        bool grab              (void* buffer, goSize_t size);

        bool checkCapture      () const;

    protected:
        void getCapabilities   ();
        void getCaptureWindow  ();
        
    private:
        goVideoCapturePrivate* myPrivate;
};

#endif
