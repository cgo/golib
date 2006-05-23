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
        goVideoCapture();
        virtual ~goVideoCapture();

        bool setDevice      (const char* name);
        bool setCaptureSize (goSize_t width, goSize_t height);
        bool open           ();
        void close          ();
        bool grab           (goSignal3DBase<void>& signal);

    private:
        goVideoCapturePrivate* myPrivate;
};

#endif
