#include <govideocapture.h>
#include <gostring.h>
#include <golog.h>
#include <goconfig.h>
#include <gomath.h>

#include <fcntl.h>
#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#include <errno.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <asm/types.h>          /* for videodev2.h */
//#if HAVE_LINUX_VIDEODEV2_H
//# include <linux/videodev2.h>
//#endif
#if HAVE_LINUX_VIDEODEV_H
# include <linux/videodev.h>
#endif

static int xioctl (int    fd,
                   int    request,
                   void * arg)
{
    int r;
    do r = ioctl (fd, request, arg);
    while (-1 == r && EINTR == errno);
    return r;
}

class goVideoCapturePrivate
{
    public:
        goVideoCapturePrivate() : 
            deviceName(), 
            fileDescriptor (-1), 
            captureWidth   (640), 
            captureHeight  (480), 
            colourMode     (goVideoCapture::RGB24) 
        {
#if HAVE_LINUX_VIDEODEV_H
            memset (&this->cap, 0, sizeof(struct video_capability));
#endif
        };
        ~goVideoCapturePrivate() {};

        goString deviceName;
        int      fileDescriptor;
        goSize_t captureWidth;
        goSize_t captureHeight;
        enum goVideoCapture::ColourMode colourMode;
#if HAVE_LINUX_VIDEODEV_H
        struct video_capability cap;
#endif
};

goVideoCapture::goVideoCapture ()
    : goObjectBase (),
      myPrivate (0)
{
    this->setClassName ("goVideoCapture");
    myPrivate = new goVideoCapturePrivate;
}

goVideoCapture::~goVideoCapture ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

bool goVideoCapture::setDevice (const char* name)
{
    myPrivate->deviceName = name;
    return true;
}

void goVideoCapture::setFileDescriptor (int fd)
{
    myPrivate->fileDescriptor = fd;
    this->getSettings();
    // this->getCapabilities ();
    // this->getCaptureWindow ();
}

int goVideoCapture::getFileDescriptor () const
{
    return myPrivate->fileDescriptor;
}

bool goVideoCapture::setCaptureSize (goSize_t width, goSize_t height)
{
    myPrivate->captureWidth = width;
    myPrivate->captureHeight = height;
    return true;
}

goSize_t goVideoCapture::getCaptureWidth () const
{
    return myPrivate->captureWidth;
}

goSize_t goVideoCapture::getCaptureHeight () const
{
    return myPrivate->captureHeight;
}

void goVideoCapture::setColourMode (enum goVideoCapture::ColourMode m)
{
    myPrivate->colourMode = m;
}

enum goVideoCapture::ColourMode goVideoCapture::getColourMode () const
{
    return myPrivate->colourMode;
}

bool goVideoCapture::open ()
{
    int result = ::open (myPrivate->deviceName.toCharPtr(), O_RDWR); 
    if (result < 0)
    {
        return false;
    }
    myPrivate->fileDescriptor = result;
    return true;
}

void goVideoCapture::close ()
{
    if (myPrivate->fileDescriptor >= 0)
    {
        ::close (myPrivate->fileDescriptor);
        myPrivate->fileDescriptor = -1;
    }
}

bool goVideoCapture::initDevice ()
{
#if HAVE_LINUX_VIDEODEV_H
    //= This is for V4L version 1 (soon deprecated)
    this->getCapabilities ();
    //= Set image format (RGB24)
    struct video_picture vp;
    if (ioctl (myPrivate->fileDescriptor, VIDIOCGPICT, &vp) < 0)
    {
        printf ("VIDIOCGPICT:");
        perror(0);
        return false;
    }
    switch (myPrivate->colourMode)
    {
        case RGB24: vp.palette = VIDEO_PALETTE_RGB24; break;
        case YUV422P: vp.palette = VIDEO_PALETTE_YUV422P; break;
        case YUV420P: vp.palette = VIDEO_PALETTE_YUV420P; break;
        case GREY: vp.palette = VIDEO_PALETTE_GREY; break;
        default: goLog::warning ("initDevice(): unknown colour mode.",this); return false; break;
    }
    if (ioctl (myPrivate->fileDescriptor, VIDIOCSPICT, &vp) < 0)
    {
        printf ("VIDIOCSPICT:");
        perror(0);
        return false;
    }

    //= Set capture window size
    struct video_window vw;
    if (ioctl (myPrivate->fileDescriptor, VIDIOCGWIN, &vw) < 0)
    {
        printf ("VIDIOCGWIN:");
        perror(0);
        return false;
    }
    vw.width = myPrivate->captureWidth;
    vw.height = myPrivate->captureHeight;
    if (ioctl (myPrivate->fileDescriptor, VIDIOCSWIN, &vw) < 0)
    {
        printf ("VIDIOCSWIN:");
        perror(0);
        return false;
    }
    return true;
#else
    return false;
#endif
}

bool goVideoCapture::grab (void* target, goSize_t sz)
{
    //= Read RGB image
    if (this->checkCapture())
    {
        goSize_t captureSize = 0;
        captureSize = myPrivate->captureWidth*myPrivate->captureHeight;
        if (myPrivate->colourMode == RGB24 ||
            myPrivate->colourMode == YUV422P ||
            myPrivate->colourMode == YUV420P)
        {
            captureSize *= 3;
        }
        ::read (myPrivate->fileDescriptor, target, goMath::min<goSize_t>(captureSize, sz));
        // printf ("Read %d bytes.\n",s);
        return true;
    }
    else
    {
        goLog::warning("grab(): Cannot grab.",this);
        return false;
    }
    //= Video4Linux version 2
#if 0
    struct v4l2_capability cap;
    int result = xioctl(myPrivate->fileDescriptor, VIDIOC_QUERYCAP, &cap);
    if (result < 0)
    {
        printf ("ioctl failed.\n");
        if (EINVAL == errno) {
            fprintf (stderr, "%s is no V4L2 device\n",
                    myPrivate->deviceName.toCharPtr());
        }
        ::perror(0);
        return false;
    }
    if ((cap.capabilities & V4L2_CAP_VIDEO_CAPTURE) != 0)
    {
        //= Grab image
        printf ("Can grab!\n");
        return true;
    }
    else
    {
        printf ("Cannot grab!\n");
        return false;
    }
#endif
}

bool goVideoCapture::grab (goSignal3DBase<void>& target)
{
#if HAVE_LINUX_VIDEODEV_H
    if (target.getSizeX() < myPrivate->captureWidth || target.getSizeY() < myPrivate->captureHeight ||
        target.getDataType().getID() != GO_UINT8 ||
        target.getBlockSizeX() != target.getSizeX() || target.getBlockSizeY() != target.getSizeY())
    {
        goLog::warning ("grab(): Unsupported target.",this);
        return false;
    }
    switch (myPrivate->colourMode)
    {
        case RGB24:
            {
                if (target.getChannelCount() != 3)
                {
                    goLog::warning ("grab(): channel count must be 3 for rgb grabbing.",this);
                    return false;
                }
            }
            break;
        default: break;
    }
    return this->grab (target.getPtr(), target.getSizeX() * target.getSizeY() * target.getChannelCount());
#else
    return false;
#endif
}

bool goVideoCapture::checkCapture () const
{
#if HAVE_LINUX_VIDEODEV_H
    //= This is for V4L version 1 (soon deprecated)
    if ((myPrivate->cap.type & VID_TYPE_CAPTURE) != 0)
    {
        return true;
    }
    return false;
#else
    return false;
#endif
}

void goVideoCapture::getCapabilities ()
{
#if HAVE_LINUX_VIDEODEV_H
    //= This is for V4L version 1 (soon deprecated)
    int result = xioctl(myPrivate->fileDescriptor, VIDIOCGCAP, &myPrivate->cap);
    if (result < 0)
    {
        printf ("ioctl failed.\n");
        if (EINVAL == errno) {
            fprintf (stderr, "%s is no V4L device\n",
                    myPrivate->deviceName.toCharPtr());
        }
        ::perror(0);
    }
#endif
}

void goVideoCapture::getSettings()
{
#if HAVE_LINUX_VIDEODEV_H
    this->getCapabilities();
    //= Get other settings.
    struct video_picture vp;
    if (ioctl (myPrivate->fileDescriptor, VIDIOCGPICT, &vp) < 0)
    {
        printf ("VIDIOCGPICT:");
        perror(0);
        return;
    }
    switch (vp.palette)
    {
        case VIDEO_PALETTE_RGB24: myPrivate->colourMode = RGB24; break;
        case VIDEO_PALETTE_YUV422P: myPrivate->colourMode = YUV422P; break;
        case VIDEO_PALETTE_YUV420P: myPrivate->colourMode = YUV420P; break;
        case VIDEO_PALETTE_GREY: myPrivate->colourMode = GREY; break;
        default: goLog::warning ("getCapabilities(): unsupported colour mode.",this); return; break;
    }
    //= Get capture window size
    this->getCaptureWindow();
#endif
}

void goVideoCapture::getCaptureWindow ()
{
#if HAVE_LINUX_VIDEODEV_H
    struct video_window vw;
    if (ioctl (myPrivate->fileDescriptor, VIDIOCGWIN, &vw) < 0)
    {
        printf ("VIDIOCGWIN:");
        perror(0);
        return;
    }
    myPrivate->captureWidth = vw.width;
    myPrivate->captureHeight = vw.height;
#endif
}
