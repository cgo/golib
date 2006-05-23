#include <govideocapture.h>
#include <gostring.h>
#include <golog.h>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <asm/types.h>          /* for videodev2.h */
#include <linux/videodev2.h>
#include <linux/videodev.h>

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
        goVideoCapturePrivate() : deviceName(), fileDescriptor(-1), captureWidth (640), captureHeight (480) {};
        ~goVideoCapturePrivate() {};

        goString deviceName;
        int      fileDescriptor;
        goSize_t captureWidth;
        goSize_t captureHeight;
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

bool goVideoCapture::setCaptureSize (goSize_t width, goSize_t height)
{
    myPrivate->captureWidth = width;
    myPrivate->captureHeight = height;
    return true;
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

bool goVideoCapture::grab (goSignal3DBase<void>& target)
{
    if (target.getSizeX() < myPrivate->captureWidth || target.getSizeY() < myPrivate->captureHeight ||
        target.getChannelCount() != 3 || target.getDataType().getID() != GO_UINT8 ||
        target.getBlockSizeX() != target.getSizeX() || target.getBlockSizeY() != target.getSizeY())
    {
        goLog::warning ("grab(): Unsupported target.",this);
        return false;
    }
    //= This is for V4L version 1 (soon deprecated)
    struct video_capability cap;
    int result = xioctl(myPrivate->fileDescriptor, VIDIOCGCAP, &cap);
    if (result < 0)
    {
        printf ("ioctl failed.\n");
        if (EINVAL == errno) {
            fprintf (stderr, "%s is no V4L device\n",
                    myPrivate->deviceName.toCharPtr());
        }
        ::perror(0);
        return false;
    }
    if ((cap.type & VID_TYPE_CAPTURE) != 0)
    {
        //= Grab image
        printf ("Can grab!\n");

        //= Set image format (RGB24)
        struct video_picture vp;
        if (ioctl (myPrivate->fileDescriptor, VIDIOCGPICT, &vp) < 0)
        {
            printf ("VIDIOCGPICT:");
            perror(0);
            return false;
        }
        vp.palette = VIDEO_PALETTE_RGB24;
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
        //= Read RGB image
        ssize_t s = ::read (myPrivate->fileDescriptor, target.getPtr(), myPrivate->captureWidth*myPrivate->captureHeight*3);
        printf ("Read %d bytes\n",s);

        return true;
    }
    else
    {
        printf ("Cannot grab!\n");
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
