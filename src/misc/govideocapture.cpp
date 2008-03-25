#include <govideocapture.h>
#include <gostring.h>
#include <golog.h>
#include <goconfig.h>
#include <gomath.h>
#include <gofixedarray.h>
#include <gocolourspace.h>

#include <fcntl.h>
#include <unistd.h>
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
# ifdef __STRICT_ANSI__
#  define FOO__STRICT_ANSI__
#  undef __STRICT_ANSI__
# endif
# include <linux/videodev.h>
# ifdef FOO__STRICT_ANSI__
#  define __STRICT_ANSI__
#  undef FOO__STRICT_ANSI__
# endif
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
            colourMode     (goVideoCapture::RGB24),
            grabBuffer     (0)
        {
#if HAVE_LINUX_VIDEODEV_H
            memset (&this->cap, 0, sizeof(struct video_capability));
#endif
            brightnessRange[0] = 0;
            brightnessRange[1] = 65535;
            hueRange[0] = 0;
            hueRange[1] = 65535;
            colourRange[0] = 0;
            colourRange[1] = 65535;
            contrastRange[0] = 0;
            contrastRange[1] = 65535;
            whitenessRange[0] = 0;
            whitenessRange[1] = 65535;
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
        goFixedArray<goUInt8> grabBuffer;

        int brightnessRange[2];
        int hueRange[2];
        int colourRange[2];
        int contrastRange[2];
        int whitenessRange[2];
};


goVideoCapture::goVideoCapture ()
    : goObjectBase (),
      myPrivate (0)
{
    this->setClassID(GO_VIDEOCAPTURE);
    myPrivate = new goVideoCapturePrivate;
}

goVideoCapture::goVideoCapture (const char* devname, goSize_t width, goSize_t height)
    : goObjectBase (),
      myPrivate (0)
{
    this->setClassID(GO_VIDEOCAPTURE);
    myPrivate = new goVideoCapturePrivate;

    this->setDevice (devname);
    this->open ();
    this->getSettings ();
    this->setCaptureSize (width, height);
    this->setSettings ();
}

goVideoCapture::~goVideoCapture ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

/** 
 * @brief Set device file name.
 * 
 * @param name Device file name, e.g. "/dev/video0"
 * 
 * @return True if successful, false otherwise.
 */
bool goVideoCapture::setDevice (const char* name)
{
    myPrivate->deviceName = name;
    return true;
}

/** 
 * @brief Set file discriptor of an already open device.
 * 
 * Sets the descriptor and automatically reads settings from the device
 * to update the settings stored in the goVideoCapture object.
 *
 * @param fd File descriptor.
 */
void goVideoCapture::setFileDescriptor (int fd)
{
    myPrivate->fileDescriptor = fd;
    this->getSettings();
    // this->getCapabilities ();
    // this->getCaptureWindow ();
}

/** 
 * @brief Get file descriptor for open device.
 * 
 * @return File descriptor. Negative means no device is open.
 */
int goVideoCapture::getFileDescriptor () const
{
    return myPrivate->fileDescriptor;
}

/** 
 * @brief Set capture frame size.
 * 
 * @param width Width in pixels.
 * @param height Height in pixels.
 * 
 * @return True if successful, false otherwise.
 */
bool goVideoCapture::setCaptureSize (goSize_t width, goSize_t height)
{
    myPrivate->captureWidth = width;
    myPrivate->captureHeight = height;
    return true;
}

/** 
 * @brief Capture frame width.
 * 
 * @return Frame width in pixels.
 */
goSize_t goVideoCapture::getCaptureWidth () const
{
    return myPrivate->captureWidth;
}

/** 
 * @brief Capture frame height.
 * 
 * @return Frame height in pixels.
 */
goSize_t goVideoCapture::getCaptureHeight () const
{
    return myPrivate->captureHeight;
}

/** 
 * @brief Sets the colour mode.
 *
 * @note Care should be taken when forcing a colour mode and setting it with setSettings().
 * Apparently some devices don't support most colour modes or I'm not using the v4l interface right.
 * Either way, it didn't work with my cameras. So just use the format provided by the camera for now.
 * 
 * @param m Colour mode enumerator.
 */
void goVideoCapture::setColourMode (enum goVideoCapture::ColourMode m)
{
    myPrivate->colourMode = m;
}

/** 
 * @brief Get the colour mode this object is currently set to.
 * 
 * @return The colour mode enumerator.
 */
enum goVideoCapture::ColourMode goVideoCapture::getColourMode () const
{
    return myPrivate->colourMode;
}

/** 
 * @brief Open the device.
 * 
 * @see setDevice()
 *
 * @return True if successful, false otherwise.
 */
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

/** 
 * @brief Close the open device.
 */
void goVideoCapture::close ()
{
    if (myPrivate->fileDescriptor >= 0)
    {
        ::close (myPrivate->fileDescriptor);
        myPrivate->fileDescriptor = -1;
    }
}


/** 
 * @brief Grab a frame from the open device.
 * 
 * @note No data conversion is done. The data is as-is from the video device.
 *
 * @param target Target pointer to memory of size sz bytes.
 * @param sz Maximum of sz bytes is read from the open device.
 * 
 * @return True if successful, false otherwise.
 */
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

/** 
 * @brief Convenience method.
 * 
 * Calls grab (goSignal3DBase<void>&), changing type and size of target
 * if necessary.
 * 
 * @param target Target containing the image if return value is true.
 * 
 * @return True if successful, false otherwise.
 */
bool goVideoCapture::grab (goSignal3D<void>& target)
{
    if (target.getSizeX() != this->getCaptureWidth() || target.getSizeY() != this->getCaptureHeight() || target.getSizeX() != target.getBlockSizeX() || target.getSizeY() != target.getBlockSizeY() || target.getChannelCount() != 3 || target.getDataType().getID() != GO_UINT8)
    {
        target.setDataType (GO_UINT8);
        goSize3D sz (this->getCaptureWidth(), this->getCaptureHeight(), 1);
        goSize3D border (goMath::min<int> (16, sz.x),
                         goMath::min<int> (16, sz.y), 0);
        target.make (sz, sz, border, 3);
    }

    return this->grab (*static_cast<goSignal3DBase<void>*> (&target));
}

/** 
 * @brief Grab a frame from the open device.
 * 
 * @param target Target. Must currently be linear in memory (block size == signal size), of type GO_UINT8 and
 * must have 3 channels if the colour mode is RGB24.
 * 
 * @return True if successful, false otherwise.
 */
bool goVideoCapture::grab (goSignal3DBase<void>& target)
{
#if HAVE_LINUX_VIDEODEV_H
    switch (myPrivate->colourMode)
    {
        case RGB24:
            {
                if (target.getSizeX() < myPrivate->captureWidth || target.getSizeY() < myPrivate->captureHeight ||
                        target.getDataType().getID() != GO_UINT8 ||
                        target.getBlockSizeX() != target.getSizeX() || target.getBlockSizeY() != target.getSizeY())
                {
                    goLog::warning ("grab RGB24 : Unsupported target. Data type must be uint8 and block size must equal size.",this);
                    return false;
                }
                if (target.getChannelCount() != 3)
                {
                    goLog::warning ("grab(): channel count must be 3 for rgb grabbing.",this);
                    return false;
                }
                else
                {
                    return this->grab (target.getPtr(), target.getSizeX() * target.getSizeY() * target.getChannelCount());
                }
            }
            break;
        case YUV422P:
        case YUV420P:
            {
                if (myPrivate->grabBuffer.getSize() != myPrivate->captureWidth * myPrivate->captureHeight * 3)
                {
                    myPrivate->grabBuffer.setSize (myPrivate->captureWidth * myPrivate->captureHeight * 3);
                }
                if (!this->grab (myPrivate->grabBuffer.getPtr(), myPrivate->grabBuffer.getSize()))
                {
                    return false;
                }
                goYUV420P_RGB_base (myPrivate->grabBuffer.getPtr(), myPrivate->captureWidth, myPrivate->captureHeight, target);
                return true;
            }
            break;
        default: break;
    }
    return false;
#else
    goLog::warning ("grab(): compiled without HAVE_VIDEODEV_H.", this);
    return false;
#endif
}

/** 
 * @brief Check if video capture is available by the open device.
 * 
 * @return True if capture is available, false otherwise.
 */
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

/** 
 * @brief Gets device capabilities and stores them internally.
 * Called, amongst others, by getSettings().
 */
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

/** 
 * @brief Store settings from this object to the open device.
 * 
 * @return True if successful, false otherwise. Also check the logfile.
 */
bool goVideoCapture::setSettings ()
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
        default: goLog::warning ("setSettings(): unknown colour mode.",this); return false; break;
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

/** 
 * @brief Get settings from device and store them internally.
 * It is a good idea to call this after opening a device.
 * It is also called by setFileDescriptor().
 * This method calls getCapabilities() and getCaptureWindow().
 */
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

/** 
 * @brief Get the size of the capture frame from the device
 * and store it internally.
 * It can be retrieved with getCapture[Width|Height]().
 */
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

static void getImageProp (struct video_picture* ret, int fd)
{
    if (xioctl (fd, VIDIOCGPICT, &ret) < 0)
    {
        printf ("VIDIOCGPICT:");
        perror(0);
        return;
    }
}

static bool setImageProp (struct video_picture* vp, int fd)
{
    if (xioctl (fd, VIDIOCSPICT, &vp) < 0)
    {
        printf ("VIDIOCSPICT:");
        perror(0);
        return false;
    }
    return true;
}

/**
 * @brief Get valid ranges for brightnes etc.
 * @todo Does not work yet. a) doesn't find the real maximum value the way it works now, b) gets invalid argument all
 * the time.
 */
void goVideoCapture::getValidRanges ()
{
    //= Find the max. values
    struct video_picture vp;
    int i = 0, step = 0;
    bool failed = false;
#define FIND_MAX(entry) \
    getImageProp (&vp, myPrivate->fileDescriptor); \
    i = 65535; \
    step = i / 2; \
    do \
    { \
        vp. entry = i; \
        failed = !setImageProp (&vp, myPrivate->fileDescriptor); \
        if (failed) \
        { \
            if (i > 0) \
                i = i - i / 2; \
        } \
        else \
        { \
            if (i <= 65535) \
                i = i + (65535 - i) / 2; \
        } \
        step = step / 2; \
        printf (#entry " value: %d\n", i); \
    } while (i > 1 && i < 65535 && failed); \
    myPrivate-> entry ## Range[1] = i;

    FIND_MAX(brightness);
    FIND_MAX(hue);
    FIND_MAX(colour);
    FIND_MAX(contrast);
    FIND_MAX(whiteness);
#undef FIND_MAX
}

#define SET_GET_PICT(Entry, entry) \
void goVideoCapture::set ## Entry (double b) \
{ \
    struct video_picture vp; \
    getImageProp (&vp, myPrivate->fileDescriptor); \
    vp. entry = (int) ( (b) * (double)myPrivate-> entry ## Range[0] + (1.0 - b) * (double)myPrivate-> entry ## Range[1] ); \
    printf ("Setting " #entry " to %lf\n", b); \
    setImageProp (&vp, myPrivate->fileDescriptor); \
    getImageProp (&vp, myPrivate->fileDescriptor); \
    printf (#Entry " is now %d\n", vp. entry); \
} \
 \
double goVideoCapture::get ## Entry () const \
{ \
    struct video_picture vp; \
    getImageProp (&vp, myPrivate->fileDescriptor); \
    return (double) ( (vp. entry) - myPrivate-> entry ## Range[0] ) / (double)(myPrivate-> entry ## Range[1] - myPrivate-> entry ## Range[0] ); \
}

SET_GET_PICT(Brightness, brightness);
SET_GET_PICT(Hue, hue);
SET_GET_PICT(Colour, colour);
SET_GET_PICT(Contrast, contrast);
SET_GET_PICT(Whiteness, whiteness);

#undef SET_GET_PICT

#if 0
void goVideoCapture::setHue (int b)
{
    struct video_picture vp;
    getImageProp (&vp, myPrivate->fileDescriptor);
    vp.hue = b;
    setImageProp (&vp, myPrivate->fileDescriptor);
}

int goVideoCapture::getHue () const
{
    struct video_picture vp;
    getImageProp (&vp, myPrivate->fileDescriptor);
    return vp.hue;
}

void goVideoCapture::setColour (int b)
{
    struct video_picture vp;
    getImageProp (&vp, myPrivate->fileDescriptor);
    vp.colour = b;
    setImageProp (&vp, myPrivate->fileDescriptor);
}

int goVideoCapture::getColour () const
{
    struct video_picture vp;
    getImageProp (&vp, myPrivate->fileDescriptor);
    return vp.colour;
}

void goVideoCapture::setContrast (int b)
{
    struct video_picture vp;
    getImageProp (&vp, myPrivate->fileDescriptor);
    vp.contrast = b;
    setImageProp (&vp, myPrivate->fileDescriptor);
}

int goVideoCapture::getContrast () const
{
    struct video_picture vp;
    getImageProp (&vp, myPrivate->fileDescriptor);
    return vp.contrast;
}

void goVideoCapture::setWhiteness (int b)
{
    struct video_picture vp;
    getImageProp (&vp, myPrivate->fileDescriptor);
    vp.whiteness = b;
    setImageProp (&vp, myPrivate->fileDescriptor);
}

int goVideoCapture::getWhiteness () const
{
    struct video_picture vp;
    getImageProp (&vp, myPrivate->fileDescriptor);
    return vp.whiteness;
}
#endif
