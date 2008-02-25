#include <gosignaloperation.h>
#include <gofileio.h>
#include <goplot.h>
#include <gosignal3d.h>
#include <gosignalhelper.h>
#include <gofunctor.h>
#include <gotimerobject.h>

    class Blah
    {
        public:
        goDouble divit (const goDouble& s)
        {
            return s * 0.1;
        };
    };
int main ()
{
    goSignal3D<void> image;
    goFileIO::readImage ("/home/christian/Documents/bilder/Dani/Bilder DigiCam I 050.jpg", &image);
    goSignal3D<void> fimage;
    fimage.setDataType (GO_FLOAT);
    fimage.make (image.getSize(), image.getSize(), image.getBorderSize (), 1);
    goRGBAtoScalar (&image, &fimage);

    goSignal3D<void> fimage2;
    fimage2.make (&fimage);

    Blah blah;
    goSignalOperation2 operation;
    operation.setKernelMethod (goMemberFunction<Blah, goDouble, const goDouble&> (&blah, &Blah::divit));
    goTimerObject to;
    to.startTimer ();
    operation (fimage, fimage2);
    to.stopTimer ();
    printf ("Seconds: %f\n", to.getTimerSeconds ());

    goPlot::Plot p;
    p.setPrefix ("set yrange [:] reverse\nset palette grey\nset cbtics\n");
    p.plotImage (fimage);
    p.plotImage (fimage2, "", "w image", 1, 0);
    p.plotPause ();

    printf ("min/max 1: %f %f\n", fimage.getMinimum(), fimage.getMaximum ());
    printf ("min/max 2: %f %f\n", fimage2.getMinimum(), fimage2.getMaximum ());

}
