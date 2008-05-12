#include <gomath.h>
#include <godenoise.h>
#include <stdio.h>
#include <stdlib.h>
#include <gofileio.h>
#include <gosignalhelper.h>
#include <gtkmm.h>
#include <gogui/imageview.h>

int main (int argc, char* argv[])
{
    if (argc < 2)
    {
        printf ("Usage: %s <image file>\n", argv[0]);
        exit (1);
    }

    const char* fname = argv[1];

    goSignal3D<void> image;
    if (!goFileIO::readImage (fname, &image, true))
    {
        printf ("Could not read image %s\n", fname);
        exit (1);
    }

    goSignal3D<void> *f = new goSignal3D<void>;
    goAutoPtr<goSignal3DBase<void> > fp (f);

    f->setDataType (GO_FLOAT);
    f->make (image.getSize(), image.getBlockSize(), image.getBorderSize(), 1);
    if (image.getChannelCount() > 1)
    {
        if (!goRGBAtoScalar (&image, f))
            exit (1);
    }
    else
    {
        goCopySignal (&image, f);
    }

    goNormalizeSignal (f);

    goSignal::TVL1 tvl1 (fp, 0.1);

    goDouble lambda = 0.01;

    tvl1.setAutoTimeStep (true);
    tvl1.setLambda (lambda);

    {
        Gtk::Main kit (argc, argv);
        Gtk::Window window;
        // Gtk::ScrolledWindow sw;
        goGUI::ImageView view;
        window.add (view);
        window.show_all ();
        
        //= Make a linear RGB image for display
        goSignal3D<void> rgb, tempf;
        rgb.setDataType (GO_UINT8);
        rgb.make (tvl1.getU()->getSize(), tvl1.getU()->getSize(), goSize3D (4, 4, 0), 3);
        tempf.setDataType (tvl1.getU()->getDataType().getID ());
        tempf.make (f);

        for (int i = 0; i < 1000; ++i)
        {
            //= Copy to 8byte RGB image for display.
            goCopySignal (tvl1.getU(), &tempf);
            tempf *= 255.0f;
            rgb.setChannel (0);
            goCopySignalChannel (&tempf, &rgb);
            rgb.setChannel (1);
            goCopySignalChannel (&tempf, &rgb);
            rgb.setChannel (2);
            goCopySignalChannel (&tempf, &rgb);
            rgb.setChannel (0);

            view.setImage (rgb);
            view.queue_draw ();
            while (Gtk::Main::events_pending())
            {
                Gtk::Main::iteration ();
            }
            tvl1.evolve (0.01);
        }
    }

    exit (1);
}
