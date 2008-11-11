#include <gosignalhelper.h>
#include <golog.h>
#include <gofilter3d.h>

bool goSignal::sobel2D (const goSignal3DBase<void>& input, goSignal3DBase<void>& output)
{
    const goSize3D& sz = input.getSize ();
    
    static const goFloat mask_data_x[] = {-1.0, 0.0, 1.0, 
                                         -2.0, 0.0, 2.0, 
                                         -1.0, 0.0, 1.0};
    static const goFloat mask_data_y[] = {-1.0, -2.0, -1.0,
                                         0.0, 0.0, 0.0,
                                         1.0, 2.0, 1.0};

    if (output.getSize () != sz || output.getChannelCount () != 2 || output.getDataType().getID () != GO_FLOAT)
    {
        goSignal3D<void>* p = reinterpret_cast <goSignal3D<void>*> (&output);
        if (!p)
        {
            goLog::error ("sobel2D (): output is of wrong size and is not a goSignal3D.");
            return false;
        }
        else
        {
            p->setDataType (GO_FLOAT);
            p->make (sz, input.getBlockSize(), input.getBorderSize(), 2);
        }
    }

    goFilter3D<void,void> filter;
    output.setChannel (0);
    filter.setMask (mask_data_x, 3, 3, 1, false);
    filter.setMaskCenter (1, 1, 0);
    filter.filter (input, output);
    output.setChannel (1);
    filter.setMask (mask_data_y, 3, 3, 1, false);
    filter.setMaskCenter (1, 1, 0);
    filter.filter (input, output);
    output.setChannel (0);

    return true;
}
