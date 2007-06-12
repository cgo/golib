#include <govector.h>
#include <golist.h>

#define HAVE_MATLAB
#include <engine.h>
#include <gomatlab.h>

int main ()
{
    goSize_t i;

    goMatlab matlab;

    for (i = 0; i < 100; ++i)
    {
        goList<goVectorf> vl;
        vl.append (goVectorf(2));
        vl.erase();
        vl.append (goVectorf(2));
        vl.append (goVectorf(2));
        vl.append (goVectorf(2));
        vl.append (goVectorf(2));
        printf ("Putting points...\n");
        goVectord v (3);
        matlab.put2DPoints (vl, "dummy");
        matlab.putVector (&v, "dummy2");
        printf (" ... done.\n");
    }
    exit (1);
}
