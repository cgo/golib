#include <goregionls.h>
#include <gosignalhelper.h>
#include <gofileio.h>
#include <gotimerobject.h>

#define HAVE_MATLAB
#include <engine.h>
#include <gomatlab.h>

static goDouble heaviside (goDouble x, goDouble epsilon)
{
    static goDouble co = 2.0 / (goDouble)M_PI;
    return 0.5*(1.0+co*atan(x/epsilon));
}

static goDouble dirac (goDouble x, goDouble epsilon)
{
    return 0.3183098861837907/(epsilon*(1 + x*x/(epsilon*epsilon)));
}

int main (int argc, char* argv[])
{
//   for (double x=-20.0; x < 20.0; x+=40.0/500.0)
//   {
//        printf ("%f\n",dirac(x,1.0));
//   }
//   exit(1);

    if (argc < 7)
    {
        printf ("Usage: %s <imagefile> <mu> <Delingette> <Li> <delta_t> <time to run> <explicit|implicit>\n", argv[0]);
        return 0;
    }

    goString       filename   = argv[1];
    const goDouble mu         = atof(argv[2]);
    const goDouble delingette = atof(argv[3]);
    const goDouble li         = atof(argv[4]);
    const goDouble deltaT     = atof(argv[5]);
    const goDouble stopTime   = atof(argv[6]);
    const int      numSteps   = static_cast<int>(stopTime/deltaT);
    printf ("Steps to go: %d\n",numSteps);
    bool implicitSolve = false;
    if (argc > 7)
    {
        goString temp = argv[7];
        if (temp == goString("implicit"))
        {
            implicitSolve = true;
        }
        else
        {
            printf ("5th argument was given but does not spell 'implicit'. Check argument or see usage by calling %s without arguments.\n",argv[0]);
            return 2;
        }
    }
    
    printf ("Loading %s\n", filename.toCharPtr());
    goSignal3D<void> temp;
    goFileIO::readImage (filename.toCharPtr(), &temp);
    goSignal3D<void> image;
    image.setDataType (GO_DOUBLE);
    image.make (&temp);
    // goConvertSignal (&temp, &image);
    goCopySignal (&temp, &image);
    goNormalizeSignal (&image);

    {
        goString tmpstr;
        filename.getFileName (tmpstr);
        filename=tmpstr;
    }
   

    goRegionLS ls;
    ls.setImage (&image);
    ls.setMu (mu);
    ls.setDelingette (delingette);
    ls.setLi (li);
    ls.setNu (0.0);
    ls.setHx (1.0);
    ls.setHy (1.0);
    ls.setLambda1 (1.0);
    ls.setLambda2 (1.0);
    ls.setEpsilon (1.5);
    goMatlab mat;
    if (!mat.signalToVariable (ls.getPhi(), "phi"))
    {
        printf ("Could not put variable to matlab.\n");
        return 2;
    }

    goSignal3D<void> phi;
    phi.setDataType (GO_UINT8);
    phi.make (ls.getPhi());
    int i;
    goTimerObject timer;
    const int printSteps = numSteps / 10;
    int j = 0;
    timer.startTimer();
    for (i = 0; i < numSteps; ++i)
    {
        // mat.signalToVariable (ls.getPhi(), "phi");
        // mat.matlabCall ("surf(phi); pause;");
        if (implicitSolve)
        {
            ls.evolveImplicitly (deltaT);
        }
        else
        {
            ls.evolve (deltaT);
        }
        //mat.putSignal (ls.getPhi(), "phi");
        //mat.matlabCall ("subplot(2,1,1); surf(phi); subplot(2,1,2); contour(phi,[0 0]); drawnow;");
        
        if (j >= printSteps)
        {
            printf ("%d\n", i);
            mat.putSignal (ls.getPhi(), "phi");
            mat.matlabCall ("subplot(2,1,1); surf(phi); subplot(2,1,2); contour(phi,[0 0]); drawnow;");
            //= Save picture.
#if 0
            goString name = filename;
            name += (int)i;
            name += ".png";
            goString command = "print -dpng ";
            command += name; command += ";";
            goString s;
            s.resize(1024);
            mat.matlabCall (command.toCharPtr(),&s);
            printf ("%s\n",s.toCharPtr());
#endif
            j = 0;
        }
        ++j;
    }
    timer.stopTimer();
    printf ("Seconds: %f\n", timer.getTimerSeconds());
    if (!mat.signalToVariable (ls.getPhi(), "phi"))
    {
        printf ("Could not put variable to matlab.\n");
        return 2;
    }
    mat.matlabCall ("subplot(2,1,1); surf(phi); subplot(2,1,2); contour(phi,[0 0]); waitforbuttonpress;");
    return 1;
}
