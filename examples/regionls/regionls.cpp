/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goregionls.h>
#include <gosignalhelper.h>
#include <gosignal3dgenericiterator.h>
#include <gofileio.h>
#include <gotimerobject.h>

#define HAVE_MATLAB
#include <engine.h>
#include <gomatlab.h>

#include <unistd.h>
#include <getopt.h>

static goDouble heaviside (goDouble x, goDouble epsilon)
{
    static goDouble co = 2.0 / (goDouble)M_PI;
    return 0.5*(1.0+co*atan(x/epsilon));
}

static goDouble dirac (goDouble x, goDouble epsilon)
{
    return 0.3183098861837907/(epsilon*(1 + x*x/(epsilon*epsilon)));
}

void printUsage (char* argv0)
{
    printf ("Usage: %s <options>\n", argv0);
    printf (" Required options are: \n");
    printf ("   --image <filename>\n");
    printf ("   --mu <mu>\n");
    printf ("   --dt <delta_t>\n");
    printf ("   --time <time span>\n");
    printf (" Optional options:\n");
    printf ("   --delingette <Delingette term factor>,\n");
    printf ("     Zero by default. (Only for explicit solver)\n");
    printf ("   --li <Li term factor>, Zero by default.\n");
    printf ("     (Only for explicit solver)\n");
    printf ("   --lambda1 <lambda1 parameter>, 1.0 by default.\n");
    printf ("   --lambda2 <lambda2 parameter>, 1.0 by default.\n");
    printf ("   --nu <nu parameter>, 0.0 by default.\n");
    printf ("   --external <external field factor>, 0.0 by default.\n");
    printf ("   --implicit (use implicit instead of explicit solver).\n");
    printf ("   --save <image base name> (save image every few iterations).\n");
}

bool getCommandLineParameters (int argc, char** argv,
                               goString& filename,
                               goDouble& mu,
                               goDouble& deltaT,
                               goDouble& time,
                               goDouble& lambda1,
                               goDouble& lambda2,
                               goDouble& delingette,
                               goDouble& li,
                               goDouble& nu,
                               goDouble& external,
                               goString& saveBase,
                               bool&     implicitSolve)
{
    bool got_mu         = false;
    bool got_filename   = false;
    bool got_dt         = false;
    bool got_time       = false;
    bool got_delingette = false;
    bool got_li         = false;
    bool got_lambda1    = false;
    bool got_lambda2    = false;
    bool got_nu         = false;
    bool got_external   = false;
    bool got_savebase   = false;
    int  implicit       = 0;
    while (1)
    {
        int c;
        static struct option long_options[] =
        {
            {"implicit",    no_argument, &implicit, 1},
            {"mu",          required_argument, 0, 'a'},
            {"dt",          required_argument, 0, 'b'},
            {"delingette",  required_argument, 0, 'c'},
            {"li",          required_argument, 0, 'd'},
            {"time",        required_argument, 0, 'e'},
            {"image",       required_argument, 0, 'f'},
            {"lambda1",     required_argument, 0, 'g'},
            {"lambda2",     required_argument, 0, 'h'},
            {"nu",          required_argument, 0, 'i'},
            {"external",    required_argument, 0, 'j'},
            {"save",        required_argument, 0, 'k'},
            {0, 0, 0, 0}
        };
        /* `getopt_long' stores the option index here. */
        int option_index = 0;

        c = getopt_long (argc, argv, "a:b:c:d:e:f:",
                long_options, &option_index);

        /* Detect the end of the options. */
        if (c == -1)
            break;

        switch (c)
        {
            case 0:
                /* If this option set a flag, do nothing else now. */
                if (long_options[option_index].flag != 0)
                    break;
                printf ("option %s", long_options[option_index].name);
                if (optarg)
                    printf (" with arg %s", optarg);
                printf ("\n");
                break;

            case 'a': mu         = atof(optarg); got_mu         = true; break;
            case 'b': deltaT     = atof(optarg); got_dt         = true; break;
            case 'c': delingette = atof(optarg); got_delingette = true; break;
            case 'd': li         = atof(optarg); got_li         = true; break;
            case 'e': time       = atof(optarg); got_time       = true; break;
            case 'f': filename   = optarg;       got_filename   = true; break;
            case 'g': lambda1    = atof(optarg); got_lambda1    = true; break;
            case 'h': lambda2    = atof(optarg); got_lambda2    = true; break;
            case 'i': nu         = atof(optarg); got_nu         = true; break;
            case 'j': external   = atof(optarg); got_external   = true; break;
            case 'k': saveBase   = optarg;       got_savebase   = true; break;
            case '?': break;
            default: abort(); break;
        }
    }

    if (implicit == 1)
    {
        implicitSolve = true;
    }
    else
    {
        implicitSolve = false;
    }
    
    if (got_mu && got_dt && got_time && got_filename)
    {
        return true;
    }
    return false;
}

int main (int argc, char* argv[])
{
//   for (double x=-20.0; x < 20.0; x+=40.0/500.0)
//   {
//        printf ("%f\n",dirac(x,1.0));
//   }
//   exit(1);

    goString filename;
    goString saveBase = "";
    goDouble mu;
    goDouble delingette = 0.0;
    goDouble li = 0.0;
    goDouble deltaT;
    goDouble stopTime;
    goDouble lambda1 = 1.0;
    goDouble lambda2 = 1.0;
    goDouble nu = 0.0;
    goDouble external = 0.0;
    bool  implicitSolve       = false;
    if (!getCommandLineParameters (argc, argv,
                               filename,
                               mu,
                               deltaT,
                               stopTime,
                               lambda1,
                               lambda2,
                               delingette,
                               li,
                               nu,
                               external,
                               saveBase,
                               implicitSolve))
    {
        printUsage(argv[0]);
        exit(2);
    }

    const int numSteps = static_cast<int>(stopTime/deltaT);
    printf ("Steps to go: %d\n",numSteps);
    
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
    goDouble hx = 1.0;
    goDouble hy = 1.0;
    ls.setImage (&image,hx,hy);
    ls.setMu (mu);
    ls.setDelingette (delingette);
    ls.setLi (li);
    ls.setNu (nu);
    ls.setLambda1 (lambda1);
    ls.setLambda2 (lambda2);
    ls.setEpsilon (1.5);

    printf ("CFL restriction: dt < %f\n",ls.getCFLRestriction());
    
    goMatlab mat;
    if (!mat.signalToVariable (ls.getPhi(), "phi"))
    {
        printf ("Could not put variable to matlab.\n");
        return 2;
    }

    //= Make an experimental external velocity field:
    goSignal3D<void> externalField;
    if (external != 0.0)
    {
        //= Make 2-channel signal to hold the vector field.
        externalField.setDataType(GO_DOUBLE);
        externalField.make(ls.getPhi()->getSize(),
                ls.getPhi()->getBlockSize(),
                ls.getPhi()->getBorderSize(), 2);
        goSignal3DGenericIterator it(&externalField);
        //= Set all vectors to fixed values.
        goDouble fx = 2.0*M_PI / (goDouble)externalField.getSizeX();
        goDouble fy = 2.0*M_PI / (goDouble)externalField.getSizeY();
        while (!it.endZ())
        {
            it.resetY();
            goDouble y = 0.0;
            while (!it.endY())
            {
                it.resetX();
                goDouble x = 0.0;
                while (!it.endX())
                {
                    *(goDouble*)*it = -cos(y);
                    *((goDouble*)(*it + externalField.getChannelOffset(1))) = sin(x);
                    it.incrementX();
                    x += fx;
                }
                y += fy;
                it.incrementY();
            }
            it.incrementZ();
        }

        //= Some debugging outputs with matlab
#if 0
        //= Calculate grad(phi).
        goSignal3D<void> gradPhi;
        gradPhi.setDataType(GO_DOUBLE);
        gradPhi.setBorderFlags(GO_X|GO_Y, GO_CONSTANT_BORDER);
        //= 2-channel signal for a vector field.
        gradPhi.make(ls.getPhi()->getSize(),ls.getPhi()->getBlockSize(),
                ls.getPhi()->getBorderSize(), 2);
        if (!ls.getNablaPhi (gradPhi))
        {
            printf ("Could not calculate nabla phi!\n");
        }
        goSignal3D<void> result;
        result.setDataType(GO_DOUBLE);
        result.make(ls.getPhi());
        goMath::vectorMult(externalField,gradPhi,result);

        mat.putSignal(&externalField,"Vx");
        externalField.setChannel(1);
        mat.putSignal(&externalField,"Vy");
        externalField.setChannel(0);
        mat.matlabCall("quiver(Vx,Vy); title('Velocity');");
        mat.putSignal(&gradPhi,"gradPhix");
        gradPhi.setChannel(1);
        mat.putSignal(&gradPhi,"gradPhiy");
        gradPhi.setChannel(0);
        mat.matlabCall("figure; quiver(gradPhix,gradPhiy); title('grad phi');");
        mat.putSignal(&result,"VP");
        mat.matlabCall("figure; imagesc(VP); ;title('Velocity X nablaPhi'); colorbar; waitforbuttonpress;");
#endif
    }
    
    const int printSteps = goMath::min<int>(numSteps / 10, 100);
    int j = 0;
    int i = 0;
    goTimerObject timer;
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
            if (external != 0.0)
            {
                ls.evolve (deltaT,&externalField,external);
            }
            else
            {
                ls.evolve (deltaT);
            }
        }
        //mat.putSignal (ls.getPhi(), "phi");
        //mat.matlabCall ("subplot(2,1,1); surf(phi); subplot(2,1,2); contour(phi,[0 0]); drawnow;");
        
        if (j >= printSteps)
        {
            printf ("%d\n", i);
            mat.putSignal (ls.getPhi(), "phi");
            mat.matlabCall ("subplot(2,1,1); surf(phi); subplot(2,1,2); contour(phi,[0 0]); drawnow;");
            //= Save picture.
            if (saveBase != "")
            {
#if 1
                goString name = saveBase;
                char tmp [255];
                sprintf(tmp,"%.15d",i);
                name += tmp;
                name += ".png";
                goString command = "print -dpng ";
                command += name; command += ";";
                goString s;
                s.resize(1024);
                mat.matlabCall (command.toCharPtr(),&s);
                printf ("%s\n",s.toCharPtr());
#endif
            }
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
