#include <gocurvaturediffusion.h>
#include <golist.h>
#include <gopoint.h>
#include <gofixedarray.h>
#include <gotimerobject.h>
#include <gofixedarray.h>
#include <gocurvepartition.h>
#include <stdio.h>

#include <engine.h>
#include <gomatlab.h>

#include <goplot.h>

/* Here lies the rabbit in the pepper. Haha. */
template <class REAL>
static REAL vertexNormalComponent (REAL r, REAL phi, REAL e)
{
    if (phi == 0.0)
    {
        return 0.0;
    }
    REAL mu      = REAL(1.0f);
    REAL abs_phi = fabs(phi);
    //= This sign seems to be flipped in the paper.
    if (abs_phi < M_PI*0.5)
        mu = REAL(-1.0f);
    goDouble tan_phi = tan(phi);
    return r / tan_phi * (1 + mu * sqrt(1 + 4*e*(1-e)*tan_phi*tan_phi));
}

static bool readPointsFile (const char* filename, goList<goPointf>& ret)
{
    FILE* f = fopen(filename,"r");
    if (!f)
    {
        return false;
    }
    char line [255];
    fgets (line, 255, f);
    if (strcmp(line,"curve\n") != 0)
    {
        return false;
    }
    goPointf point;
    while (!feof(f))
    {
        fscanf(f,"%f %f\n",&point.x,&point.y);
        ret.append(point);
    }
    fclose(f);
    return true;
}

int main (int argc, char* argv[])
{
#if 0
    {
        const goIndex_t stepCount = 1000;
        goFloat phi = -M_PI+1e-2;
        goFloat step = 2.0 * M_PI / (float)stepCount;
        goFixedArray<goDouble> L (stepCount);
        goFixedArray<goDouble> phiVector (stepCount);
        for (goIndex_t i = 0; i < stepCount; phi += step, ++i)
        {
            L(i) = vertexNormalComponent (2.0f, phi, 0.5f);
            phiVector(i) = phi;
        }
        goPlot::gnuplot(phiVector,L,"L vs. \\Phi");
    }
    return 1;
#endif
    
     if (argc < 3)
     {
        printf ("Format: %s <curve_filename> <num_iterations>\n",argv[0]);
        return 2;
     }
     const char* filename = argv[1];     
     const int numIterations = atoi(argv[2]);

     goList<goPointf> points;
     if (!readPointsFile (filename,points))
     {
        printf ("Could not read file %s\n",filename);
        return 2;
     }
     points.close();

     const int neighSize = 3; // points.getSize() / 6;
     
     goMatlab matlab;
     matlab.put2DPoints(points, "p");
     matlab.matlabCall("figure(1); plot(p(1,1:end),p(2,1:end)); waitforbuttonpress;");

     {
        goFixedArray<goDouble> curvatureBefore;
        goCurveCurvature (points,curvatureBefore);
        matlab.putArray (curvatureBefore.getPtr(), curvatureBefore.getSize(), "curvBef");
     }

     goFixedArray<goPointf> normalFlow;
     goTimerObject timer;
     timer.startTimer();
     for (goIndex_t j = 0; j < numIterations; ++j)
     {
        goCurvatureDiffusionFlow (points, neighSize, normalFlow);
        assert (normalFlow.getSize() == points.getSize());
        goList<goPointf>::Element* el = points.getFrontElement();
        for (goIndex_t i = 0; i < normalFlow.getSize() && el; ++i, el = el->next)
        {
            el->elem += normalFlow[i] * 0.2;
            // printf ("%f %f    %f\n", normalFlow[i].x, normalFlow[i].y, normalFlow[i].abs());
        }
        //matlab.put2DPoints(points, "p");
        //matlab.matlabCall("plot(p(1,1:end),p(2,1:end)); drawnow;");
        printf ("Iteration %d\n",j);
     }
     timer.stopTimer();
     printf ("Time: %f\n",timer.getTimerSeconds());
     matlab.put2DPoints(points, "p");
     matlab.matlabCall("figure(2); plot(p(1,1:end),p(2,1:end)); drawnow;");

     goFixedArray<goDouble> curvatureAfter;
     goCurveCurvature (points,curvatureAfter);
     matlab.putArray (curvatureAfter.getPtr(), curvatureAfter.getSize(), "curvAft");
     matlab.matlabCall("figure(3); hold on; subplot(2,1,1); plot(curvAft); subplot(2,1,2); plot(curvBef);");
     matlab.matlabCall("save('workspace.mat');");
     matlab.matlabCall("waitforbuttonpress;");
     return 1;
}
