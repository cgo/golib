#include <stdio.h>
#include <stdlib.h>
#include <gonurbs.h>
#include <golist.h>
#include <gopoint.h>
#include <gorandom.h>
#include <math.h>

int main ()
{
    goList<goPointf> points;
    goPointf p;
    p.w = 1.0f;
    goIndex_t i;
    FILE* f = fopen ("points.txt","w");
    if (!f)
        exit(-2);
    for (i = 0; i < 10; ++i)
    {
        p.x = i; // cos (i/10.0f * 2 * M_PI);
        p.y = sin (i/10.0f * 2 * M_PI);
        fprintf (f, "%f %f\n", p.x, p.y);
        points.append (p);
    }
    fclose (f);
    goNURBS nurbs;
    nurbs.interpolate (points);
    
    f = fopen ("nurbs.txt","w");
    if (!f)
        exit(-2);
    goDouble max = nurbs.getCurveLength();
    goFloat t = 0.0f;
    goFloat step = max / 100.0f;
    printf ("max == %f\n", max);
    for (i = 0; i < 101; ++i, t += step)
    {
        p = nurbs (t);
        fprintf (f, "%f %f\n", p.x, p.y);
        printf ("t == %f\n", t);
    }
    fclose (f);
    exit(1);
}
