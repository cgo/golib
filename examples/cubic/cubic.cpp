#include <gomath.h>
#include <govector.h>
#include <gomatrix.h>
#include <goplot.h>

int main ()
{
    goVectorf pm1(2), p0(2), p1(2), p2(2);
    pm1[0] = 0; pm1[1] = 0;
    p0[0] = 1; p0[1] = -1;
    p1[0] = 2; p1[1] = 1;
    p2[0] = 3; p2[1] = 0;

    goMath::CubicSpline<goFloat> spline (pm1, p0, p1, p2);

    const goSize_t N = 100;
    goMatrixf M (N, 2);
    goVectorf ref;
    goVectorf t (N);
    t.fillRange(0, 1, N);
    t *= 1.0 / (float)(N-1);
    for (goSize_t i = 0; i < N; ++i)
    {
        M.refRow(i,ref);
        ref = *spline(t[i]);
    }

    goMatrixf points (4,2);
    points.setRow(0,pm1);
    points.setRow(1,p0);
    points.setRow(2,p1);
    points.setRow(3,p2);
    goMultiPlotter mp (1,1);
    goSinglePlot plot;
    plot.addCurveMatrix (points, "points", "with linespoints");
    plot.addCurveMatrix (M, "spline fit");
    mp.addPlot(plot,0);
    mp.setPauseFlag(true);
    mp.plot();

    return 1;
}
