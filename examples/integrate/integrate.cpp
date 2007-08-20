#include <gomath.h>
#include <goplot.h>

int main ()
{
    goVectord sinx (100);
    sinx.fillRange (0,1,100);
    sinx *= 1.0 * M_PI / 99.0;
    goMath::sin (sinx);

    goPlot::plot (sinx, "sinx");

    printf ("Trapez: %lf\n", goMath::integrate(sinx));
    printf ("Simpson: %lf\n", goMath::integrateSimpson(sinx));

    return 1;
}
