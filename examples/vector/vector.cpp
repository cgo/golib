#include <govector.h>

int main ()
{
    goVector<float> v1 (4);
    v1(0) = 1;
    v1(1) = 2;
    v1(2) = 3;
    v1(3) = 4;
    goVector<double> v2 (4);
    v2(0) = 10;
    v2(1) = 20;
    v2(2) = 30;
    v2(3) = 40;

    goVector<float> v3;
    try
    {
        v3 = v1 - v2 + v1 * 0.5;
    }
    catch (goMathException ex)
    {
        if (ex.code == goMathException::SIZE_MISMATCH)
        {
            printf ("Size mismatch!\n");
            exit(1);
        }
    }

    printf ("%f %f %f %f\n", v3(0), v3(1), v3(2), v3(3));
    exit(1);
}
