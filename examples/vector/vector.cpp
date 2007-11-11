#include <govector.h>
#include <gosimplevector.h>
#include <gotimerobject.h>

int main ()
{
    {
        printf ("%e\n", 1e-5);
        goSimpleVector<goFloat> v (3);
        printf ("Size: %d\n", v.getSize());
        printf ("Vector:\n");
        v[0] = 1.0;
        v[1] = 2.0;
        v[2] = 3.0;
        //v[3] = 4.0;
        //v[4] = 5.0;
        v._print();

        float a = 1.1;
        float b = 0.0;
        goTimerObject timer;
        timer.startTimer();
        goSize_t N = 1000000000;
        for (goSize_t i = 0; i < N; ++i)
        {
            b = a * a;
            // float a = v * v;
        }
        timer.stopTimer();
        printf ("Seconds simple vector: %lf\n", timer.getTimerSeconds());
        printf ("Seconds per operation: %e\n", timer.getTimerSeconds() / (double(N)));
        goVector<goFloat> v2 (200);
        v2.fill (2.0);
        // printf ("v2: \n");
        // v2._print();
        timer.startTimer();
        N = 10000000;
        for (goSize_t i = 0; i < N; ++i)
        {
            float a = v2 * v2;
        }
        timer.stopTimer();
        printf ("Seconds general vector: %lf\n", timer.getTimerSeconds());
        printf ("Seconds per operation: %e\n", timer.getTimerSeconds() / (double(N) * double(v2.getSize()) + double(v2.getSize() - 1)));

        exit (1);
    }

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
