#include <gomatrix.h>
#include <gomatrix.hpp>
#include <gopoint.h>
#include <go44matrix.h>
#include <gocomplex.h>
#include <goeigenvalue.h>
#include <govector.h>
#include <gofixedarray.h>
#include <gotimerobject.h>
#undef HAVE_MATLAB
#include <gosparsematrix.h>

#define HAVE_MATLAB
#include <engine.h>
#include <gomatlab.h>

int main ()
{
    {
        goList<goMatrixf> ml;
        ml.append (goMatrixf (100,2));
        ml.append (goMatrixf (100,2));
        ml.append (goMatrixf (100,2));
        ml.append (goMatrixf (100,2));

        goMatrix<goComplexf> x1 (1,100);
        goMatrix<goComplexf> x2 (100,1);
        goMatrix<goComplexf> M (100,100);
        goList<goMatrixf>::Element * el = ml.getFrontElement();
        while (el)
        {
            M += x2 * x1;
            el = el->next;
        }
        exit(1);
    }
    {
        goMatlab mat;
        goString buffer;
        // mat.matlabCall ("M1 = rand(10,10); M2 = rand(10,10);",0);
        // printf ("%s\n", buffer.toCharPtr());
        // mat.matlabCall ("M3 = M1 * M2;",0);
        // printf ("%s\n", buffer.toCharPtr());
        goMatrixd M1,M2,M3;
        M1.setIdentity();
        M2.setIdentity();
        M3.setIdentity();
        mat.getMatrix (M1,"M1");
        mat.getMatrix (M2,"M2");
        mat.getMatrix (M3,"M3");
        M3 = M1 * M2;
        M3.print();
        exit(1);
    }

    {
        printf ("\nSparse timing\n");
        goSparseMatrix m(1000,1000);

        goIndex_t i,j;
        goTimerObject timer;
        timer.startTimer();
        m.fillBegin (1000*1000);
        for (i = 0; i < 1000; ++i)
        {
            for (j = 0; j < 1000; ++j)
            {
                m.fillNext (i,j,i+j);
            }
        }
        m.fillEnd();
        timer.stopTimer();
        printf ("Seconds for 1000x1000 row-first fill: %.5f\n", timer.getTimerSeconds());
        timer.startTimer();
        m.fillBegin (1000*1000);
        for (i = 0; i < 1000; ++i)
        {
            for (j = 0; j < 1000; ++j)
            {
                m.fillNext (j,i,i+j);
            }
        }
        m.fillEnd();
        timer.stopTimer();
        printf ("Seconds for 1000x1000 col-first fill: %.5f\n", timer.getTimerSeconds());
        // return 1;
    }
    {
        goVector< goComplex<float> >* test = new goVector< goComplex<float> > [3];
        goMatrix<goComplexf> m (3,3);
        m(0,0) = goComplexf (1,1);
        m(1,1) = goComplexf (3,2);
        m(2,2) = goComplexf (3,5);
        m(2,1) = goComplexf (2,1);
        m(1,2) = goComplexf (2,-1);
        m.print();

        goVector<goComplexf> v1 (3);
        goVector<goComplexf> v2 (3);
        v1[0] = goComplexf (1,1);
        v1[1] = goComplexf (2,1);
        v1[2] = goComplexf (3,1);
        v2[0] = v1[0].conj();
        v2[1] = v1[1].conj();
        v2[2] = v1[2].conj();
        goMatrix<goComplexf> m2;
        v1.outerProduct (v2, m2);
        m2.print(); 
        goVectorf v;
        goFixedArray< goVector<goComplexf> > w;
        // w.setSize (3);
        //w(0).setSize(3);
        //w(1).setSize(3);
        //w(2).setSize(3);
        goMath::goComplexEigenvaluesHermite (m2, v, &w);
        for (goIndex_t i = 0; i < w.getSize(); ++i)
        {
            std::cout << "Eigenvalue: " << v[i] << ": ";
            std::cout << "Vector " << i << ": ";
            for (goIndex_t j = 0; j < w[i].getSize(); ++j)
            {
                std::cout << "(" << w[i][j] << ") ";
            }
            std::cout << "\n";
        }
    }

    //= Sparse matrix
    {
        printf ("\nSparse multiplication\n");
        goSparseMatrix m1 (50, 5000);
        goSparseMatrix m2 (5000, 50);

        m1.fillBegin (4);
        m2.fillBegin (5);
        m1.fillNext (25, 24, 1);
        m1.fillNext (25, 13, 10);
        m2.fillNext (24, 25, 2);
        m1.fillNext (11, 16, 4);
        m2.fillNext (16, 11, 3);
        m1.fillNext (38, 6,  5);
        m2.fillNext (6, 38,  1.5);
        m2.fillNext (5,5, 9);
        m2.fillNext (100, 20, 8);
        m1.fillEnd();
        m2.fillEnd();

        goSparseMatrix m3;
        m1.matrixMatrixMult (m3, m2);
        goIndex_t i;
        for (i = 0; i < m3.getElementCount(); ++i)
        {
            printf ("(%d,%d) == %f\n", m3.row(i), m3.column(i), m3.value(i));
        }
        printf ("\n");
        m2 = m1;
        m1.transpose();
        m3 = m1 * m2;
        // m1.matrixMatrixMult (m3, m2);
        for (i = 0; i < m3.getElementCount(); ++i)
        {
            printf ("(%d,%d) == %f\n", m3.row(i), m3.column(i), m3.value(i));
        }
        printf ("\n");
        // return 1;
    }
    {
        printf ("\nSparse addition\n");
        goSparseMatrix m1 (53, 50);
        goSparseMatrix m2 (53, 50);

        m1.fillBegin (3);
        m2.fillBegin (3);
        m1.fillNext (25, 24, 1);
        m2.fillNext (25, 24, 2);
        m1.fillNext (11, 16, 4);
        m2.fillNext (17, 11, 3);
        m1.fillNext (38, 6,  5);
        m2.fillNext (4, 12,  1.5);
        m1.fillEnd();
        m2.fillEnd();

        goSparseMatrix m3;
        m1.matrixMatrixAdd (m3, m2);
        //m3 = m1 + m2;
        goIndex_t i;
        for (i = 0; i < m3.getElementCount(); ++i)
        {
            printf ("(%d,%d) == %f\n", m3.row(i), m3.column(i), m3.value(i));
        }
        printf ("Sparse subtraction: \n");
        m1.matrixMatrixSubtract (m3, m2);
        //m3 = m1 - m2;
        for (i = 0; i < m3.getElementCount(); ++i)
        {
            printf ("(%d,%d) == %f\n", m3.row(i), m3.column(i), m3.value(i));
        }
        printf ("\n");
        // return 1;
    }
    {
        printf ("\nSparse vector multiplication\n");
        goSparseMatrix m1 (5, 50);

        m1.fillBegin (6);
        m1.fillNext (1, 24, 1);
        m1.fillNext (1, 16, 4);
        m1.fillNext (1, 6,  5);
        m1.fillNext (2, 6,  1);
        m1.fillNext (2, 25, 2);
        m1.fillNext (2, 30, 3);
        m1.fillEnd();

        goVectord v(50);
        v.fill(0.0);
        v(24) = 2;
        v(16) = 3;
        v(6) = 4;
        v(25) = 5;
        v(1) = 3;

        goVectord v2 = v;
        
        v = m1 * v2;

        for (goIndex_t i = 0; i < v.getSize(); ++i)
        {
            printf ("%f ", v[i]);
        }
        printf ("\n");
        return 1;
    }
    
    goMatrix<goFloat> m (3,3);
    int i;
    for (i = 0; i < 3; ++i)
    {
        int j;
        for (j = 0; j < 3; ++j)
            m(i,j) = i;
    }
    m.print();
    goMatrix<goFloat> m2 (3,10);
    for (i = 0; i < m2.getRows(); ++i)
    {
        int j;
        for (j = 0; j < m2.getColumns(); ++j)
            m2(i,j) = j;
    }
    m2.print ();
    m2.transpose ();
    m2.print ();
    goMatrix<goFloat> m2_2 (m2);
    m2_2.transpose ();
    
    goMatrix<goFloat> m3;
    m3 = m2_2 * m2;
    m3.print(); 

//    goPointf p (1.0f, 1.0f, 1.0f, 1.0f);
//    go44Matrix<goFloat> m4 (2, 0, 0, 0,
//                            0, 2, 0, 0,
//                            0, 0, 2, 0,
//                            0, 0, 0, 0);
//    p *= m4;
//    std::cout << p << "\n";

    
    return 1;
}

#include <golist.hpp>
template class goList<goMatrixf>;
