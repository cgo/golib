#include <gosvd.h>
#include <gomatrix.h>

int main ()
{
    goMatrix<goFloat> m (4,3);
    m.fill(0.0f);
    m[0][0] = 1;
    m[0][1] = 0;
    m[0][2] = 0;
    m[1][0] = 0;
    m[1][1] = 2;
    m[1][2] = 0;
    m[2][0] = 0;
    m[2][1] = 0;
    m[2][2] = 3;

    goMath::goSVD<goFloat> svd (m);
    
    goMatrix<goFloat> U (4,3);
    goMatrix<goFloat> V (3,3);
    goMatrix<goFloat> S (3,3);
    goArray<goFloat> s (3);
    
    svd.getU (U);
    svd.getV (V);
    svd.getSingularValues (s);
    svd.getS (S);
    U.print();
    V.print();
    S.print(); 
    goMatrix<goFloat> A (4,3);
    V.transpose();
    A = U*S*V;    
    A.print();
    exit (1);
}
