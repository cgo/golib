#include <gomatrix.h>
#include <gomatrix.i>

int main ()
{
    goMatrix<goFloat> m (3,3);
    int i;
    for (i = 0; i < 3; ++i)
    {
        int j;
        for (j = 0; j < 3; ++j)
            m[i][j] = i;
    }
    m.print();
    goMatrix<goFloat> m2 (3,10);
    for (i = 0; i < m2.getRows(); ++i)
    {
        int j;
        for (j = 0; j < m2.getColumns(); ++j)
            m2[i][j] = j;
    }
    m2.print ();
    m2.transpose ();
    m2.print ();
    goMatrix<goFloat> m2_2 (m2);
    m2_2.transpose ();
    
    goMatrix<goFloat> m3;
    m3 = m2_2 * m2;
    m3.print(); 
    for (i = 0; i < 3; ++i)
    {
        m3[i].print ();
    }
    return 1;
}
