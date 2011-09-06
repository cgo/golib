/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GO_MATH_H
# include <gomath.h>
#endif

/** 
 * @brief Pairwise distances of row or column vectors of X.
 * 
 * This function calculates the euclidean distance between column or row vectors of a matrix.
 * goMath::pdist() can be used as a synonym.
 *
 * @param X         Input matrix X.
 * @param dimension 0: Use row vectors, 1: Use column vectors.
 * @param ret       On return, contains the symmetric distance matrix.
 *                  Note that this matrix could be stored more efficiently, since it
 *                  is symmetric.
 * 
 * @return True if successful, false otherwise.
 * @author Christian Gosch
 */
template <class T>
bool goMath::pairwiseDistances (const goMath::Matrix<T>& X, int dimension, goMath::Matrix<T>& ret)
{
    goMath::Vector<T> ref1, ref2;
    if (dimension == 0)  // rows contain points
    {
        goSize_t N = X.getRows();
        if (ret.getColumns() != N || ret.getRows() != N)
        {
            ret.resize (N,N);
        }
        T temp = T(0);
        for (goSize_t i = 0; i < N - 1; ++i)
        {
            X.refRow (i, ref1);
            ret (i,i) = T(0);
            for (goSize_t j = i + 1; j < N; ++j)
            {
                X.refRow (j, ref2);
                temp = (ref1 - ref2).norm2();
                ret(i,j) = temp;
                ret(j,i) = temp;
            }
        }
    }
    else
    { // columns contain points
        goSize_t N = X.getColumns();
        if (ret.getColumns() != N || ret.getRows() != N)
        {
            ret.resize (N,N);
        }
        T temp = T(0);
        for (goSize_t i = 0; i < N - 1; ++i)
        {
            X.refColumn (i, ref1);
            ret (i,i) = T(0);
            for (goSize_t j = i + 1; j < N; ++j)
            {
                X.refColumn (j, ref2);
                temp = (ref1 - ref2).norm2();
                ret(i,j) = temp;
                ret(j,i) = temp;
            }
        }
    }

    return true;
}


template bool goMath::pairwiseDistances <goFloat> (const goMath::Matrix<goFloat>&, int, goMath::Matrix<goFloat>&);
template bool goMath::pairwiseDistances <goDouble> (const goMath::Matrix<goDouble>&, int, goMath::Matrix<goDouble>&);
