#ifndef GOMATH_H
# include <gomath.h>
#endif
#ifndef GOMATRIX_H
# include <gomatrix.h>
#endif
#ifndef GOVECTOR_H
# include <govector.h>
#endif

/**
 * @addtogroup math
 * @{
 */
/** 
 * @brief Convert euclidean to barycentric coordinates.
 *
 * This works only for vertices with n+1 points in n dimensions.
 * 
 * @param simplex Simplex points, one point per column
 * @param point   Point in euclidean coordinates
 * @param ret     On return, contains barycentric coordinates of point.
 * 
 * @return True if successful, false otherwise (also check log).
 */
template <class T>
bool goMath::euclideanToBarycentric (const goMatrix<T>& simplex, const goVector<T>& point, goVector<T>& ret)
{
    if (simplex.getRows() != simplex.getColumns() - 1)
    {
        goLog::warning ("goMath::euclideanToBarycentric() works only for n+1 points in n dimensions.");
        return false;
    }
    if (point.getSize() != simplex.getRows())
    {
        goLog::warning ("goMath::euclideanToBarycentric(): point dimensions mismatch.");
        return false;
    }
    goMatrix<T> simplex_ (simplex.getRows() + 1, simplex.getColumns());
    simplex.copy (0, 0, simplex.getRows()-1, simplex.getColumns()-1, simplex_);
    goVector<T> ref;
    simplex_.refRow (simplex_.getRows()-1, ref);
    ref.fill (T(1));

    goVector<T> point_ (point.getSize() + 1);
    point.copy (point_);
    point_[point_.getSize()-1] = T(1);

    simplex_.invert ();
    
    goMatrixVectorMult<T> (T(1), simplex_, false, point_, T(0), ret);
    return true;
}
/** @} */

template <class T>
void goMath::barycentricToEuclidean (const goMatrix<T>& simplex, const goVector<T>& barycentric, goVector<T>& ret)
{
    goMatrixVectorMult<T> (T(1), simplex, false, barycentric, T(0), ret);
}

template 
bool goMath::euclideanToBarycentric<goFloat> (const goMatrix<goFloat>&, const goVector<goFloat>&, goVector<goFloat>&);
template 
bool goMath::euclideanToBarycentric<goDouble> (const goMatrix<goDouble>&, const goVector<goDouble>&, goVector<goDouble>&);

template 
void goMath::barycentricToEuclidean<goFloat> (const goMatrix<goFloat>&, const goVector<goFloat>&, goVector<goFloat>&);
template 
void goMath::barycentricToEuclidean<goDouble> (const goMatrix<goDouble>&, const goVector<goDouble>&, goVector<goDouble>&);
