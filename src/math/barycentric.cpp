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
bool goMath::euclideanToBarycentric (const goMath::Matrix<T>& simplex, const Vector<T>& point, Vector<T>& ret)
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
    goMath::Matrix<T> simplex_ (simplex.getRows() + 1, simplex.getColumns());
    simplex.copy (0, 0, simplex.getRows()-1, simplex.getColumns()-1, simplex_);
    Vector<T> ref;
    simplex_.refRow (simplex_.getRows()-1, ref);
    ref.fill (T(1));

    Vector<T> point_ (point.getSize() + 1);
    point.copy (point_);
    point_[point_.getSize()-1] = T(1);

    simplex_.invert ();
    
    goMath::matrixVectorMult<T> (T(1), simplex_, false, point_, T(0), ret);
    return true;
}
/** @} */

template <class T>
void goMath::barycentricToEuclidean (const goMath::Matrix<T>& simplex, const Vector<T>& barycentric, Vector<T>& ret)
{
    goMath::matrixVectorMult<T> (T(1), simplex, false, barycentric, T(0), ret);
}

template 
bool goMath::euclideanToBarycentric<goFloat> (const goMath::Matrix<goFloat>&, const Vector<goFloat>&, Vector<goFloat>&);
template 
bool goMath::euclideanToBarycentric<goDouble> (const goMath::Matrix<goDouble>&, const Vector<goDouble>&, Vector<goDouble>&);

template 
void goMath::barycentricToEuclidean<goFloat> (const goMath::Matrix<goFloat>&, const Vector<goFloat>&, Vector<goFloat>&);
template 
void goMath::barycentricToEuclidean<goDouble> (const goMath::Matrix<goDouble>&, const Vector<goDouble>&, Vector<goDouble>&);
