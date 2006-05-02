#ifndef GOCURVEPARTITION_H
#define GOCURVEPARTITION_H

#ifndef GOLIST_H
# include <golist.h>
#endif
#ifndef GOFIXEDARRAY_H
# include <gofixedarray.h>
#endif
#ifndef GOVECTOR_H
# include <govector.h>
#endif
#ifndef GOTYPES_H
# include <gotypes.h>
#endif

template <class pointT>
bool goCurvePartition(const goList<pointT>& curvePoints,
                      int                   curvatureSigma,
                      goDouble              curvatureEpsilon,
                      goList<pointT>&       ret);

template <class pointT>
bool goCurvePartitionLabelFiltering(const goList<pointT>& curvePoints,
                                    goIndex_t             filterCount,
                                    goList<pointT>&       ret);

template <class pointT>
bool goLocalMeanCurvature (typename goList<pointT>::ConstElement* curvePointsBegin,
                           goIndex_t             pointCount,
                           bool                  closed,
                           goSize_t              neighSize,
                           goUInt32              filterCount,
                           goFixedArray<goDouble>& meanCurvatureRet);

template <class pointT>
bool goCurvePartitionLocalMeanCurvature(const goList<pointT>& curvePoints,
                                        goSize_t              neighSize,
                                        goUInt32              filterCount,
                                        goList<pointT>&       ret,
                                        goList<goIndex_t>&    indexRet,
                                        goFixedArray<goDouble>* curvatureRet = 0,
                                        goFixedArray<goDouble>* meanCurvatureRet = 0);

template <class pointT>
bool goCurvePartitionDelingette (const goList<pointT>& curvePoints, 
                                 goSize_t neighSize,
                                 goSize_t iterations,
                                 goList<pointT>& ret,
                                 goList<goIndex_t>& indexRet);

template <class pointT>
bool goCurvePartitionTrigger(const goList<pointT>& curvePoints,
                             goDouble              triggerLevelPos,
                             goDouble              triggerLevelNeg,
                             goList<pointT>&       ret);

template <class pointT> 
bool goCurveCurvature (typename goList<pointT>::ConstElement* begin, goIndex_t pointCount, goFixedArray<goDouble>& curvRet, bool assumeConstantBoundary);

template <class pointT>
bool goCurveCurvature (const goList<pointT>&   curvePoints,
                       goFixedArray<goDouble>& curvRet);

template <class pointT>
bool goCurveCurvature (const goList<pointT>&   curvePoints,
                       int                     curvatureSigma,
                       goFixedArray<goDouble>& curvRet);
#endif
