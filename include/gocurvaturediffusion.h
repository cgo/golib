#ifndef GOCURVATUREDIFFUSION_H
#define GOCURVATUREDIFFUSION_H

#ifndef GOLIST_H
# include <golist.h>
#endif
#ifndef GOFIXEDARRAY_H
# include <gofixedarray.h>
#endif
#ifndef GOPOINT_H
# include <gopoint.h>
#endif

template <class pointT>
bool goCurvatureDiffusionFlow (goList<pointT>& points, int sigma, goFixedArray<pointT>& f_normal_ret);

template <class pointT>
bool goCurvatureDiffusion (goList<pointT>& points, int sigma);
#endif