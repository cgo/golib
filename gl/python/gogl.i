/* example.i */
%module gogl

%feature("autodoc","1");
%include "typemaps.i"

%{
#include <gogl/helper.h>
#include <gogl/offfile.h>
#include <goconfig.h>
#include <gotypes.h>
#include <goobjectbase.h>
#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <godwt3d.h>
#include <goarray.h>
#include <gofixedarray.h>
#include <gostring.h>
#include <gofileio.h>
#include <gosignalhelper.h>
#include <goexception.h>
#include <gomatrix.hpp>
#include <gomatrix.h>
#include <govector.h>
#include <goplot.h>
#include <gohistogram.h>
#include <gopointcloud.h>
#include <gocurve.h>
#include <gognuplot.h>
#include <gofilter1d.h>
#include <golist.h>
%}
%import ../../python/golib.i
%include <gogl/helper.h>
%include <gogl/offfile.h>

%extend goGL::OFFFile 
{
    %pythoncode{
        def get_adjacency_lists(self):
            """Python version of the adjacency lists.
               Returns: List of lists, one adjacency list for each vertex. Numbers start at 0."""
            lists = golib.goFixedArraygoListInt()
            self.getAdjacencyLists (lists)
            ret = []
            for i in xrange(len(lists)):
                newlist = []
                # el = lists[i].getFrontElement()
                for j in xrange(lists[i].getSize()):
                    newlist.append (lists[i](j).elem) # el.elem)
                    # el = el.next  # This does not work ... dont know why.
                ret.append (newlist)
            return ret
    %}
}
