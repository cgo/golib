#ifndef GOPLOT_H
#define GOPLOT_H
#include <goarray.h>
#include <gofixedarray.h>
#include <gostring.h>
#ifndef GOLIST_H
# include <golist.h>
#endif
#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOPOINT_H
# include <gopoint.h>
#endif
#ifndef GOVECTOR_H
# include <govector.h>
#endif

class goPlotterPrivate;

class goSinglePlotPrivate;

//= Internal use.
class goPlotterLabel
{
    public:
        goPlotterLabel (const char* l, goDouble x_, goDouble y_) 
            : label(l), x(x_), y(y_) {};
        goPlotterLabel ()
            : label(""), x(0.0), y(0.0) {};
        goPlotterLabel (const goPlotterLabel& other)
        {
            *this = other;
        };
        virtual ~goPlotterLabel () {};

        bool operator== (const goPlotterLabel& other) const
        {
            return label == other.label && x == other.x && y == other.y;
        };
        bool operator!= (const goPlotterLabel& other) const
        {
            return !this->operator== (other);
        };

        goString label;
        goDouble x;
        goDouble y;
};

/** 
 * @brief Single plot class used for goMultiPlotter.
 */
class goSinglePlot : public goObjectBase
{
    public:
        goSinglePlot ();
        virtual ~goSinglePlot ();
        goSinglePlot (const goSinglePlot&);
        const goSinglePlot& operator= (const goSinglePlot&);

        bool operator== (const goSinglePlot&) const;    //= Just for the goList to work.
        bool operator!= (const goSinglePlot&) const;

        void setPosition (goSize_t row, goSize_t col);
        goSize_t getRow () const;
        goSize_t getColumn () const;

        template <class pointT>
            bool addCurve (const goList<pointT>& points, const char* title, const char* plotOptions = 0)
            {
                typename goList<pointT>::ConstElement* el = points.getFrontElement();
                goSize_t sz = points.getSize();
                goSize_t szVec = sz;
                if (points.isClosed())
                {
                    ++szVec;
                }
                goVectord x(szVec);
                goVectord y(szVec);
                goSize_t i = 0;
                while (i < sz && el)
                {
                    x[i] = el->elem[0];
                    y[i] = el->elem[1];
                    ++i;
                    el = el->next;
                }
                if (points.isClosed())
                {
                    x[szVec-1] = x[0];
                    y[szVec-1] = y[0];
                }
                return this->addCurve (x,y,title,plotOptions);
            };

        bool addCurve (const goVectord& x, const goVectord& y, const char* title, const char* plotOptions = 0);

        template <class vectorT>
            bool addCurve (const vectorT& v, const char* title, const char* plotOptions = 0)
            {
                goSize_t sz = v.getSize();
                goVectord x (sz);
                goVectord y (sz);
                goSize_t i = 0;
                for (i = 0; i < sz; ++i)
                {
                    x[i] = i;
                    y[i] = v[i];
                }
                return this->addCurve (x,y,title,plotOptions);
            };

        bool addLabel    (const goString& l, goDouble x, goDouble y);
        bool makePlot    (goString& plotCommandsRet) const;
        void removeFiles () const;
        void clear       ();

    private:
        goSinglePlotPrivate* myPrivate;
};

class goMultiPlotterPrivate;

/** 
 * @brief Plotting multiple plots in one window (or on one page).
 *
 * This is the plotting facility that should be used generally.
 * It uses Gnuplot.
 *
 * @see goSinglePlot
 * @see goPlotter
 * 
 * @author Christian Gosch
 */
class goMultiPlotter : public goObjectBase
{
    public:
        goMultiPlotter (goSize_t rows, goSize_t cols);
        virtual ~goMultiPlotter ();
        goMultiPlotter (goMultiPlotter&);
        goMultiPlotter & operator= (goMultiPlotter&);

        void addPlot (const goSinglePlot&, goSize_t row, goSize_t col);

        goSize_t getRows () const;
        goSize_t getColumns () const;

        void setWaitFlag (bool w);
        bool getWaitFlag () const;
        void setPauseFlag (bool p);
        bool getPauseFlag () const;
       
        void setPrefix (const goString&);

        virtual bool plot ();
        virtual bool plotPostscript (const goString& filename);
        virtual bool plotFile (const goString& filename, const goString& type);

        void clear ();

    private:
        goMultiPlotterPrivate* myPrivate;
};

class goPlotter : public goObjectBase
{
    public:
        goPlotter ();
        virtual ~goPlotter ();
        goPlotter (const goPlotter&);
        const goPlotter& operator= (const goPlotter&);

        template <class pointT>
            bool addCurve (const goList<pointT>& points, const char* title, const char* plotOptions = 0)
            {
                typename goList<pointT>::ConstElement* el = points.getFrontElement();
                goSize_t sz = points.getSize();
                goSize_t szVec = sz;
                if (points.isClosed())
                {
                    ++szVec;
                }
                goVectord x(szVec);
                goVectord y(szVec);
                goSize_t i = 0;
                while (i < sz && el)
                {
                    x[i] = el->elem[0];
                    y[i] = el->elem[1];
                    ++i;
                    el = el->next;
                }
                if (points.isClosed())
                {
                    x[szVec-1] = x[0];
                    y[szVec-1] = y[0];
                }
                return this->addCurve (x,y,title,plotOptions);
            };

        bool addCurve (const goVectord& x, const goVectord& y, const char* title, const char* plotOptions = 0);
        //bool addCurve (const goVectorf& x, const goVectorf& y, const char* title, const char* plotOptions = 0);

        template <class vectorT>
            bool addCurve (const vectorT& v, const char* title, const char* plotOptions = 0)
            {
                goSize_t sz = v.getSize();
                goVectord x (sz);
                goVectord y (sz);
                goSize_t i = 0;
                for (i = 0; i < sz; ++i)
                {
                    x[i] = i;
                    y[i] = v[i];
                }
                return this->addCurve (x,y,title,plotOptions);
            };

        bool addLabel (const goString& l, goDouble x, goDouble y);

        void setWaitFlag (bool w);
        bool getWaitFlag () const;
        void setPauseFlag (bool p);
        bool getPauseFlag () const;
        
        virtual bool plot ();
        virtual bool plotPostscript (const goString& filename);
        virtual bool plotFile (const goString& filename, const goString& type);
        
    private:
        goPlotterPrivate* myPrivate;
};

namespace goPlot
{
    /** 
     * @brief Plots 1D data using gnuplot.
     *
     * Before calling the plot command, <prefixCommands> are executed
     * in gnuplot and then plot <filename> <gnuplotCommands>.
     * If a shell postfix is given, gnuplot is called in
     * a shell like
     * gnuplot <shellPostfix>. You can e.g. use this
     * to redirect output for postscript terminal type.
     * The calling process can either wait for gnuplot to finish
     * or call it the non-blocking way; this is selected with
     * the waitFor parameter.<br>
     * This function is currently implemented for
     * goArray and goFixedArray template classes with
     * goFloat and goDouble template parameters.
     * 
     * @param array Array, currently goArray<> and goFixedArray<>
     *              of types goFloat and goDouble are supported.
     * @param title Title of the plot.
     * @param gnuplotCommands  Commands that come with the plot command,
     *                         like this:
     *                         plot <filename> <gnuplotCommands>. 
     *                         You could for example put "with dots" 
     *                         or similar here. If left null,
     *                         "with lines" is assumed.
     * @param prefixCommands   Gnuplot commands that go before
     *                         the plot command. You can for example
     *                         put commands to select the terminal type
     *                         here.
     * @param shellPostfix     If this is not null,
     *                         gnuplot is called in a shell (/bin/sh)
     *                         and this string is put after the gnuplot
     *                         command string. Use this e.g.
     *                         for "> my_file.ps" if you use
     *                         postscript terminal.
     * @param cmdFileNameRet   If not null, the command file name
     *                         used by gnuplot (a temporary file)
     *                         is stored here after the call.
     * @param dataFileNameRet  If not null, the data file name
     *                         used by gnuplot (a temporary file)
     *                         is stored here after the call.
     * @param waitfor          If true, the calling process blocks
     *                         until gnuplot has finished.
     *                         If false, the calling process does not 
     *                         block.
     * @return                 True if successful, false otherwise.
     */
    template <class arrayT>
        bool gnuplot (const arrayT& array, 
                      const char* title = 0, 
                      const char* gnuplotCommands = 0,
                      const char* prefixCommands = 0,
                      const char* shellPostfix = 0,
                      goString* cmdFileNameRet = 0, 
                      goString* dataFileNameRet = 0, 
                      bool waitfor = false);

    template <class arrayT, class arrayT2>
        bool gnuplot (const arrayT& array,
                      const arrayT2& array2,
                      const char* title = 0, 
                      const char* gnuplotCommands = 0,
                      const char* prefixCommands = 0,
                      const char* shellPostfix = 0,
                      goString* cmdFileNameRet = 0, 
                      goString* dataFileNameRet = 0, 
                      bool waitfor = false);

    template <class arrayT>
        bool gnuplotList (const goList<arrayT>* arrayListX,
                          const goList<arrayT>* arrayListY = 0,
                          goList<goString>*     title = 0, 
                          goList<goString>* plotCommands = 0,
                          const char*       prefixCommands = 0,
                          const char*       postfixCommands = 0,
                          const char*       shellPostfix = 0,
                          goString*         cmdFileNameRet = 0, 
                          goList<goString>* dataFileNameRet = 0, 
                          bool waitfor = true);

    bool callGnuplot (const goString& gnuplotCommands, 
                      const char*     shellPostfix, 
                      bool            waitfor, 
                      goString*       cmdFilenameRet);

    bool addGnuplotCommands (goString& gnuplotCommands,
                             const goList<goString>* dataFileNames, 
                             const goList<goString>* titles, 
                             const goList<goString>* plotCommands, 
                             const char* prefixCommands,
                             const char* postfixCommands);
    template <class arrayT>
        bool writeGnuplotDataFiles (const goList<arrayT>* arrayListX, 
                                    const goList<arrayT>* arrayListY, 
                                    goList<goString>& dataFileNameRet);
#if 0
        static bool gnuplot (const goArray<goFloat>&, const char* title = 0, const char* gnuplotCommands = 0, goString* cmdFileNameRet = 0, goString* dataFileNameRet = 0, bool waitfor = false);
        static bool gnuplot (const goArray<goDouble>&, const char* title = 0, const char* gnuplotCommands = 0, goString* cmdFileNameRet = 0, goString* dataFileNameRet = 0, bool waitfor = false);
        static bool gnuplot (const goFixedArray<goFloat>&, const char* title = 0, const char* gnuplotCommands = 0, goString* cmdFileNameRet = 0, goString* dataFileNameRet = 0, bool waitfor = false);
        static bool gnuplot (const goFixedArray<goDouble>&, const char* title = 0, const char* gnuplotCommands = 0, goString* cmdFileNameRet = 0, goString* dataFileNameRet = 0, bool waitfor = false);
#endif
}

#endif
