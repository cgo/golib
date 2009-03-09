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
#ifndef GOGNUPLOT_H
# include <gognuplot.h>
#endif

#include <goplot/plot.h>

class goPlotterPrivate;

class goSinglePlotPrivate;

//= Internal use.
class goPlotterLabel
{
    public:
        goPlotterLabel (const char* l, goDouble x_, goDouble y_, goDouble z_ = 0.0, const char* colourspec_ = "", const char* options_ = "") 
            : label(l), x(x_), y(y_), z(z_), colourspec(colourspec_), options(options_) {};
        goPlotterLabel ()
            : label(""), x(0.0), y(0.0), z(0.0), colourspec(""), options("") {};
        goPlotterLabel (const goPlotterLabel& other)
        {
            *this = other;
        };
        virtual ~goPlotterLabel () {};

        bool operator== (const goPlotterLabel& other) const
        {
            return label == other.label && x == other.x && y == other.y && z == other.z;
        };
        bool operator!= (const goPlotterLabel& other) const
        {
            return !this->operator== (other);
        };

        goString label;
        goDouble x;
        goDouble y;
        goDouble z;
        goString colourspec;
        goString options;
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

        bool add3D (const goMath::Vectord& x, const goMath::Vectord& y, goIndex_t lineLength, 
                    const goMath::Vectord& values, const char* title, const char* plotOptions = 0);
        bool add3D (const goMath::Vectorf& x, const goMath::Vectorf& y, goIndex_t lineLength, 
                    const goMath::Vectorf& values, const char* title, const char* plotOptions = 0);
        bool add3D (const goMath::Matrixf& m, const char* title, const char* plotOptions = 0, bool separateRows = false);
        bool add3D (const goMath::Matrixd& m, const char* title, const char* plotOptions = 0, bool separateRows = false);
        bool add3D (const goSignal3DBase<void>* image, const char* title, const char* plotOptions = 0);

        bool addImage (const goMath::Matrixf& m, const char* title, const char* plotOptions = 0);
        bool addImage (const goMath::Matrixd& m, const char* title, const char* plotOptions = 0);
        bool addImage (const goSignal3DBase<void>& m, const char* title, const char* plotOptions = 0);

        bool add (const char* commands, const char* dataString = 0);
        bool add3D (const char* commands, const char* dataString = 0);
        bool addSphere (const char* plotOptions = 0, goFloat radius = 1.0f, bool autoRange = true);

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
                goMath::Vectord x(szVec);
                goMath::Vectord y(szVec);
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

        bool addCurve (const goMath::Vectord& x, const goMath::Vectord& y, const char* title, const char* plotOptions = 0);
        bool addCurve (const goMath::Vectorf& x, const goMath::Vectorf& y, const char* title, const char* plotOptions = 0);
        bool addCurveMatrix (const goMath::Matrixf& m, const char* title, const char* plotOptions = 0);
        bool addCurveMatrix (const goMath::Matrixd& m, const char* title, const char* plotOptions = 0);

        bool addPoint (const goMath::Vectorf& p, const char* title, const char* plotOptions = 0);
        bool addPoint (const goMath::Vectord& p, const char* title, const char* plotOptions = 0);

        bool addPlane (const goMath::Vectorf& n, const goMath::Vectorf& p, goDouble dx = 0.1, goDouble dy = 0.1, goDouble sx = 1.0, goDouble sy = 1.0, const char* title = "", const char* plotOptions = 0);
        bool addPlane (const goMath::Vectord& n, const goMath::Vectord& p, goDouble dx = 0.1, goDouble dy = 0.1, goDouble sx = 1.0, goDouble sy = 1.0, const char* title = "", const char* plotOptions = 0);
        bool addLine (const goMath::Vectorf& n, const goMath::Vectorf& p, const char* title = "", const char* plotOptions = 0);
        bool addLine (const goMath::Vectord& n, const goMath::Vectord& p, const char* title = "", const char* plotOptions = 0);

        template <class vectorT>
            bool addCurve (const vectorT& v, const char* title, const char* plotOptions = 0)
            {
                goSize_t sz = v.getSize();
                goMath::Vectord x (sz);
                goMath::Vectord y (sz);
                goSize_t i = 0;
                for (i = 0; i < sz; ++i)
                {
                    x[i] = i;
                    y[i] = v[i];
                }
                return this->addCurve (x,y,title,plotOptions);
            };

        void setPrefix   (const goString& p);
        void setPrefix   (const char* p);
        const goString&  getPrefix () const;
        void setTitle    (const goString& s);
        void setTitle    (const char* s);
        const goString& getTitle () const;
        bool addLabel    (const goString& l, goDouble x, goDouble y, const char* colourspec = "", const char* options = "");
        bool addLabel    (const char* l, goDouble x, goDouble y, const char* colourspec = "", const char* options = "");
        bool addLabel    (const goString& l, goDouble x, goDouble y, goDouble z, const char* colourspec = "", const char* options = "");
        bool addLabel    (const char* l, goDouble x, goDouble y, goDouble z, const char* colourspec = "", const char* options = "");
        bool writeGnuplotDataFiles () const;
        bool makePlot    (goString& plotCommandsRet, bool useDataFiles = false) const;
        bool addGnuplotCommands (goString& commandsRet) const;
        void removeFiles () const;
        void clear       ();
        void setUseFiles (bool f);
        bool getUseFiles () const;

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

        void setInputFD (int fd);
        void setOutputFD (int fd);

        goMultiPlotter & operator= (goMultiPlotter&);

        void addPlot (const goSinglePlot&, goSize_t row, goSize_t col, goFloat extentRow = 1.0f, goFloat extentCol = 1.0f);
        void addPlot (const goSinglePlot&, goSize_t index);
        void addPlotBarycentric (goSinglePlot& p, goDouble u, goDouble v, goDouble w, goDouble xsize, goDouble ysize);

        goSize_t getRows () const;
        goSize_t getColumns () const;

        void setWaitFlag (bool w);
        bool getWaitFlag () const;
        void setPauseFlag (bool p);
        bool getPauseFlag () const;
        void setSingleFlag (bool s);
        bool getSingleFlag () const;
        void setAutoPosition (bool a);
        bool getAutoPosition () const;

        void setBarycentric (const goMath::Matrixf& triangle);
        void setBarycentric (const goMath::Matrixd& triangle);
       
        void setPrefix (const goString&);
        void setPrefix (const char*);
        void setPostfix (const goString&);
        void setPostfix (const char*);
        void setTitle (const char*);

        bool makePlotCommands (goString& plotCommandsRet);

        virtual bool plot           (goGnuplot *gp = 0);
        virtual bool plotPostscript (const goString& filename, goFloat sizeX = -1.0f, goFloat sizeY = -1.0f);
        virtual bool plotEPS        (const goString& filename);
        virtual bool plotFile       (const goString& filename, const goString& type);
        bool plotPostscript (const char* filename, goFloat sizeX = -1.0f, goFloat sizeY = -1.0f);
        bool plotEPS        (const char* filename);
        bool plotFile       (const char* filename, const char* type);

        bool saveGnuplot (const char* filename);

        void clear ();
        void removeFiles ();

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
                goMath::Vectord x(szVec);
                goMath::Vectord y(szVec);
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

        bool addCurve (const goMath::Vectord& x, const goMath::Vectord& y, const char* title, const char* plotOptions = 0);
        //bool addCurve (const goMath::Vectorf& x, const goMath::Vectorf& y, const char* title, const char* plotOptions = 0);

        template <class vectorT>
            bool addCurve (const vectorT& v, const char* title, const char* plotOptions = 0)
            {
                goSize_t sz = v.getSize();
                goMath::Vectord x (sz);
                goMath::Vectord y (sz);
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

    typedef enum
    {
        Normal,
        Surface
    } PlotType;

    template <class T>
        void plot (const goMath::Vector<T>& x, const goMath::Vector<T>& y, 
                const char* title = 0, const char* plotOptions = 0, const char* prefix = 0);
    template <class T>
        void plot (const goMath::Vector<T>& y, 
                const char* title = 0, const char* plotOptions = 0, const char* prefix = 0);
    template <class T>
        void plot (const goMath::Matrix<T>& points,
                const char* title = 0, const char* plotOptions = 0, const char* prefix = 0);
    template <class T>
        void plot3D (const goMath::Matrix<T>& M,
                const char* title, const char* plotOptions = 0, const char* prefix = 0, bool separateRows = false);

    class PlotPrivate;

    class Plot : public goObjectBase
    {
        public:
            Plot ();
            virtual ~Plot ();

            void plot (const goMath::Matrixf& curve, const char* title = "", const char* options = "w l", goSize_t x = 0, goSize_t y = 0);
            void plot (const goMath::Matrixd& curve, const char* title = "", const char* options = "w l", goSize_t x = 0, goSize_t y = 0);
            void plot (const goMath::Vectorf& px, const goMath::Vectorf& py, const char* title = "", const char* options = "w l", goSize_t x = 0, goSize_t y = 0);
            void plot (const goMath::Vectord& px, const goMath::Vectord& py, const char* title = "", const char* options = "w l", goSize_t x = 0, goSize_t y = 0);
            void plot (const goMath::Vectorf& py, const char* title = "", const char* options = "w l", goSize_t x = 0, goSize_t y = 0);
            void plot (const goMath::Vectord& py, const char* title = "", const char* options = "w l", goSize_t x = 0, goSize_t y = 0);
            void plotImage (const goSignal3DBase<void>& image, const char* title = "", const char* options = "w image", goSize_t x = 0, goSize_t y = 0);
            void plotImage (const goMath::Matrixf& image, const char* title = "", const char* options = "w image", goSize_t x = 0, goSize_t y = 0);
            void plotImage (const goMath::Matrixd& image, const char* title = "", const char* options = "w image", goSize_t x = 0, goSize_t y = 0);
            void plotPoint (goDouble px, goDouble py, const char* title = "", const char* options = "w p", goSize_t x = 0, goSize_t y = 0);
            void plotPoint (const goMath::Vectorf& p, const char* title = "", const char* options = "w p", goSize_t x = 0, goSize_t y = 0);
            void plotPoint (const goMath::Vectord& p, const char* title = "", const char* options = "w p", goSize_t x = 0, goSize_t y = 0);
            void plotLine  (const goMath::Vectorf& n, const goMath::Vectorf& p, const char* title = "", const char* options = "w l", goSize_t x = 0, goSize_t y = 0);
            void plotLine  (const goMath::Vectord& n, const goMath::Vectord& p, const char* title = "", const char* options = "w l", goSize_t x = 0, goSize_t y = 0);
            void plot (goMultiPlotter& mp);
            void plot ();
            void plotPostscript (const char* filename, goFloat sizeX = -1.0f, goFloat sizeY = -1.0f);
            void plotFile (const char* filename, const char* termstring);
            void plotPause ();
            void saveGnuplot (const char* filename);
            void setPrefix (const char* p);
            void addPrefix (const char* p);
            void setPostfix (const char* p);
            void addPostfix (const char* p);
            void setTitle (const char* s);

            void clear ();
            void clear (goSize_t x, goSize_t y);

            goAutoPtr<goSinglePlot> getPlotp (goSize_t x, goSize_t y);
            goSinglePlot& getPlot (goSize_t x, goSize_t y);
            goGnuplot& getGnuplot ();

        private:
            PlotPrivate* myPrivate;
    };

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
                      goString*       cmdFilenameRet,
                      int             redirectInputFD = -1,
                      int             redirectOutputFD = -1);

    bool addGnuplotCommands (goString& gnuplotCommands,
                             const goList<goString>* dataFileNames, 
                             const goList<goString>* titles, 
                             const goList<goString>* plotCommands, 
                             const char* prefixCommands,
                             const char* postfixCommands,
                             goPlot::PlotType plotType = goPlot::Normal);
    template <class arrayT>
        bool writeGnuplotDataFiles (const goList<arrayT>* arrayListX, 
                                    const goList<arrayT>* arrayListY, 
                                    goList<goString>& dataFileNameRet);

    //= For 3D-plots
    template <class arrayT>
        bool writeGnuplotDataFiles (const goList<arrayT>* arrayListX, 
                                    const goList<arrayT>* arrayListY, 
                                    const goList<arrayT>* arrayListZ,
                                    goIndex_t             lineLength,
                                    goList<goString>& dataFileNameRet);
    //= For 3D-plots
    template <class T>
        bool writeGnuplotDataFiles (const goList<goMath::Matrix<T> >*    matrix,
                                    goList<goString>& dataFileNameRet);
    //= For 2D-images
    template <class T>
        bool writeGnuplotDataFilesBinary (const goList<goMath::Matrix<T> >*    matrix,
                                    goList<goString>& dataFileNameRet);

    //= For 3D-plots
    bool writeGnuplotDataFiles (const goList<const goSignal3DBase<void>*>* images,
                                goList<goString>&     dataFileNameRet);
#if 0
        static bool gnuplot (const goArray<goFloat>&, const char* title = 0, const char* gnuplotCommands = 0, goString* cmdFileNameRet = 0, goString* dataFileNameRet = 0, bool waitfor = false);
        static bool gnuplot (const goArray<goDouble>&, const char* title = 0, const char* gnuplotCommands = 0, goString* cmdFileNameRet = 0, goString* dataFileNameRet = 0, bool waitfor = false);
        static bool gnuplot (const goFixedArray<goFloat>&, const char* title = 0, const char* gnuplotCommands = 0, goString* cmdFileNameRet = 0, goString* dataFileNameRet = 0, bool waitfor = false);
        static bool gnuplot (const goFixedArray<goDouble>&, const char* title = 0, const char* gnuplotCommands = 0, goString* cmdFileNameRet = 0, goString* dataFileNameRet = 0, bool waitfor = false);
#endif


        /** 
         * @brief For use in the goPlot library (uses the same namespace).
         *
         * Points stored in a goMath::Matrix<T> object, one point per row.
         */
        template <class T>
            class Points2DMatrix : public goPlot ::Points2D <T>
        {
            public:
                Points2DMatrix () 
                    : goPlot ::Points2D <T> (),
                    myMatrix (0, 0)
            {
            }

                virtual ~Points2DMatrix ()
                {
                }

                Points2DMatrix (const goMatrix<T>& M)
                    : goPlot ::Points2D<T> (),
                    myMatrix (M)
            {
            }

                Points2DMatrix& operator= (const goMatrix<T>& o)
                {
                    myMatrix = o;
                    return *this;
                }

                virtual T x (int i) const { return myMatrix (i, 0); }
                virtual T y (int i) const { return myMatrix (i, 1); }

                virtual void set (int i, T x, T y) 
                {
                    myMatrix (i, 0) = x;
                    myMatrix (i, 1) = y;
                }
                //= Return number of points
                virtual size_t size () const
                {
                    return myMatrix.getRows ();
                }

            private:
                goMatrix<T> myMatrix;
        };

}

#endif
