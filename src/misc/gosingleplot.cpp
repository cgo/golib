#include <goplot.h>
#include <gofileio.h>
#include <goautoptr.h>

class goPlotElement
{
    public:
        goPlotElement (const char* command = "plot", const char* fn = "-" , const char* options = "with lines")
            : myPlotOptions (options),
              myPlotCommand (command),
              myFilename (fn)
        {
        };
        virtual ~goPlotElement () 
        {
            if (myFilename != "-")
            {
                goFileIO::remove (myFilename);
            }
        };

        void setPlotCommand (const char* cmd) { myPlotCommand = cmd; };
        void setPlotOptions (const char* cmd) { myPlotOptions = cmd; };
        void setPlotCommand (const goString& cmd) { myPlotCommand = cmd; };
        void setPlotOptions (const goString& cmd) { myPlotOptions = cmd; };
        void setFilename    (const char* fn) { myFilename = fn; };
        void setFilename    (const goString& fn) { myFilename = fn; };

        const goString& plotCommand () const { return myPlotCommand; };
        const goString& plotOptions () const { return myPlotOptions; };
        const goString& filename    () const { return myFilename; };

        virtual void data (goString& ret) const {};

        goString myPlotOptions;
        goString myPlotCommand;
        goString myFilename;
};

template <class T>
class goPlotElementMatrixImage : public goPlotElement
{
    public:
        goPlotElementMatrixImage (const goMatrix<T>& M) : goPlotElement ("plot", "-", "with image"), myMatrix (M) { };
        virtual ~goPlotElementMatrixImage () {};

        virtual void data (goString& ret) const
        {
            for (goSize_t i = 0; i < myMatrix.getRows(); ++i)
            {
                for (goSize_t j = 0; j < myMatrix.getColumns(); ++j)
                {
                    ret += (int)j; ret += " "; ret += (int)i; ret += " "; ret += (float)myMatrix(i,j); ret += "\n";
                }
            }
        };

        goMatrix<T> myMatrix;
};

template <class T>
class goPlotElementMatrixSurface : public goPlotElement
{
    public:
        goPlotElementMatrixSurface (const goMatrix<T>& M) : goPlotElement ("splot" , "-", "with lines"), myMatrix (M) { };
        virtual ~goPlotElementMatrixSurface () {};

        virtual void data (goString& ret) const
        {
            for (goSize_t i = 0; i < myMatrix.getRows(); ++i)
            {
                for (goSize_t j = 0; j < myMatrix.getColumns(); ++j)
                {
                    ret += (int)j; ret += " "; ret += (int)i; ret += " "; ret += (float)myMatrix(i,j); ret += "\n";
                }
                ret += "\n";
            }
        };

        goMatrix<T> myMatrix;
};

template <class T>
class goPlotElementPlane : public goPlotElement
{
    public:
        goPlotElementPlane (const goVector<T>& n, const goVector<T>& p, T dx = 0.1, T dy = 0.1, T sx = 1.0, T sy = 1.0) 
            : goPlotElement ("splot" , "-", "with lines"), 
              myNormal (n), 
              myPoint (p),
              myDx (dx),
              myDy (dy),
              mySx (sx),
              mySy (sy) { };
        virtual ~goPlotElementPlane () {};

        virtual void data (goString& ret) const
        {

            //= Find orthonormal basis of the plane
            goVector<T> x(3), y(3);
            x[0] = 1.0; x[1] = 0.0; x[2] = 0.0;
            goVector<T> n (myNormal);
            n *= 1.0 / n.norm2();
            if (fabs(1.0 - n * x) < 1e-3)
            {
                x[0] = 0.0; x[1] = 1.0; x[2] = 0.0;
            }
            n.cross (x, y);  //= y = n X x
            y.cross (n, x);
            x *= 1.0 / x.norm2();
            y *= 1.0 / y.norm2();

            goVector<T> p (3);

            for (T u = -0.5 * mySx; u <= 0.5 * mySx; u += myDx)
            {
                for (T v = -0.5 * mySy; v <= 0.5 * mySy; v += myDy)
                {
                    p = myPoint + x * u + y * v;
                    ret += (float)p[0]; ret += " "; ret += (float)p[1]; ret += " "; ret += (float)p[2]; ret += "\n";
                }
                ret += "\n";
            }
        };

        goVector<T> myNormal;
        goVector<T> myPoint;
        T myDx;
        T myDy;
        T mySx;
        T mySy;
};

template <class T>
class goPlotElementLine : public goPlotElement
{
    public:
        goPlotElementLine (const goVector<T>& n, const goVector<T>& p)
            : goPlotElement ("plot" , "-", "with lines"), 
              myDirection (n), 
              myPoint (p) 
        {
            if (n.getSize() > 2)
            {
                const_cast<goPlotElementLine*>(this)->setPlotCommand ("splot");
            }
        };
        virtual ~goPlotElementLine () {};

        virtual void data (goString& ret) const
        {
            goSize_t sz = myDirection.getSize();
            if (sz != myPoint.getSize())
            {
                goLog::warning ("goPlotElementLine: myPoint different size than myDirection");
                return;
            }
            for (goSize_t i = 0; i < sz; ++i)
            {
                ret += (float)myPoint[i]; ret += " ";
            }
            ret += "\n";
            goVector<T> p2 = myPoint + myDirection;
            for (goSize_t i = 0; i < sz; ++i)
            {
                ret += (float)p2[i]; ret += " ";
            }
            ret += "\n";
        };

        goVector<T> myDirection;
        goVector<T> myPoint;
};

template <class T>
class goPlotElementMatrixCurve : public goPlotElement
{
    public:
        goPlotElementMatrixCurve (const goMatrix<T>& M) : goPlotElement ("plot", "-", "with lines"), myMatrix (M) { };
        virtual ~goPlotElementMatrixCurve () {};

        virtual void data (goString& ret) const
        {
            for (goSize_t i = 0; i < myMatrix.getRows(); ++i)
            {
                for (goSize_t j = 0; j < myMatrix.getColumns(); ++j)
                {
                    ret += (float)myMatrix(i,j); ret += " ";
                }
                ret += "\n";
            }
        };

        goMatrix<T> myMatrix;
};

template <class T>
class goPlotElementPoint : public goPlotElement
{
    public:
        goPlotElementPoint (const goVector<T>& p) : goPlotElement ("plot", "-", "with points"), myPoint (p) { };
        virtual ~goPlotElementPoint () {};

        virtual void data (goString& ret) const
        {
            for (goSize_t j = 0; j < myPoint.getSize(); ++j)
            {
                ret += (float)myPoint[j]; ret += " ";
            }
            ret += "\n";
        };

        goVector<T> myPoint;
};

template <class T>
class goPlotElementVectorCurve : public goPlotElement
{
    public:
        goPlotElementVectorCurve (const goVector<T>* x, const goVector<T>* y, const goVector<T>* z, int linelength) 
            : goPlotElement ("plot", "-", "with lines"), myX(0), myY(0), myZ(0), myLinelength(linelength)
        {
            if (x)
                myX = *x;
            if (y)
                myY = *y;
            if (z)
                myZ = *z;
        };
        virtual ~goPlotElementVectorCurve () {};

        virtual void data (goString& ret) const
        {
            goSize_t N = myX.getSize();
            bool haveY = (myY.getSize() == N);
            bool haveZ = (myZ.getSize() == N);
            int ll = 0;
            for (goSize_t i = 0; i < N; ++i)
            {
                if (haveZ && haveY)
                {
                    ret += (float)myX[i]; ret += " "; ret += (float)myY[i]; ret += " "; ret += (float)myZ[i]; ret += "\n";
                    ++ll;
                    if (ll >= myLinelength)
                    {
                        ret += "\n";
                    }
                }
                if (haveY)
                {
                    ret += (float)myX[i]; ret += " "; ret += (float)myY[i]; ret += "\n";
                }
                else
                {
                    ret += (int)i; ret += " "; ret += (float)myX[i]; ret += "\n";
                }
            }
        };

        goVector<T> myX, myY, myZ;
        int myLinelength;
};


class goSinglePlotPrivate
{
    public:
        goSinglePlotPrivate() 
            : 
              plotElements (),
              plotX(), 
              plotY(), 
              plotZ(),
              plotMatrixf(),
              plotMatrixd(),
              plotImages(),
              plotImageMatrixf(),
              plotImageMatrixd(),
              lineLength(0),
              titles(), 
              plotCommands(), 
              prefixCommands(""), 
              dataFilenames(),
              row(0),
              column(0),
              title(""),
              plotType(goPlot::Normal),
              useFiles(false)
        {};
        ~goSinglePlotPrivate() {};

        goList<goAutoPtr<goPlotElement> > plotElements;  //= This should deprecate the lists of different objects below.

        //= Except for labels, these are only for the old interface to gnuplot using temporary files and
        //= the functions in goplot.cpp. These will deprecate in the future.
        goList<goVectord>      plotX;
        goList<goVectord>      plotY;
        goList<goVectord>      plotZ;
        goList<goMatrixf>      plotMatrixf;
        goList<goMatrixd>      plotMatrixd;
        goList<const goSignal3DBase<void>*> plotImages;
        goList<goMatrixf>      plotImageMatrixf;  // For 2D image plotting -- the others are for 3D.
        goList<goMatrixd>      plotImageMatrixd;  // dito.
        goIndex_t              lineLength;      // Meaning only in 3D plots -- length of one grid line in x direction.
        goList<goString>       titles;          // These are titles for each of the individual curves
        goList<goPlotterLabel> labels;
        goList<goString>       plotCommands;
        goString               prefixCommands;

        goList<goString>  dataFilenames;

        goSize_t          row;
        goSize_t          column;

        goString          title;               // This is the title of the whole plot.

        goPlot::PlotType  plotType;

        bool              useFiles;
};

goSinglePlot::goSinglePlot ()
    : goObjectBase (), myPrivate (0)
{
    this->setClassID (GO_SINGLEPLOT);
    myPrivate = new goSinglePlotPrivate;
}

goSinglePlot::~goSinglePlot ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

goSinglePlot::goSinglePlot (const goSinglePlot& other)
    : goObjectBase (), myPrivate (0)
{
    this->setClassID (GO_SINGLEPLOT);
    myPrivate = new goSinglePlotPrivate;
    *this = other;
}

const goSinglePlot& goSinglePlot::operator= (const goSinglePlot& other)
{
    *this->myPrivate = *other.myPrivate;
    return *this;
}

/** 
 * @brief Returns false. Comparison doesn't make sense.
 * 
 * @return false
 */
bool goSinglePlot::operator== (const goSinglePlot&) const
{
    return false;
}

bool goSinglePlot::operator!= (const goSinglePlot& other) const
{
    return !(*this == other);
}

/** 
 * @brief Set position in multiplot.
 * 
 * Sets the position in the goMultiPlotter grid.
 * The user does not have to set this, it is set with goMultiPlotter::addPlot().
 *
 * @param row Row
 * @param col Column
 */
void goSinglePlot::setPosition (goSize_t row, goSize_t col)
{
    myPrivate->row = row;
    myPrivate->column = col;
}

/** 
 * @brief Get row position in the multiplot grid.
 * 
 * @return Row position.
 */
goSize_t goSinglePlot::getRow () const
{
    return myPrivate->row;
}

/** 
 * @brief Get column position in the multiplot grid.
 * 
 * @return Column position.
 */
goSize_t goSinglePlot::getColumn () const
{
    return myPrivate->column;
}

/** 
 * @brief Add 3D plot given by x,y,z coordinates.
 * 
 * @param x x coordinates
 * @param y y coordinates
 * @param lineLength Length of the grid in x-direction (number of elements in one line)
 * @param values z values
 * @param title Title
 * @param plotOptions plot options as in the other add* functions.
 * 
 * @return True if successful, false otherwise.
 */
bool goSinglePlot::add3D (const goVectord& x, const goVectord& y,
                          goIndex_t lineLength,
                          const goVectord& values, 
                          const char* title, 
                          const char* plotOptions)
{
    if (myPrivate->plotType != goPlot::Surface)
    {
        // this->clear (); //= This deletes all labels ... not good.
        myPrivate->plotType = goPlot::Surface;
    }
    myPrivate->plotX.append (x);
    myPrivate->plotY.append (y);
    myPrivate->plotZ.append (values);
    myPrivate->lineLength = lineLength;

    myPrivate->titles.append(goString(title));
    goString temp = "";
    if (!plotOptions)
    {
        temp = "w l";
    }
    else
    {
        temp = plotOptions;
    }
    temp += " title \""; temp += title; temp += "\"";
    myPrivate->plotCommands.append(temp);

    goAutoPtr<goPlotElement> aptr = goAutoPtr<goPlotElement> (new goPlotElementVectorCurve<goDouble>(&x,&y,&values,lineLength));
    if (plotOptions)
        aptr->setPlotOptions (plotOptions);
    if (title)
    {
        goString newpo = aptr->plotOptions();
        newpo += " title \"";
        newpo += title;
        newpo += "\"";
        aptr->setPlotOptions (newpo.toCharPtr());
    }
    myPrivate->plotElements.append (aptr);

    return true;
}

bool goSinglePlot::add3D (const goVectorf& x, const goVectorf& y,
                          goIndex_t lineLength,
                          const goVectorf& values, 
                          const char* title, 
                          const char* plotOptions)
{
    if (myPrivate->plotType != goPlot::Surface)
    {
        this->clear ();
        myPrivate->plotType = goPlot::Surface;
    }

    goVectord tempx (x.getSize());
    goVectord tempy (y.getSize());
    goVectord tempz (values.getSize());
    goSize_t sz = x.getSize();
    for (goSize_t i = 0; i < sz; ++i) tempx[i] = x[i];
    sz = y.getSize();
    for (goSize_t i = 0; i < sz; ++i) tempy[i] = y[i];
    sz = tempz.getSize();
    for (goSize_t i = 0; i < sz; ++i) tempz[i] = values[i];

    return this->add3D (tempx, tempy, lineLength, tempz, title, plotOptions);
}

bool goSinglePlot::add3D (const goMatrixf& m, const char* title, const char* plotOptions, bool separateRows)
{
    if (myPrivate->plotType != goPlot::Surface)
    {
        this->clear ();
        myPrivate->plotType = goPlot::Surface;
    }
    if (!separateRows)
    {
        myPrivate->plotMatrixf.append (m);
        myPrivate->titles.append(goString(title));
        goString temp = "";
        if (!plotOptions)
        {
            temp = "w l";
        }
        else
        {
            temp = plotOptions;
        }
        temp += " title \""; temp += title; temp += "\"";
        myPrivate->plotCommands.append(temp);


        //= New
        goAutoPtr<goPlotElement> aptr = goAutoPtr<goPlotElement> (new goPlotElementMatrixSurface<goFloat>(m));
        if (plotOptions)
            aptr->setPlotOptions (plotOptions);
        if (title)
        {
            goString newpo = aptr->plotOptions();
            newpo += " title \"";
            newpo += title;
            newpo += "\"";
            aptr->setPlotOptions (newpo);
        }
        myPrivate->plotElements.append (aptr);
    }
    else
    {
        const goMatrixf ref;
        goSize_t sz = m.getRows();
        for (goSize_t i = 0; i < sz; ++i)
        {
            goVectorf y (m.getColumns());
            y.fill (float(i));
            goVectorf x (m.getColumns());
            for (goSize_t j = 0; j < x.getSize(); ++j)
                x[j] = float(j);
            const goVectorf z;
            m.refRow (i, z);
            this->add3D (x,y,x.getSize(),z,title,plotOptions);
        }
    }

    return true;
}

bool goSinglePlot::add3D (const goMatrixd& m, const char* title, const char* plotOptions, bool separateRows)
{
    if (myPrivate->plotType != goPlot::Surface)
    {
        this->clear ();
        myPrivate->plotType = goPlot::Surface;
    }
    if (!separateRows)
    {
        myPrivate->plotMatrixd.append (m);
        myPrivate->titles.append(goString(title));
        goString temp = "";
        if (!plotOptions)
        {
            temp = "w l";
        }
        else
        {
            temp = plotOptions;
        }
        temp += " title \""; temp += title; temp += "\"";
        myPrivate->plotCommands.append(temp);


        //= New
        goAutoPtr<goPlotElement> aptr = goAutoPtr<goPlotElement> (new goPlotElementMatrixSurface<goDouble>(m));
        if (plotOptions)
            aptr->setPlotOptions (plotOptions);
        if (title)
        {
            goString newpo = aptr->plotOptions();
            newpo += " title \"";
            newpo += title;
            newpo += "\"";
            aptr->setPlotOptions (newpo);
        }
        myPrivate->plotElements.append (aptr);
//        if (!plotOptions)
//        {
//            myPrivate->plotCommands.append(goString("w l"));
//        }
//        else
//        {
//            myPrivate->plotCommands.append(goString(plotOptions));
//        }
    }
    else
    {
        const goMatrixd ref;
        goSize_t sz = m.getRows();
        for (goSize_t i = 0; i < sz; ++i)
        {
            m.ref (i, 0, 1, m.getColumns(), ref);
            myPrivate->plotMatrixd.append (ref);
            myPrivate->titles.append(goString(title));
            goString temp = "";
            if (!plotOptions)
            {
                temp = "w l";
            }
            else
            {
                temp = plotOptions;
            }
            temp += " title \""; temp += title; temp += "\"";
            myPrivate->plotCommands.append(temp);

//            if (!plotOptions)
//            {
//                myPrivate->plotCommands.append(goString("w l"));
//            }
//            else
//            {
//                myPrivate->plotCommands.append(goString(plotOptions));
//            }
        }
    }

    return true;
}

/** 
 * @brief Add surface to draw as goSignal3DBase<void>.
 *
 * @note Only the current channel of the 0th z slice will be used since it is assumed the input signal is
 * a 2D image.
 * 
 * @param image Image object. May not be deleted as long as this goSinglePlot uses it.
 * @param title Title.
 * @param plotOptions  Options for gnuplot (like "with lines", "with points").
 * 
 * @return True if successful, false otherwise.
 */
bool goSinglePlot::add3D (const goSignal3DBase<void>* image, const char* title, const char* plotOptions)
{
    if (myPrivate->plotType != goPlot::Surface)
    {
        this->clear ();
        myPrivate->plotType = goPlot::Surface;
    }

    myPrivate->plotImages.append (image);
    goString temp = "";
    if (!plotOptions)
    {
        temp = "w l";
    }
    else
    {
        temp = plotOptions;
    }
    temp += " title \""; temp += title; temp += "\"";
    myPrivate->plotCommands.append(temp);

    goLog::warning ("goSinglePlot::add3D(goSignal3DBase): no new version for signals yet! Will not be added to plot list using new plot objects.");
//    myPrivate->titles.append(goString(title));
//    if (!plotOptions)
//    {
//        myPrivate->plotCommands.append(goString("w l"));
//    }
//    else
//    {
//        myPrivate->plotCommands.append(goString(plotOptions));
//    }
    return true;
}

bool goSinglePlot::addImage (const goMatrixf& m, const char* title, const char* plotOptions)
{
    if (myPrivate->plotType != goPlot::Normal)
    {
        this->clear ();
        myPrivate->plotType = goPlot::Normal;
    }

    myPrivate->plotImageMatrixf.append (m);
    myPrivate->titles.append(goString(""));
    goString temp = "";
    if (!plotOptions)
    {
        temp = "binary w image";
    }
    else
    {
        temp = plotOptions;
    }
    if (title != "")
    {
        temp += " title \""; temp += title; temp += "\"";
    }
    else
    {
        temp += " notitle";
    }
    myPrivate->plotCommands.append(temp);


    //= New
    goAutoPtr<goPlotElement> aptr = goAutoPtr<goPlotElement> (new goPlotElementMatrixImage<goFloat>(m));
    if (plotOptions)
        aptr->setPlotOptions (plotOptions);
    if (title)
    {
        goString newpo = aptr->plotOptions();
        newpo += " title \"";
        newpo += title;
        newpo += "\"";
        aptr->setPlotOptions (newpo);
    }
    myPrivate->plotElements.append (aptr);
    return true;
}

bool goSinglePlot::addImage (const goMatrixd& m, const char* title, const char* plotOptions)
{
    if (myPrivate->plotType != goPlot::Normal)
    {
        this->clear ();
        myPrivate->plotType = goPlot::Normal;
    }

    myPrivate->plotImageMatrixd.append (m);
    myPrivate->titles.append(goString(""));
    goString temp = "";
    if (!plotOptions)
    {
        temp = "binary w image";
    }
    else
    {
        temp = plotOptions;
    }
    if (title != "")
    {
        temp += " title \""; temp += title; temp += "\"";
    }
    else
    {
        temp += " notitle";
    }
    myPrivate->plotCommands.append(temp);


    //= New
    goAutoPtr<goPlotElement> aptr = goAutoPtr<goPlotElement> (new goPlotElementMatrixImage<goDouble>(m));
    if (plotOptions)
        aptr->setPlotOptions (plotOptions);
    if (title)
    {
        goString newpo = aptr->plotOptions();
        newpo += " title \"";
        newpo += title;
        newpo += "\"";
        aptr->setPlotOptions (newpo);
    }
    myPrivate->plotElements.append (aptr);
    return true;
}

/** 
 * @brief Add a curve to the plot.
 * 
 * @param x X coordinates of curve points.
 * @param y Y coordinates of curve points.
 * @param title Title of the curve (set to "" for none).
 * @param plotOptions Options for gnuplot, written after the plot command, default is "w l" for "with lines". E.g. set this to
 * "with linespoints" for drawing with points and lines.
 * 
 * @return True if successful, false otherwise.
 */
bool goSinglePlot::addCurve (const goVectord& x, const goVectord& y, const char* title, const char* plotOptions)
{
    if (x.getSize() != y.getSize())
    {
        goLog::warning("addCurve(): x and y array sizes mismatch.",this);
        return false;
    }

    if (myPrivate->plotType != goPlot::Normal)
    {
        this->clear ();
        myPrivate->plotType = goPlot::Normal;
    }

    myPrivate->plotX.append(x);
    myPrivate->plotY.append(y);
    myPrivate->titles.append(goString(title));
    goString temp = "";
    if (!plotOptions)
    {
        temp = "w l";
    }
    else
    {
        temp = plotOptions;
    }
    if (title != "")
    {
        temp += " title \""; temp += title; temp += "\"";
    }
    else
    {
        temp += " notitle";
    }
    myPrivate->plotCommands.append(temp);


    //= New
    goAutoPtr<goPlotElement> aptr = goAutoPtr<goPlotElement> (new goPlotElementVectorCurve<goDouble>(&x,&y,0,1));
    if (plotOptions)
        aptr->setPlotOptions (plotOptions);
    if (title)
    {
        goString newpo = aptr->plotOptions();
        newpo += " title \"";
        newpo += title;
        newpo += "\"";
        aptr->setPlotOptions (newpo);
    }
    myPrivate->plotElements.append (aptr);
    return true;
}

/** 
 * @brief Add a curve to the plot.
 * 
 * @param x X coordinates of curve points.
 * @param y Y coordinates of curve points.
 * @param title Title of the curve (set to "" for none).
 * @param plotOptions Options for gnuplot, written after the plot command, default is "w l" for "with lines". E.g. set this to
 * "with linespoints" for drawing with points and lines.
 * 
 * @return True if successful, false otherwise.
 */
bool goSinglePlot::addCurve (const goVectorf& x, const goVectorf& y, const char* title, const char* plotOptions)
{
    if (x.getSize() != y.getSize())
    {
        goLog::warning("addCurve(): x and y array sizes mismatch.",this);
        return false;
    }

    if (myPrivate->plotType != goPlot::Normal)
    {
        this->clear ();
        myPrivate->plotType = goPlot::Normal;
    }

    goSize_t sz = x.getSize();
    goVectord tempX (sz);
    goVectord tempY (sz);
    for (goSize_t i = 0; i < sz; ++i)
    {
        tempX[i] = x[i];
        tempY[i] = y[i];
    }

    myPrivate->plotX.append(tempX);
    myPrivate->plotY.append(tempY);
    myPrivate->titles.append(goString(title));
    goString temp = "";
    if (!plotOptions)
    {
        temp = "w l";
    }
    else
    {
        temp = plotOptions;
    }
    if (title != "")
    {
        temp += " title \""; temp += title; temp += "\"";
    }
    else
    {
        temp += " notitle";
    }
    myPrivate->plotCommands.append(temp);


    //= New
    goAutoPtr<goPlotElement> aptr = goAutoPtr<goPlotElement> (new goPlotElementVectorCurve<goFloat>(&x,&y,0,1));
    if (plotOptions)
        aptr->setPlotOptions (plotOptions);
    if (title)
    {
        goString newpo = aptr->plotOptions();
        newpo += " title \"";
        newpo += title;
        newpo += "\"";
        aptr->setPlotOptions (newpo);
    }
    myPrivate->plotElements.append (aptr);
    return true;
}

/** 
 * @brief Add curve defined by configuration matrix.
 *
 * The matrix contains one point per row.
 * Dimensions must be m x 2, where m is the number of points.
 * 
 * @param m Configuration matrix.
 * @param title  Title of the curve.
 * @param plotOptions  Options (see addCurve() functions).
 * 
 * @return True if successful, false otherwise.
 */
bool goSinglePlot::addCurveMatrix (const goMatrixf& m, const char* title, const char* plotOptions)
{
//    const goVectorf x;
//    const goVectorf y;
//    m.refColumn (0, x);
//    m.refColumn (1, y);
//    return this->addCurve (x, y, title, plotOptions);
    //= New
    goAutoPtr<goPlotElement> aptr = goAutoPtr<goPlotElement> (new goPlotElementMatrixCurve<goFloat>(m));
    if (plotOptions)
        aptr->setPlotOptions (plotOptions);
    if (title)
    {
        goString newpo = aptr->plotOptions();
        newpo += " title \"";
        newpo += title;
        newpo += "\"";
        aptr->setPlotOptions (newpo);
    }
    myPrivate->plotElements.append (aptr);
    return true;
}

bool goSinglePlot::addCurveMatrix (const goMatrixd& m, const char* title, const char* plotOptions)
{
    //const goVectord x;
    //const goVectord y;
    //m.refColumn (0, x);
    //m.refColumn (1, y);
    //return this->addCurve (x, y, title, plotOptions);
    //= New
    goAutoPtr<goPlotElement> aptr = goAutoPtr<goPlotElement> (new goPlotElementMatrixCurve<goDouble>(m));
    if (plotOptions)
        aptr->setPlotOptions (plotOptions);
    if (title)
    {
        goString newpo = aptr->plotOptions();
        newpo += " title \"";
        newpo += title;
        newpo += "\"";
        aptr->setPlotOptions (newpo);
    }
    myPrivate->plotElements.append (aptr);
    return true;
}

bool goSinglePlot::addPoint (const goVectorf& p, const char* title, const char* plotOptions)
{
    if (myPrivate->plotType != goPlot::Surface && p.getSize() > 2)
    {
        // this->clear (); //= This deletes all labels ... not good.
        myPrivate->plotType = goPlot::Surface;
    }
    //= New
    goAutoPtr<goPlotElement> aptr = goAutoPtr<goPlotElement> (new goPlotElementPoint<goFloat>(p));
    if (plotOptions)
        aptr->setPlotOptions (plotOptions);
    if (title)
    {
        goString newpo = aptr->plotOptions();
        newpo += " title \"";
        newpo += title;
        newpo += "\"";
        aptr->setPlotOptions (newpo);
    }
    myPrivate->plotElements.append (aptr);
    return true;
}

bool goSinglePlot::addPoint (const goVectord& p, const char* title, const char* plotOptions)
{
    if (myPrivate->plotType != goPlot::Surface && p.getSize() > 2)
    {
        // this->clear (); //= This deletes all labels ... not good.
        myPrivate->plotType = goPlot::Surface;
    }
    //= New
    goAutoPtr<goPlotElement> aptr = goAutoPtr<goPlotElement> (new goPlotElementPoint<goDouble>(p));
    if (plotOptions)
        aptr->setPlotOptions (plotOptions);
    if (title)
    {
        goString newpo = aptr->plotOptions();
        newpo += " title \"";
        newpo += title;
        newpo += "\"";
        aptr->setPlotOptions (newpo);
    }
    myPrivate->plotElements.append (aptr);
    return true;
}

bool goSinglePlot::addPlane (const goVectorf& n, const goVectorf& p, goDouble dx, goDouble dy, goDouble sx, goDouble sy, const char* title, const char* plotOptions)
{
    if (myPrivate->plotType != goPlot::Surface)
    {
        // this->clear (); //= This deletes all labels ... not good.
        myPrivate->plotType = goPlot::Surface;
    }
    //= New
    goAutoPtr<goPlotElement> aptr = goAutoPtr<goPlotElement> (new goPlotElementPlane<goFloat>(n,p,dx,dy,sx,sy));
    if (plotOptions)
        aptr->setPlotOptions (plotOptions);
    if (title)
    {
        goString newpo = aptr->plotOptions();
        newpo += " title \"";
        newpo += title;
        newpo += "\"";
        aptr->setPlotOptions (newpo);
    }
    myPrivate->plotElements.append (aptr);
    return true;
}

bool goSinglePlot::addPlane (const goVectord& n, const goVectord& p, goDouble dx, goDouble dy, goDouble sx, goDouble sy, const char* title, const char* plotOptions)
{
    if (myPrivate->plotType != goPlot::Surface)
    {
        // this->clear (); //= This deletes all labels ... not good.
        myPrivate->plotType = goPlot::Surface;
    }
    //= New
    goAutoPtr<goPlotElement> aptr = goAutoPtr<goPlotElement> (new goPlotElementPlane<goDouble>(n,p,dx,dy,sx,sy));
    if (plotOptions)
        aptr->setPlotOptions (plotOptions);
    if (title)
    {
        goString newpo = aptr->plotOptions();
        newpo += " title \"";
        newpo += title;
        newpo += "\"";
        aptr->setPlotOptions (newpo);
    }
    myPrivate->plotElements.append (aptr);
    return true;
}

bool goSinglePlot::addLine (const goVectorf& n, const goVectorf& p, const char* title, const char* plotOptions)
{
    if (myPrivate->plotType != goPlot::Surface && n.getSize() > 2)
    {
        // this->clear (); //= This deletes all labels ... not good.
        myPrivate->plotType = goPlot::Surface;
    }
    //= New
    goAutoPtr<goPlotElement> aptr = goAutoPtr<goPlotElement> (new goPlotElementLine<goFloat>(n,p));
    if (plotOptions)
        aptr->setPlotOptions (plotOptions);
    if (title)
    {
        goString newpo = aptr->plotOptions();
        newpo += " title \"";
        newpo += title;
        newpo += "\"";
        aptr->setPlotOptions (newpo);
    }
    myPrivate->plotElements.append (aptr);
    return true;
}

bool goSinglePlot::addLine (const goVectord& n, const goVectord& p, const char* title, const char* plotOptions)
{
    if (myPrivate->plotType != goPlot::Surface && n.getSize() > 2)
    {
        // this->clear (); //= This deletes all labels ... not good.
        myPrivate->plotType = goPlot::Surface;
    }
    //= New
    goAutoPtr<goPlotElement> aptr = goAutoPtr<goPlotElement> (new goPlotElementLine<goDouble>(n,p));
    if (plotOptions)
        aptr->setPlotOptions (plotOptions);
    if (title)
    {
        goString newpo = aptr->plotOptions();
        newpo += " title \"";
        newpo += title;
        newpo += "\"";
        aptr->setPlotOptions (newpo);
    }
    myPrivate->plotElements.append (aptr);
    return true;
}

/** 
 * @brief Add a label at a given position.
 * 
 * @param l Label text.
 * @param x Position X
 * @param y Position Y
 * 
 * @return True if successful, false otherwise.
 */
bool goSinglePlot::addLabel (const goString& l, goDouble x, goDouble y, const char* colourspec)
{
    return myPrivate->labels.append (goPlotterLabel(l.toCharPtr(), x, y, 0.0, colourspec));
}
bool goSinglePlot::addLabel (const char* l, goDouble x, goDouble y, const char* colourspec)
{
    return this->addLabel (goString(l), x, y, 0.0, colourspec);
}
bool goSinglePlot::addLabel (const goString& l, goDouble x, goDouble y, goDouble z, const char* colourspec)
{
    return myPrivate->labels.append (goPlotterLabel(l.toCharPtr(), x, y, z, colourspec));
}
bool goSinglePlot::addLabel (const char* l, goDouble x, goDouble y, goDouble z, const char* colourspec)
{
    return this->addLabel (goString(l), x, y, z, colourspec);
}

void goSinglePlot::setPrefix (const goString& p)
{
    myPrivate->prefixCommands = p;
    myPrivate->prefixCommands += "\n";
}
void goSinglePlot::setPrefix (const char* p)
{
    this->setPrefix (goString(p));
}

const goString& goSinglePlot::getPrefix () const
{
    return myPrivate->prefixCommands;
}

void goSinglePlot::setTitle (const goString& s)
{
    myPrivate->title = s;
}
void goSinglePlot::setTitle (const char* s)
{
    this->setTitle (goString(s));
}

bool goSinglePlot::writeGnuplotDataFiles () const
{
    switch (myPrivate->plotType)
    {
        case goPlot::Normal:
            {
                myPrivate->dataFilenames.erase ();
                if (!goPlot::writeGnuplotDataFiles (&myPrivate->plotX,
                            &myPrivate->plotY,
                            myPrivate->dataFilenames))
                {
                    return false;
                }
                if (myPrivate->plotImageMatrixf.getSize() > 0)
                {
                    if (!goPlot::writeGnuplotDataFilesBinary<goFloat> (&myPrivate->plotImageMatrixf,
                                myPrivate->dataFilenames))
                    {
                        return false;
                    }
                }
                if (myPrivate->plotImageMatrixd.getSize() > 0)
                {
                    if (!goPlot::writeGnuplotDataFilesBinary<goDouble> (&myPrivate->plotImageMatrixd,
                                myPrivate->dataFilenames))
                    {
                        return false;
                    }
                }
            }
            break;
        case goPlot::Surface:
            {
                myPrivate->dataFilenames.erase ();
                if (myPrivate->plotX.getSize() > 0)
                {
                    if (!goPlot::writeGnuplotDataFiles (&myPrivate->plotX,
                                &myPrivate->plotY,
                                &myPrivate->plotZ,
                                myPrivate->lineLength,
                                myPrivate->dataFilenames))
                    {
                        return false;
                    }
                }
                if (myPrivate->plotMatrixf.getSize() > 0)
                {
                    if (!goPlot::writeGnuplotDataFiles (&myPrivate->plotMatrixf,
                                myPrivate->dataFilenames))
                    {
                        return false;
                    }
                }
                if (myPrivate->plotMatrixd.getSize() > 0)
                {
                    if (!goPlot::writeGnuplotDataFiles (&myPrivate->plotMatrixd,
                                myPrivate->dataFilenames))
                    {
                        return false;
                    }
                }
                if (myPrivate->plotImages.getSize() > 0)
                {
                    if (!goPlot::writeGnuplotDataFiles (&myPrivate->plotImages,
                                myPrivate->dataFilenames))
                    {
                        return false;
                    }
                }
            }
            break;
        default: goLog::error ("goSinglePlot::writeGnuplotDataFiles(): Unknown plot type!"); return false;
    }
    return true;
}

/** 
 * @brief Create all necessary temporary files and create gnuplot command string.
 * 
 * @param plotCommandsRet Contains the command string after the function returned true.
 * 
 * @return True if successful, false otherwise.
 */
bool goSinglePlot::makePlot (goString& plotCommandsRet, bool useDataFiles) const
{
    goString prefix = myPrivate->prefixCommands;
    if (myPrivate->title != "")
    {
        prefix += "set title \"";
        prefix += myPrivate->title;
        prefix += "\"\n";
    }
    else
    {
        prefix += "unset title\n";
    }
    if (!myPrivate->labels.isEmpty())
    {
        assert (!myPrivate->labels.isClosed());
        goList<goPlotterLabel>::Element* el = myPrivate->labels.getFrontElement();
        goSize_t labelTag = 1;
        while (el)
        {
            prefix += "set label ";
            prefix += (int)labelTag;
            prefix += " \"";
            prefix += el->elem.label;
            prefix += "\" at ";
            prefix += (float)el->elem.x;
            prefix += ",";
            prefix += (float)el->elem.y;
            prefix += ",";
            prefix += (float)el->elem.z;
            if (el->elem.colourspec.getSize() > 0)
            {
                prefix += " ";
                prefix += "textcolor ";
                prefix += el->elem.colourspec.toCharPtr();
            }
            prefix += "\n";
            el = el->next;
            ++labelTag;
        }
        // prefix += "replot\n";
    }
    goString postfix = "";

    //= Unset labels
    {
        goIndex_t labelTag = 1;
        for (labelTag = 1; labelTag <= myPrivate->labels.getSize(); ++labelTag)
        {
            postfix += "unset label ";
            postfix += (int)labelTag;
            postfix += "\n";
        }
    }

    if (useDataFiles)
    {
        this->writeGnuplotDataFiles ();
        if (!goPlot::addGnuplotCommands (plotCommandsRet,
                                         &myPrivate->dataFilenames,
                                         0, // &myPrivate->titles,
                                         &myPrivate->plotCommands,
                                         prefix.toCharPtr(),
                                         postfix.toCharPtr(),
                                         myPrivate->plotType))
        {
            return false;
        }
    }
    else
    {
        plotCommandsRet += prefix;
        this->addGnuplotCommands (plotCommandsRet);
        plotCommandsRet += postfix;
    }
    return true;
}

bool goSinglePlot::addGnuplotCommands (goString& plotCommandsRet) const
{
    goList<goAutoPtr<goPlotElement> >::Element* el = myPrivate->plotElements.getFrontElement();
    switch (myPrivate->plotType)
    {
        case goPlot::Normal: plotCommandsRet += "plot "; break;
        case goPlot::Surface: plotCommandsRet += "splot "; break;
        default: goLog::warning ("goSinglePlot::addGnuplotCommands(): Unknown plot type."); return false; break;
    }
    while (el)
    {
        if (this->getUseFiles())
        {
            goString dataString;
            el->elem->data (dataString);
            goString filename;
            FILE* f = goFileIO::createTempFile (filename);
            el->elem->setFilename (filename);
            goFileIO::writeASCII (f, dataString);
            fclose (f);
        }
        plotCommandsRet += "\"";
        plotCommandsRet += el->elem->filename();
        plotCommandsRet += "\" "; 
        plotCommandsRet += el->elem->plotOptions();
        if (el->next)
            plotCommandsRet += ",";
        else
            plotCommandsRet += "\n";
        el = el->next;
    }
    el = myPrivate->plotElements.getFrontElement();
    if (!this->getUseFiles())
    {
        while (el)
        {
            el->elem->data(plotCommandsRet);
            plotCommandsRet += "e\n";
            el = el->next;
        }
    }
    return true; 
}

/** 
 * @brief Removes temporary files.
 *
 * This is only safe to use when the gnuplot command has returned.
 * goMultiPlotter copies the single plots, so the user will usually not have to deal with this.
 * If the plot is not paused for user input after plotting (goMultiPlotter::setPauseFlag()), 
 * the files are not deleted automatically and are left for a system service to clean (usually in /tmp).
 */
void goSinglePlot::removeFiles () const
{
    goList<goAutoPtr<goPlotElement> >::Element* el = myPrivate->plotElements.getFrontElement();

    while (el)
    {
        goFileIO::remove (el->elem->filename());
        el = el->next;
    }

//    goList<goString>::ConstElement* el = myPrivate->dataFilenames.getFrontElement();
//    while (el)
//    {
//        goFileIO::remove (el->elem);
//        el = el->next;
//    }
}

/** 
 * @brief Clears all curves.
 */
void goSinglePlot::clear ()
{
    myPrivate->plotElements.erase ();
    myPrivate->title = "";

    myPrivate->plotX.erase ();
    myPrivate->plotY.erase ();
    myPrivate->plotZ.erase ();
    myPrivate->plotMatrixf.erase ();
    myPrivate->plotMatrixd.erase ();
    myPrivate->plotImageMatrixf.erase ();
    myPrivate->plotImageMatrixd.erase ();
    myPrivate->plotImages.erase ();
    myPrivate->titles.erase ();
    myPrivate->labels.erase ();
    myPrivate->plotCommands.erase ();
    myPrivate->prefixCommands = "";
    myPrivate->dataFilenames.erase ();
}


void goSinglePlot::setUseFiles (bool f)
{
    myPrivate->useFiles = f;
}

bool goSinglePlot::getUseFiles () const
{
    return myPrivate->useFiles;
}
