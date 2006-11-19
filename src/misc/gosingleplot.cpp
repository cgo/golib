#include <goplot.h>
#include <gofileio.h>

class goSinglePlotPrivate
{
    public:
        goSinglePlotPrivate() 
            : plotX(), 
              plotY(), 
              plotZ(),
              plotMatrixf(),
              plotMatrixd(),
              plotImages(),
              lineLength(0),
              titles(), 
              plotCommands(), 
              prefixCommands(""), 
              dataFilenames(),
              row(0),
              column(0),
              title(""),
              plotType(goPlot::Normal)
        {};
        ~goSinglePlotPrivate() {};

        goList<goVectord>      plotX;
        goList<goVectord>      plotY;
        goList<goVectord>      plotZ;
        goList<goMatrixf>      plotMatrixf;
        goList<goMatrixd>      plotMatrixd;
        goList<const goSignal3DBase<void>*> plotImages;
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

bool goSinglePlot::add3D (const goVectord& x, const goVectord& y,
                          goIndex_t lineLength,
                          const goVectord& values, 
                          const char* title, 
                          const char* plotOptions)
{
    if (myPrivate->plotType != goPlot::Surface)
    {
        this->clear ();
        myPrivate->plotType = goPlot::Surface;
    }
    myPrivate->plotX.append (x);
    myPrivate->plotY.append (y);
    myPrivate->plotZ.append (values);
    myPrivate->lineLength = lineLength;

    myPrivate->titles.append(goString(title));
    if (!plotOptions)
    {
        myPrivate->plotCommands.append(goString("w l"));
    }
    else
    {
        myPrivate->plotCommands.append(goString(plotOptions));
    }
    return true;
}

bool goSinglePlot::add3D (const goMatrixf& m, const char* title, const char* plotOptions)
{
    if (myPrivate->plotType != goPlot::Surface)
    {
        this->clear ();
        myPrivate->plotType = goPlot::Surface;
    }
    myPrivate->plotMatrixf.append (m);

    myPrivate->titles.append(goString(title));
    if (!plotOptions)
    {
        myPrivate->plotCommands.append(goString("w l"));
    }
    else
    {
        myPrivate->plotCommands.append(goString(plotOptions));
    }
    return true;
}

bool goSinglePlot::add3D (const goMatrixd& m, const char* title, const char* plotOptions)
{
    if (myPrivate->plotType != goPlot::Surface)
    {
        this->clear ();
        myPrivate->plotType = goPlot::Surface;
    }
    myPrivate->plotMatrixd.append (m);

    myPrivate->titles.append(goString(title));
    if (!plotOptions)
    {
        myPrivate->plotCommands.append(goString("w l"));
    }
    else
    {
        myPrivate->plotCommands.append(goString(plotOptions));
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

    myPrivate->titles.append(goString(title));
    if (!plotOptions)
    {
        myPrivate->plotCommands.append(goString("w l"));
    }
    else
    {
        myPrivate->plotCommands.append(goString(plotOptions));
    }
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
    if (!plotOptions)
    {
        myPrivate->plotCommands.append(goString("w l"));
    }
    else
    {
        myPrivate->plotCommands.append(goString(plotOptions));
    }
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
    if (!plotOptions)
    {
        myPrivate->plotCommands.append(goString("w l"));
    }
    else
    {
        myPrivate->plotCommands.append(goString(plotOptions));
    }
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
bool goSinglePlot::addLabel (const goString& l, goDouble x, goDouble y)
{
    return myPrivate->labels.append (goPlotterLabel(l.toCharPtr(), x, y));
}

void goSinglePlot::setTitle (const goString& s)
{
    myPrivate->title = s;
}

/** 
 * @brief Create all necessary temporary files and create gnuplot command string.
 * 
 * @param plotCommandsRet Contains the command string after the function returned true.
 * 
 * @return True if successful, false otherwise.
 */
bool goSinglePlot::makePlot (goString& plotCommandsRet) const
{
    goString prefix = myPrivate->prefixCommands;
    if (myPrivate->title != "")
    {
        prefix += "set title \"";
        prefix += myPrivate->title;
        prefix += "\"\n";
    }
    if (!myPrivate->labels.isEmpty())
    {
        assert (!myPrivate->labels.isClosed());
        goList<goPlotterLabel>::Element* el = myPrivate->labels.getFrontElement();
        goSize_t labelTag = 0;
        while (el)
        {
            prefix += "set label";
            prefix += (int)labelTag;
            prefix += " \"";
            prefix += el->elem.label;
            prefix += "\" at ";
            prefix += (float)el->elem.x;
            prefix += ",";
            prefix += (float)el->elem.y;
            prefix += "\n";
            el = el->next;
            ++labelTag;
        }
        // prefix += "replot\n";
    }
    goString postfix = "";

    //= Unset labels
    {
        goIndex_t labelTag = 0;
        for (labelTag = 0; labelTag < myPrivate->labels.getSize(); ++labelTag)
        {
            postfix += "unset label ";
            postfix += (int)labelTag;
            postfix += "\n";
        }
    }

    switch (myPrivate->plotType)
    {
        case goPlot::Normal:
            {
                if (!goPlot::writeGnuplotDataFiles (&myPrivate->plotX,
                            &myPrivate->plotY,
                            myPrivate->dataFilenames))
                {
                    return false;
                }
            }
            break;
        case goPlot::Surface:
            {
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
        default: goLog::error ("goSinglePlot::makePlot(): Unknown plot type!"); return false;
    }

    if (!goPlot::addGnuplotCommands (plotCommandsRet,
                                     &myPrivate->dataFilenames,
                                     &myPrivate->titles,
                                     &myPrivate->plotCommands,
                                     prefix.toCharPtr(),
                                     postfix.toCharPtr(),
                                     myPrivate->plotType))
    {
        return false;
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
    goList<goString>::ConstElement* el = myPrivate->dataFilenames.getFrontElement();
    while (el)
    {
        goFileIO::remove (el->elem);
        el = el->next;
    }
}

/** 
 * @brief Clears all curves.
 */
void goSinglePlot::clear ()
{
    myPrivate->plotX.erase ();
    myPrivate->plotY.erase ();
    myPrivate->plotZ.erase ();
    myPrivate->plotMatrixf.erase ();
    myPrivate->plotMatrixd.erase ();
    myPrivate->plotImages.erase ();
    myPrivate->titles.erase ();
    myPrivate->labels.erase ();
    myPrivate->plotCommands.erase ();
    myPrivate->prefixCommands = "";
    myPrivate->dataFilenames.erase ();
}
