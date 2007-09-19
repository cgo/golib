#include <goplot.h>
#include <godefs.h>
#include <gofileio.h>

class goMultiPlotterPrivate
{
    public:
        goMultiPlotterPrivate ()
          : prefixCommands(), shellPostfix(), waitFlag(true), 
            pauseFlag(false), autoPosition(true), barycentric(false),
            barycentricTriangle(2,3), cmdFilename(), 
            plots(), inputFD(-1), outputFD(-1) {};
        ~goMultiPlotterPrivate () {};


        goString               prefixCommands;
        goString               shellPostfix;

        bool              waitFlag;
        bool              pauseFlag;
        bool              autoPosition;
        bool              barycentric;
        goMatrixd         barycentricTriangle;
        goString          cmdFilename;
        goList<goSinglePlot> plots;
        goList<goFloat>   extentRow;
        goList<goFloat>   extentCol;

        goSize_t rows;
        goSize_t columns;

        //= IO redirection. -1 means not used. This is not used, only here for completeness
        //= because one CAN set it to redirect gnuplot's input/output if it is called
        //= via  This is not used, only here for completeness
        //= because one CAN set it to redirect gnuplot's input/output if it is called
        //= via callGnuplot().
        int               inputFD;
        int               outputFD;
};

/** 
 * @brief Constructor.
 * 
 * Constructs a new goMultiPlotter with multiplot grid size (rows x cols).
 * This means a grid with <rows> rows and <cols> columns of goSinglePlot objects can be drawn by
 * the constructed object.
 *
 * @param rows Number of rows.
 * @param cols Number of columns.
 */
goMultiPlotter::goMultiPlotter (goSize_t rows, goSize_t cols)
    : goObjectBase (), myPrivate (0)
{
    this->setClassID (GO_MULTIPLOTTER);
    myPrivate = new goMultiPlotterPrivate;
    myPrivate->rows = rows;
    myPrivate->columns = cols;
}

goMultiPlotter::~goMultiPlotter ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

goMultiPlotter::goMultiPlotter (goMultiPlotter& other)
    : goObjectBase (), myPrivate (0)
{
    this->setClassID (GO_MULTIPLOTTER);
    myPrivate = new goMultiPlotterPrivate;
    *this = other;
}

/** 
 * @brief Set input file descriptor.
 *
 * Used for IO redirection.
 * 
 * @see C library functions dup2(), pipe(), etc.
 *
 * @param fd File descriptor.
 */
void goMultiPlotter::setInputFD (int fd)
{
    myPrivate->inputFD = fd;
}

/** 
 * @brief Set output file descriptor.
 *
 * Used for IO redirection.
 * 
 * @see C library functions dup2(), pipe(), etc.
 *
 * @param fd File descriptor.
 */
void goMultiPlotter::setOutputFD (int fd)
{
    myPrivate->outputFD = fd;
}

goMultiPlotter& goMultiPlotter::operator= (goMultiPlotter& other)
{
    *this->myPrivate = *other.myPrivate;
    return *this;
}

/** 
 * @brief Add a plot at a given position.
 * 
 * Adds a plot to the multiplot grid.
 *
 * @param p     Plot.
 * @param row   Row position.
 * @param col   Column position.
 */
void goMultiPlotter::addPlot (const goSinglePlot& p, goSize_t row, goSize_t col, goFloat extentRow, goFloat extentCol)
{
    if (myPrivate->barycentric)
    {
        goLog::warning ("goMultiPlotter::addPlot(): mp is barycentric, use addPlotBarycentric()");
        return;
    }
    myPrivate->plots.append (p);
    myPrivate->extentRow.append (extentRow);
    myPrivate->extentCol.append (extentCol);
    myPrivate->plots.getTailElement()->elem.setPosition (row, col);
}

void goMultiPlotter::addPlot (const goSinglePlot& p, goSize_t index)
{
    if (myPrivate->barycentric)
    {
        goLog::warning ("goMultiPlotter::addPlot(): mp is barycentric, use addPlotBarycentric()");
        return;
    }
    goSize_t row = index % this->getRows();
    goSize_t col = index / this->getRows();
    myPrivate->plots.append (p);
    myPrivate->extentRow.append (1.0f);
    myPrivate->extentCol.append (1.0f);
    myPrivate->plots.getTailElement()->elem.setPosition (row, col);
}

void goMultiPlotter::addPlotBarycentric (goSinglePlot& p, goDouble u, goDouble v, goDouble w, goDouble xsize, goDouble ysize)
{
    if (!myPrivate->barycentric)
    {
        goLog::warning ("goMultiPlotter::addPlotBarycentric(): put into barycentric mode first by setBarycentric() !");
        return;
    }

    goVectord bary (3);
    goVectord pos (2);
    bary[0] = u; bary[1] = v; bary[2] = w;
    goMath::barycentricToEuclidean (myPrivate->barycentricTriangle, bary, pos);

    goString pref = p.getPrefix ();
    pref += "set origin ";
    pref += (float)pos[0];
    pref += ",";
    pref += (float)pos[1];
    pref += "\nset size ";
    pref += (float)xsize; pref += ",";
    pref += (float)ysize; pref += "\n";
    p.setPrefix (pref);

    myPrivate->plots.append (p);
    myPrivate->extentRow.append (1.0f);
    myPrivate->extentCol.append (1.0f);
    myPrivate->plots.getTailElement()->elem.setPosition (0, 0);
}

/** 
 * @brief Get number of rows in the multiplot grid.
 * 
 * @return Number of rows.
 */
goSize_t goMultiPlotter::getRows () const
{
    return myPrivate->rows;
}

/** 
 * @brief Get number of columns in the multiplot grid.
 * 
 * @return Number of columns.
 */
goSize_t goMultiPlotter::getColumns () const
{
    return myPrivate->columns;
}

/** 
 * @brief Set wait flag (default true).
 * 
 * If true, the caller waits for the gnuplot process.
 *
 * @param w Wait flag.
 */
void goMultiPlotter::setWaitFlag (bool w)
{
    myPrivate->waitFlag = w;
}

/** 
 * @brief Get wait flag.
 * 
 * @return Wait flag.
 */
bool goMultiPlotter::getWaitFlag () const
{
    return myPrivate->waitFlag;
}

/** 
 * @brief Set pause flag.
 *
 * If set to true, the plotter (gnuplot) waits for user input (usually <RET> on the console)
 * until the program is resumed. To be precise, the gnuplot command "pause -1" is added at the end of the
 * gnuplot command.
 * 
 * @param w Pause flag.
 */
void goMultiPlotter::setPauseFlag (bool w)
{
    myPrivate->pauseFlag = w;
}

/** 
 * @brief Get pause flag.
 * 
 * @return Pause flag.
 */
bool goMultiPlotter::getPauseFlag () const
{
    return myPrivate->pauseFlag;
}

void goMultiPlotter::setAutoPosition (bool a)
{
    myPrivate->autoPosition = a;
}

bool goMultiPlotter::getAutoPosition () const
{
    return myPrivate->autoPosition;
}

void goMultiPlotter::setBarycentric (const goMatrixf& triangle)
{
    goVectord row;

    myPrivate->barycentricTriangle.resize (2,3);

    if (triangle.getRows() != myPrivate->barycentricTriangle.getRows() || triangle.getColumns() != myPrivate->barycentricTriangle.getColumns())
    {
        goLog::warning ("goMultiPlotter::setBarycentric(): triangle has size != 2x3");
        return;
    }

    for (goSize_t i = 0; i < 2; ++i)
    {
        myPrivate->barycentricTriangle.refRow (i, row);
        triangle.copyRow (i, row);
    }

    this->setAutoPosition (false);
    myPrivate->barycentric = true;
}

void goMultiPlotter::setBarycentric (const goMatrixd& triangle)
{
    goVectord row;

    myPrivate->barycentricTriangle.resize (2,3);

    if (triangle.getRows() != myPrivate->barycentricTriangle.getRows() || triangle.getColumns() != myPrivate->barycentricTriangle.getColumns())
    {
        goLog::warning ("goMultiPlotter::setBarycentric(): triangle has size != 2x3");
        return;
    }

    for (goSize_t i = 0; i < 2; ++i)
    {
        myPrivate->barycentricTriangle.refRow (i, row);
        triangle.copyRow (i, row);
    }

    this->setAutoPosition (false);
    myPrivate->barycentric = true;
}

/** 
 * @brief Set a gnuplot command prefix.
 * 
 * This string will be at the beginning of the gnuplot command.
 * You can use this e.g. to set things like "unset xtics\nunset ytics\n".
 * @note The string should be ended with a "\n" carriage return.
 *
 * @param p Command string.
 */
void goMultiPlotter::setPrefix (const goString& p)
{
    myPrivate->prefixCommands = p;
}

bool goMultiPlotter::makePlotCommands (goString& plotCommands)
{
    assert (!myPrivate->plots.isClosed());

    goDouble stepX = 1.0 / static_cast<goDouble>(this->getColumns());
    goDouble stepY = -1.0 / static_cast<goDouble>(this->getRows());

    goString prefix = myPrivate->prefixCommands;
    if (myPrivate->rows > 1 || myPrivate->columns > 1 || myPrivate->barycentric)
    {
        prefix += "set multiplot\n";
    }
    else
    {
        prefix += "unset multiplot\n";
    }
    //set size ";
    //prefix += (float)stepX;
    //prefix += ",";
    //prefix += (float)-stepY;
    //prefix += "\n";
    goDouble posX  = 0.0;
    goDouble posY  = 1.0 + stepY;

    plotCommands = prefix;
    goList<goSinglePlot>::Element *el = myPrivate->plots.getFrontElement();
    goList<goFloat>::Element *extentRowEl = myPrivate->extentRow.getFrontElement();
    goList<goFloat>::Element *extentColEl = myPrivate->extentCol.getFrontElement();
    while (el)
    {
        if (this->getAutoPosition())
        {
            plotCommands += "set origin ";
            plotCommands += (float)(posX + el->elem.getColumn() * stepX);
            plotCommands += ",";
            plotCommands += (float)(posY + el->elem.getRow() * stepY);
            plotCommands += "\n";
            plotCommands += "set size ";
            plotCommands += (float)(extentColEl->elem * stepX);
            plotCommands += ",";
            plotCommands += (float)(extentRowEl->elem * -stepY);
            plotCommands += "\n";
        }
        el->elem.makePlot (plotCommands, false);
        el = el->next;
        extentRowEl = extentRowEl->next;
        extentColEl = extentColEl->next;
    }

    return true;
}

/** 
 * @brief Plot all plots.
 * 
 * @param gp goGnuplot object pointer. If not null, all commands are sent to this object. 
 * A command files will then not be written.
 *
 * @return True if successful, false otherwise.
 */
bool goMultiPlotter::plot (goGnuplot* gp)
{
    goString plotCommands;
    if (!this->makePlotCommands (plotCommands))
        return false;

    if (gp)
    {
        goString postfix = "unset multiplot\n";
        plotCommands += postfix;
        return gp->call (plotCommands);
    }
    else
    {
        goString postfix = "unset multiplot\n";
        if (myPrivate->pauseFlag)
        {
            postfix += "pause -1\n";
        }
        plotCommands += postfix;
        bool ok = goPlot::callGnuplot (plotCommands, 
                myPrivate->shellPostfix != "" ? myPrivate->shellPostfix.toCharPtr() : 0, 
                myPrivate->waitFlag, 
                &myPrivate->cmdFilename,
                myPrivate->inputFD,
                myPrivate->outputFD);
        //= If the process was paused, we can delete the files.
        if (myPrivate->pauseFlag && myPrivate->waitFlag)
        {
            this->removeFiles ();
        }
        return ok;
    }
}

/** 
 * @brief Plot to a postscript file.
 * 
 * Same as plotFile (filename, goString("postscript colour"));
 *
 * @param filename Postscript file name.
 * @param sizeX    X size of canvas in cm.
 * @param sizeY    Y size of canvas in cm.
 * 
 * @return True if successful, false otherwise.
 */
bool goMultiPlotter::plotPostscript (const goString& filename, goFloat sizeX, goFloat sizeY)
{
    goString termString = "postscript color enhanced";
    if (sizeX > 0.0 && sizeY > 0.0)
    {
        termString += " size ";
        termString += (float)(sizeX / 2.54f);
        termString += ",";
        termString += (float)(sizeY / 2.54f);
    }
    return this->plotFile (filename, termString);
}

bool goMultiPlotter::plotPostscript (const char* filename, goFloat sizeX, goFloat sizeY)
{
    return this->plotPostscript (goString(filename), sizeX, sizeY);
}

/** 
 * @brief Plot to a postscript file.
 * 
 * Same as plotFile (filename, goString("postscript eps colour"));
 *
 * @param filename Postscript file name.
 * 
 * @return True if successful, false otherwise.
 */
bool goMultiPlotter::plotEPS (const goString& filename)
{
    return this->plotFile (filename, goString("postscript eps colour enhanced "));
}
bool goMultiPlotter::plotEPS (const char* filename)
{
    return this->plotFile (goString(filename), goString("postscript eps colour enhanced"));
}

/** 
 * @brief Plot to a specified output terminal and pipe output to file.
 * 
 * The command "set terminal <type>" is added to the prefix gnuplot command.
 * The plotter (gnuplot) is called through a shell and the shell output is
 * stored in <filename>.
 *
 * All temporary files created by the plot are removed after plotting.
 *
 * @param filename Filename to store the shell output in.
 * @param type     Terminal type (from gnuplot, e.g. "postscript color").
 * 
 * @return True if successful, false otherwise.
 */
bool goMultiPlotter::plotFile (const goString& filename, 
                               const goString& type)
{
    goString backup1 = myPrivate->prefixCommands;
    myPrivate->prefixCommands += "set terminal ";
    myPrivate->prefixCommands += type;
    myPrivate->prefixCommands += "\n";
    goString backup2 = myPrivate->shellPostfix;
    myPrivate->shellPostfix = " > ";
    myPrivate->shellPostfix += filename;
    bool ok = this->plot();
    myPrivate->prefixCommands = backup1;
    myPrivate->shellPostfix = backup2;
    this->removeFiles();
    return ok;
}
bool goMultiPlotter::plotFile (const char* filename, 
                               const char* type)
{
    return this->plotFile (goString(filename), goString(type));
}

bool goMultiPlotter::saveGnuplot (const char* filename)
{
    goString cmd;
    this->makePlotCommands (cmd);
    return goFileIO::writeASCII (filename, cmd);
}

/** 
 * @brief Clears all commands and plots and resets flags to default values.
 */
void goMultiPlotter::clear ()
{
    myPrivate->prefixCommands = "";
    myPrivate->shellPostfix = "";
    myPrivate->waitFlag = true;
    myPrivate->pauseFlag = false;
    myPrivate->autoPosition = true;
    myPrivate->barycentric = false;
    myPrivate->cmdFilename = "";
    myPrivate->plots.erase ();
    myPrivate->extentCol.erase ();
    myPrivate->extentRow.erase ();
}

/** 
 * @brief Remove all data and command files.
 */
void goMultiPlotter::removeFiles ()
{
    goList<goSinglePlot>::Element *el = myPrivate->plots.getFrontElement();
    while (el)
    {
        el->elem.removeFiles();
        el = el->next;
    }
    goFileIO::remove (myPrivate->cmdFilename);
}

//#ifndef GOLIST_HPP
//# include <golist.hpp>
//#endif
//template class goList<goSinglePlot>;
