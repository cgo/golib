#include <goplot.h>
#include <godefs.h>
#include <gofileio.h>

class goMultiPlotterPrivate
{
    public:
        goMultiPlotterPrivate ()
          : prefixCommands(), shellPostfix(), waitFlag(true), 
            pauseFlag(false), cmdFilename(), 
            plots(), inputFD(-1), outputFD(-1) {};
        ~goMultiPlotterPrivate () {};


        goString               prefixCommands;
        goString               shellPostfix;

        bool              waitFlag;
        bool              pauseFlag;
        goString          cmdFilename;
        goList<goSinglePlot> plots;
        goList<goFloat>   extentRow;
        goList<goFloat>   extentCol;

        goSize_t rows;
        goSize_t columns;

        //= IO redirection. -1 means not used.
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
    myPrivate->plots.append (p);
    myPrivate->extentRow.append (extentRow);
    myPrivate->extentCol.append (extentCol);
    myPrivate->plots.getTailElement()->elem.setPosition (row, col);
}

void goMultiPlotter::addPlot (const goSinglePlot& p, goSize_t index)
{
    goSize_t row = index % this->getRows();
    goSize_t col = index / this->getRows();
    myPrivate->plots.append (p);
    myPrivate->extentRow.append (1.0f);
    myPrivate->extentCol.append (1.0f);
    myPrivate->plots.getTailElement()->elem.setPosition (row, col);
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

/** 
 * @brief Plot all plots.
 * 
 * @return True if successful, false otherwise.
 */
bool goMultiPlotter::plot ()
{
    assert (!myPrivate->plots.isClosed());

    goDouble stepX = 1.0 / static_cast<goDouble>(this->getColumns());
    goDouble stepY = -1.0 / static_cast<goDouble>(this->getRows());

    goString prefix = myPrivate->prefixCommands;
    prefix += "set multiplot\n";
    //set size ";
    //prefix += (float)stepX;
    //prefix += ",";
    //prefix += (float)-stepY;
    //prefix += "\n";
    goDouble posX  = 0.0;
    goDouble posY  = 1.0 + stepY;

    goString plotCommands = prefix;
    goList<goSinglePlot>::Element *el = myPrivate->plots.getFrontElement();
    goList<goFloat>::Element *extentRowEl = myPrivate->extentRow.getFrontElement();
    goList<goFloat>::Element *extentColEl = myPrivate->extentCol.getFrontElement();
    while (el)
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
        el->elem.makePlot (plotCommands);
        el = el->next;
        extentRowEl = extentRowEl->next;
        extentColEl = extentColEl->next;
    }
    
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
        el = myPrivate->plots.getFrontElement();
        while (el)
        {
            el->elem.removeFiles();
            el = el->next;
        }
        goFileIO::remove (myPrivate->cmdFilename);
    }

    return ok;
}

/** 
 * @brief Plot to a postscript file.
 * 
 * Same as plotFile (filename, goString("postscript colour"));
 *
 * @param filename Postscript file name.
 * 
 * @return True if successful, false otherwise.
 */
bool goMultiPlotter::plotPostscript (const goString& filename)
{
    return this->plotFile (filename, goString("postscript colour"));
}

bool goMultiPlotter::plotPostscript (const char* filename)
{
    return this->plotFile (goString(filename), goString("postscript colour"));
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
    return this->plotFile (filename, goString("postscript eps colour"));
}
bool goMultiPlotter::plotEPS (const char* filename)
{
    return this->plotFile (goString(filename), goString("postscript eps colour"));
}

/** 
 * @brief Plot to a specified output terminal and pipe output to file.
 * 
 * The command "set terminal <type>" is added to the prefix gnuplot command.
 * The plotter (gnuplot) is called through a shell and the shell output is
 * stored in <filename>.
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
    return ok;
}
bool goMultiPlotter::plotFile (const char* filename, 
                               const char* type)
{
    return this->plotFile (goString(filename), goString(type));
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
    myPrivate->cmdFilename = "";
    myPrivate->plots.erase ();
    myPrivate->extentCol.erase ();
    myPrivate->extentRow.erase ();
}

//#ifndef GOLIST_HPP
//# include <golist.hpp>
//#endif
//template class goList<goSinglePlot>;
