#include <goglobal.h>
#include <godefs.h>

bool goGlobal::ILInitialized = false;
goGlobal::goClassNameTable goGlobal::classNames;

#ifdef GO_USE_CLASSNAMES
goGlobal::goClassNameTable::goClassNameTable ()
    : goHashTable<int,goString>(7)
{
    this->setDefault (goString("[NO NAME FOR CLASS ID]"));
    this->add (GO_OBJECTBASE         ,goString("goObjectBase"));
    this->add (GO_SIGNAL3DBASE       ,goString("goSignal3DBase"));
    this->add (GO_SIGNAL3D           ,goString("goSignal3D"));
    this->add (GO_SUBSIGNAL3D        ,goString("goSubSignal3D"));
    this->add (GO_HAAR3D             ,goString("goHaar3D"));
    this->add (GO_QUANTIZER          ,goString("goQuantizer"));
    this->add (GO_ADAPTIVEQUANTIZER  ,goString("goAdaptiveQuantizer"));
    this->add (GO_UNIFORMQUANTIZER   ,goString("goUniformQuantizer"));
    this->add (GO_DWT3D              ,goString("goDWT3D"));
    this->add (GO_FILTER3D           ,goString("goFilter3D"));
    this->add (GO_HISTOGRAM          ,goString("goHistogram"));
    this->add (GO_BIORTHOWAVELET     ,goString("goBiorthoWavelet"));
    this->add (GO_CURVE              ,goString("goCurve"));
    this->add (GO_GAUSSPDF           ,goString("goGaussPDF"));
    this->add (GO_KMEANS             ,goString("goKMeans"));
    this->add (GO_KMEANSSPATIAL      ,goString("goKMeansSpatial"));
    this->add (GO_PDF                ,goString("goPDF"));
    this->add (GO_POINTCLOUD         ,goString("goPointCloud"));
    this->add (GO_PLOTTER            ,goString("goPlotter"));
    this->add (GO_MULTIPLOTTER       ,goString("goMultiPlotter"));
    this->add (GO_SINGLEPLOT         ,goString("goSinglePlot"));
    this->add (GO_VIDEOCAPTURE       ,goString("goVideoCapture"));
    this->add (GO_CONNECTION         ,goString("goConnection"));
    this->add (GO_NETOBJECT          ,goString("goNetObject"));
    this->add (GO_SERVER             ,goString("goServer"));
    this->add (GO_PROCESS            ,goString("goProcess"));
    this->add (GO_THREADOBJECT       ,goString("goThreadObject"));
    this->add (GO_TYPE               ,goString("goType"));
    this->add (GO_BTREEELEMENT       ,goString("goBTreeElement"));
    this->add (GO_BTREE              ,goString("goBTree"));
    this->add (GO_ROWVECTOR          ,goString("goRowVector"));
    this->add (GO_MATLAB             ,goString("goMatlab"));
}
#else
goGlobal::goClassNameTable::goClassNameTable()
{
}

goString& goGlobal::goClassNameTable::operator[] (int)
{
    static goString dummy = "NO CLASS NAME SUPPORT";
    goLog::warning("goGlobal::goClassNameTable: class name support was not compiled into golib. Use classID instead!");
    return dummy;
}
#endif

goGlobal::goClassNameTable::~goClassNameTable()
{
}
