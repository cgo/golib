#ifndef GODEFS_H
#define GODEFS_H

/** 
 * Class IDs
 */
enum {
    GO_OBJECTBASE = 1,
    GO_SIGNAL3DBASE,
    GO_SIGNAL3D,
    GO_SUBSIGNAL3D,
    GO_HAAR3D,
    GO_QUANTIZER,
    GO_ADAPTIVEQUANTIZER,
    GO_UNIFORMQUANTIZER,
    GO_DWT3D,
    GO_FILTER3D,
    GO_HISTOGRAM,
    GO_BIORTHOWAVELET,
    GO_CURVE,
    GO_GAUSSPDF,
    GO_KMEANS,
    GO_KMEANSSPATIAL,
    GO_PDF,
    GO_POINTCLOUD,
    GO_PLOTTER,
    GO_SINGLEPLOT,
    GO_MULTIPLOTTER,
    GO_VIDEOCAPTURE,
    GO_CONNECTION,
    GO_NETOBJECT,
    GO_SERVER,
    GO_PROCESS,
    GO_THREADOBJECT,
    GO_TYPE,
    GO_BTREEELEMENT,
    GO_BTREE,
    GO_ROWVECTOR,
    GO_MATLAB,
    GO_GRAPH,
    GO_GRAPHNODE,
    
    GO_CLASSID_USER = 0x00010000
};

enum {
  GO_DIRECTION,
  GO_DIRECTION_X,
  GO_DIRECTION_Y,
  GO_DIRECTION_Z,
  GO_DIRECTION_X_NEGATIVE,
  GO_DIRECTION_Y_NEGATIVE,
  GO_DIRECTION_Z_NEGATIVE
};

/** 
* Axes or coordinate definitions
*/
enum {
    GO_X = 1,
    GO_Y = 2,
    GO_Z = 4
};

/**
 * Border definitions
 */
enum {
    /** 
     * Periodic borders for goSignal3DBase type objects.
     */
    GO_PERIODIC_BORDER = 1,
    /** 
     * Constant border for goSignal3DBase type objects.
     */
    GO_CONSTANT_BORDER = 2,
    /** 
     * Constant border for goSubSignal3D type objects.
     * Does not work with other goSignal3DBase type objects.
     * THIS IS NOT YET IMPLEMENTED.
     */
    GO_PARENT_BORDER = 3
};
#endif
