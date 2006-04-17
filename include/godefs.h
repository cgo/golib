#ifndef GODEFS_H
#define GODEFS_H

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
    GO_PERIODIC_BORDER = 1,
    GO_CONSTANT_BORDER = 2
};
#endif
