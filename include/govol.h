#ifndef GOVOL_H
#define GOVOL_H

#include <gotypes.h>

typedef goFloat volFloat;

namespace Vol {
///
enum GO_ROTATION_AXIS {
	GO_ROTATION_X,
	GO_ROTATION_Y,
	GO_ROTATION_Z
};
///
enum GO_TF_TYPE {
    GO_TF_GREY,
    GO_TF_DENSITY,
    GO_TF_OPACITY
};

enum GO_VOLUMEFILE_TYPE {
	GO_VOLUMEFILE_BLOCKWISE,
	GO_VOLUMEFILE_BANDWISE,
	GO_VOLUMEFILE_UNKNOWN
};

/// Flags for the behaviour of the Vol subsystem
enum GO_BEHAVIOUR {
	/// No behaviour flags are set
	GO_BEHAVIOUR_NONE = 0,
	/// Zoom images if appropriate
	GO_BEHAVIOUR_ZOOM = 1,
	/*!
	 * View size is dependent on the resolution: On lower resolutions, load more blocks
	 * since more are visible. Generally makes only sense without GO_BEHAVIOUR_ZOOM,
	 * but can be set independently.
	 */
    GO_BEHAVIOUR_RES_DEPENDENT_VIEW = 2,
	GO_BEHAVIOUR_MOTION_PREDICTION = 4,
	GO_BEHAVIOUR_DEFAULT = GO_BEHAVIOUR_ZOOM | GO_BEHAVIOUR_MOTION_PREDICTION
};

static unsigned int VolBehaviour = GO_BEHAVIOUR_DEFAULT;   // flags of types GO_BEHAVIOUR
/*!
 * Sets the behaviour of the Vol subsystem.
 * For details, see the enum Vol::GO_BEHAVIOUR definition in the file include/govol.h.
 */
void setVolBehaviour(unsigned int);
/*!
 * @return The behaviour flags of the Vol subsystem
 * @see enum Vol::GO_BEHAVIOUR
 */
unsigned int getVolBehaviour();
/*!
 * @return An identifier for the type of data contained in a transformed file.
 * The values' meanings:
 * - 0: int8
 * - 1: uint8
 * - 2: int16
 * - 3: uint16
 * - 4: int32
 * - 5: uint32
 * - 6: float
 * - 7: double
 */
goInt32 DWTVGetDataType(const char* filename); 

};


#endif
