#include <goresolutionmanager.h>
#include <gotypes.h>

namespace Vol {

goResolutionManager::goResolutionManager ()
{
  stages = 0;
  minResolution = 0;
}

goResolutionManager::~goResolutionManager ()
{
  if (stages)
    delete stages;
}

void
goResolutionManager::init ()
{
  stages = new goNibbleArray(numberOfBlocks);
}

}
