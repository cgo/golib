#include <govolumefile.h>
#include <iostream.h>

namespace Vol {

ostream& operator<< (ostream& o, goVolumeFileInfo& info)
{
  o << "Version:" << info.version << "\n";
  o << "Blocksize: " << info.blockSize.x << "," << info.blockSize.y << "," << info.blockSize.z << "\n";
  o << "Size: " << info.size.x << "," << info.size.y << "," << info.size.z << "\n";
  o << "Blocks: " << info.blocks.x << "," << info.blocks.y << "," << info.blocks.z << "\n";
  o << "Stages: " << info.stages << "\n";
  o << "Energy: " << info.energy << "\n";
  o << "Mean: " << info.mean << "\n";
  o << "Minimum: " << info.minimum << "\n";
  o << "Maximum: " << info.maximum << "\n";
  o << "File structure: "; 
  if (info.fileType == 0)
  {
	o << "Blockwise\n";
  } else
  {
	  o << "Bandwise\n";
  }
	  
  return o;
}

};
