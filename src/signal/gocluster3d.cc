#include <gocluster3d.h>
#include <gosignalmacros.h>
#include <goerror.h>

namespace Vol {

template<class T>
goCluster3D<T>::goCluster3D() 
{
  blocks.resize(0);
}

template<class T>
goCluster3D<T>::~goCluster3D()
{
  int i;
  goSignal3D<T> *p;
  for (i = 0; i < blocks.getSize(); i++)
    {
      p = (goSignal3D<T>*)blocks[i];
      p->destroy();
      delete p;
    }
  blocks.resize(0);
}

template<class T>
void
goCluster3D<T>::setSize(goSize_t sx, goSize_t sy, goSize_t sz)
{
  xSize = sx;
  ySize = sy;
  zSize = sz;
}

template<class T>
void
goCluster3D<T>::setBlockSize(goSize_t sx, goSize_t sy, goSize_t sz)
{
  xBlockSize = sx;
  yBlockSize = sy;
  zBlockSize = sz;

  xBlockSizeLog = 0;
  goSize_t temp = sx;
  temp >>= 1;
  while (temp > 0)
    {
      xBlockSizeLog++;
      temp >>= 1;
    }
  yBlockSizeLog = 0;
  temp = sy;
  temp >>= 1;
  while (temp > 0)
    {
      yBlockSizeLog++;
      temp >>= 1;
    }
  zBlockSizeLog = 0;
  temp = sz;
  temp >>= 1;
  while (temp > 0)
    {
      zBlockSizeLog++;
      temp >>= 1;
    }
  
  goError::note("goCluster3D","");
  cout << "\txBlockSizeLog = " << xBlockSizeLog << endl << \
    "\tyBlockSizeLog = " << yBlockSizeLog << endl << \
    "\tzBlockSizeLog = " << zBlockSizeLog << endl;
  
}

template<class T>
void
goCluster3D<T>::read (const char *filename)
{
  cout << "goCluster3D::read() not yet implemented" << endl;
}

template<class T>
void
goCluster3D<T>::write (const char *filename)
{
  cout << "goCluster3D::write() not yet implemented" << endl;
}

template<class T>
goSize_t
goCluster3D<T>::getNumberOfBlocks () 
{
  return blocks.getSize();
}

template<class T>
void
goCluster3D<T>::update()
{
  xBlocks = xSize >> xBlockSizeLog;
  if ( (xSize & ( (1 << xBlockSizeLog) - 1 )) != 0 )
    {
      xBlocks++;
    }
  yBlocks = ySize >> yBlockSizeLog;
  if ( (ySize & ( (1 << yBlockSizeLog) - 1 )) != 0 )
    {
      yBlocks++;
    }
  zBlocks = zSize >> zBlockSizeLog;
  if ( (zSize & ( (1 << zBlockSizeLog) - 1 )) != 0 )
    {
      zBlocks++;
    }
  goError::note("goCluster3D::update()","");
  cout << "\txBlocks = " << xBlocks << endl;
  cout << "\tyBlocks = " << yBlocks << endl;
  cout << "\tzBlocks = " << zBlocks << endl;
}

template<class T>
void
goCluster3D<T>::addBlock (goSignal3D<T>& b)
{
  goSignal3D<T> *newBlock = new goSignal3D<T>;
  newBlock->make (&b);
  GO_SIGNAL3D_EACHELEMENT_2(*__ptr_target = *__ptr, b, (*newBlock), T, T);
  this->addBlockPtr (newBlock);
}

template<class T>
void
goCluster3D<T>::addBlockPtr (goSignal3D<T> *b)
{
  void *ptr = (void*)b;
  blocks += ptr;
}

}

template class Vol::goCluster3D<goInt8>;
template class Vol::goCluster3D<goUInt8>;
template class Vol::goCluster3D<goInt16>;
template class Vol::goCluster3D<goUInt16>;
template class Vol::goCluster3D<goInt32>;
template class Vol::goCluster3D<goUInt32>;
template class Vol::goCluster3D<goFloat>;
template class Vol::goCluster3D<goDouble>;


