#include <goimagebuffer.h>
#include <stdlib.h>

#define ALLOC_IMAGE_BUFFER(type,x,y,count) { \
                buffer = (void**)malloc (sizeof(type*) * x); \
                for (count = 0; count < x; count++) { \
                  (buffer[count]) = (void*)malloc (sizeof(type) * y); \
                } \
          }

goImageBuffer::goImageBuffer (int x, int y, GO_TYPE t) {
  goIndex_t i = 0;
  sizeX = x;
  sizeY = y;
  type  = t;


  switch (type) {
  case GO_INT8: ALLOC_IMAGE_BUFFER(goInt8, x, y, i); break;
  case GO_INT16: ALLOC_IMAGE_BUFFER(goInt16, x, y, i); break;
  case GO_INT32: ALLOC_IMAGE_BUFFER(goInt32, x, y, i); break;
  case GO_UINT8: ALLOC_IMAGE_BUFFER(goUInt8, x, y, i); break;
  case GO_UINT16: ALLOC_IMAGE_BUFFER(goUInt16, x, y, i); break;
  case GO_UINT32: ALLOC_IMAGE_BUFFER(goUInt32, x, y, i); break;
  case GO_FLOAT: ALLOC_IMAGE_BUFFER(goFloat, x, y, i); break;
  case GO_DOUBLE: ALLOC_IMAGE_BUFFER(goDouble, x, y, i); break;
  default: ALLOC_IMAGE_BUFFER(goInt8, x, y, i); break;
  }

  status.created = true;
}

goImageBuffer::goImageBuffer () {
  sizeX = 0;
  sizeY = 0;
  status.created = false;
}

goImageBuffer::~goImageBuffer () {
  goIndex_t i = 0;
  for (i = 0; i < sizeX; i++) {
    free (buffer[i]);
  }
  free (buffer);
}

void
goImageBuffer::create (int x, int y, GO_TYPE t) {
  goIndex_t i = 0;

  if (!status.created) {
    sizeX = x;
    sizeY = y;
    type  = t;

    switch (type) {
    case GO_INT8: ALLOC_IMAGE_BUFFER(goInt8, x, y, i); break;
    case GO_INT16: ALLOC_IMAGE_BUFFER(goInt16, x, y, i); break;
    case GO_INT32: ALLOC_IMAGE_BUFFER(goInt32, x, y, i); break;
    case GO_UINT8: ALLOC_IMAGE_BUFFER(goUInt8, x, y, i); break;
    case GO_UINT16: ALLOC_IMAGE_BUFFER(goUInt16, x, y, i); break;
    case GO_UINT32: ALLOC_IMAGE_BUFFER(goUInt32, x, y, i); break;
    case GO_FLOAT: ALLOC_IMAGE_BUFFER(goFloat, x, y, i); break;
    case GO_DOUBLE: ALLOC_IMAGE_BUFFER(goDouble, x, y, i); break;
    default: ALLOC_IMAGE_BUFFER(goInt8, x, y, i); break;
    }

    status.created = true;
  }
}

