#include <non-free/qtwidgets/goqtdisplay.h>
#include <defs/gotypes.h>
#include <graphics/goimagebuffer.h>
#include <qapplication.h>

int main (int argc, char* argv[]) {
  int   	x = 0, y = 0;
  QApplication  app (argc,argv);
  goQtDisplay   display;
  goImageBuffer buffer;

  app.setMainWidget (&display);
  display.show();
  display.resize (320,200);

  buffer.create (320,200,GO_INT32);
  for ( y = 0; y < 200; y++) {
    for ( x = 0; x < 320; x++) {
      (((goInt32*)buffer[x])[y]) = (goInt32)x;
    }
  }
  display.setBufferPtr ((int**)buffer.getBufferPtr());
  display.setBufferSize (320,200);
  display.displayBuffer();
  
  return app.exec();
}
