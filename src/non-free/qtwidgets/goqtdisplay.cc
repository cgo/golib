
#include <config.h>
#ifdef HAVE_LIBQT

#include <non-free/qtwidgets/goqtdisplay.h>
#include <qpainter.h>

goQtDisplay::goQtDisplay (QWidget* parent, const char* name, WFlags f)
  : QWidget (parent, name, f) {
  sizeX = 0; sizeY = 0;
}

goQtDisplay::~goQtDisplay () {
}

void
goQtDisplay::displayBuffer () {
  goIndex_t x = 0, y = 0;
  QPainter p;
  p.begin (this);
  for ( y = 0; y < sizeY; y++) {
    for ( x = 0; x < sizeX; x++) {
      p.setPen (QColor ((buffer[x][y] < 256) ? buffer[x][y] : 255, (buffer[x][y] < 256) ? buffer[x][y] : 255, (buffer[x][y] < 256) ? buffer[x][y] : 255 ) );
      p.drawPoint (x,y);
    }
  }
  p.end();
}

void
goQtDisplay::repaint () {
  QWidget::repaint();
  displayBuffer();
}

void
goQtDisplay::update () {
  QWidget::update ();
  displayBuffer();
}

void
goQtDisplay::paintEvent (QPaintEvent* e) {
  QWidget::paintEvent (e);
  displayBuffer();
}
#endif /* USE_QT */
