#ifndef NAVIGATIONFRAME_H
#define NAVIGATIONFRAME_H

#include <qframe.h>

class
NavigationFrame : public QFrame 
{
    Q_OBJECT;
 public:
    NavigationFrame (QWidget* parent = 0, 
		     const char* name = 0, 
		     WFlags fl = 0 );
    virtual ~NavigationFrame();

 signals:
    // right,up direction
    void moveSignal (int,int);
    // horizontal,vertical
    void rotateSignal (int,int);
    
 protected:
    void mousePressEvent (QMouseEvent *e);
    void mouseReleaseEvent (QMouseEvent *e);
    void mouseMoveEvent (QMouseEvent *e);

 private:
    bool moving;
    bool rotating;
    QPoint *lastMoving;
    QPoint *lastRotating;
};c

#endif
