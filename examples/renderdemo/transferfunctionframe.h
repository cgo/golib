#ifndef TRANSFERFUNCTIONFRAME_H
#define TRANSFERFUNCTIONFRAME_H

#include <qframe.h>
#include <qpopupmenu.h>
#include <qpoint.h>
#include <goarray.h>
#include <govol.h>

class
TransferFunctionFrame : public QFrame
{
    Q_OBJECT;
 public:
    TransferFunctionFrame(QWidget * parent=0, const char * name=0, WFlags f=0, bool b = TRUE);
    virtual ~TransferFunctionFrame();

    void setYMax (volFloat m) { yMax = m; }
    void setYMin (volFloat m) { yMin = m; }
    void setXMax (volFloat m) { xMax = m; }
    void setXMin (volFloat m) { xMin = m; }
    
    goArray<volFloat> *getPointsX();
    goArray<volFloat> *getPointsY();

 public slots:
     void openPopup ();
 void addPoint ();
 void removePoint();

 signals:
 void functionChanged();

 protected:
 void mouseMoveEvent (QMouseEvent *e);
 void mousePressEvent (QMouseEvent *e);
 void mouseReleaseEvent (QMouseEvent *e);
 void paintEvent (QPaintEvent *e);
 void resizeEvent (QResizeEvent *e);

 void sortPoints();
 void rescalePoints (int w, int h, int newW, int newH);
 private:
    double xMax;
    double yMax;
    double xMin;
    double yMin;
    goArray<int> pointsX;
    goArray<int> pointsY;
    goArray<volFloat> __pointsX;
    goArray<volFloat> __pointsY;

    QPoint *closestPoint;
    QPoint *lastClickedPoint;
    QPopupMenu *popupMenu;

    bool isDraggingPoint;
    int  draggingPoint;
};

#endif
