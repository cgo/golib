/****************************************************************************
** Form interface generated from reading ui file 'rendergui.ui'
**
** Created: Sun Dec 16 19:22:20 2001
**      by:  The User Interface Compiler (uic)
**
** WARNING! All changes made in this file will be lost!
****************************************************************************/
#ifndef RENDERDEMO_H
#define RENDERDEMO_H

#include <qvariant.h>
#include <qwidget.h>
class QVBoxLayout; 
class QHBoxLayout; 
class QGridLayout; 
class QCheckBox;
class QFrame;
class QGroupBox;
class QLabel;
class QLineEdit;
class QPushButton;
class QSpinBox;
class QTabWidget;
class QTextBrowser;
class QToolButton;

class RenderGUI : public QWidget
{ 
    Q_OBJECT

public:
    RenderGUI( QWidget* parent = 0, const char* name = 0, WFlags fl = 0 );
    ~RenderGUI();

    QPushButton* renderButton;
    QTabWidget* optionsTab;
    QWidget* tab;
    QGroupBox* optionsBox1;
    QLabel* TextLabel1;
    QLabel* TextLabel1_2;
    QLabel* TextLabel1_3;
    QSpinBox* viewPosBoxY;
    QSpinBox* viewPosBoxX;
    QSpinBox* viewPosBoxZ;
    QGroupBox* GroupBox4;
    QSpinBox* viewDepthBox;
    QGroupBox* GroupBox5;
    QSpinBox* eyeDistanceBox;
    QWidget* tab_2;
    QGroupBox* GroupBox10;
    QTextBrowser* TextBrowser1;
    QGroupBox* GroupBox2;
    QToolButton* xRotateButton;
    QToolButton* yRotateButton;
    QToolButton* zRotateButton;
    QLabel* TextLabel1_4;
    QLabel* TextLabel1_4_2;
    QLabel* TextLabel1_4_2_2;
    QSpinBox* xRot;
    QSpinBox* yRot;
    QSpinBox* zRot;
    QWidget* tab_3;
    QGroupBox* GroupBox13;
    QLineEdit* sampleDistanceEditY;
    QLineEdit* sampleDistanceEditZ;
    QLineEdit* sampleDistanceEditX;
    QPushButton* sampleDistanceChangeButton;
    QGroupBox* GroupBox9_2;
    QSpinBox* maxResBox;
    QFrame* Line1;
    QCheckBox* zoomCheckBox;
    QCheckBox* sameSizeCheckBox;
    QGroupBox* GroupBox9;
    QPushButton* greyValuesOptButton;
    QGroupBox* GroupBox9_3;
    QPushButton* memoryButton;
    QLabel* memoryLabel;

protected:
    bool event( QEvent* );
};

#endif // RENDERDEMO_H
