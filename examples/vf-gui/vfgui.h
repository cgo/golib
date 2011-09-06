/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


/****************************************************************************
** Form interface generated from reading ui file 'vfgui.ui'
**
** Created: Wed Jan 16 12:59:01 2002
**      by:  The User Interface Compiler (uic)
**
** WARNING! All changes made in this file will be lost!
****************************************************************************/
#ifndef VFGUI_H
#define VFGUI_H

#include <qvariant.h>
#include <qwidget.h>
class QVBoxLayout; 
class QHBoxLayout; 
class QGridLayout; 
class QButtonGroup;
class QCheckBox;
class QComboBox;
class QGroupBox;
class QLabel;
class QLineEdit;
class QPushButton;
class QRadioButton;
class QSpinBox;

class VFGUI : public QWidget
{ 
    Q_OBJECT

public:
    VFGUI( QWidget* parent = 0, const char* name = 0, WFlags fl = 0 );
    ~VFGUI();

    QPushButton* convertButton;
    QPushButton* quitButton;
    QGroupBox* GroupBox4;
    QSpinBox* blockSizeBox1;
    QLabel* TextLabel3_3;
    QLabel* TextLabel3;
    QLabel* TextLabel3_2;
    QSpinBox* volSizeBox2;
    QSpinBox* volSizeBox1;
    QSpinBox* blockSizeBox3;
    QLabel* TextLabel3_3_2;
    QLabel* TextLabel4_2;
    QSpinBox* volSizeBox3;
    QLabel* TextLabel4;
    QSpinBox* blockSizeBox2;
    QGroupBox* GroupBox5;
    QComboBox* fileTypeBox;
    QComboBox* dataTypeBox;
    QGroupBox* GroupBox2;
    QLineEdit* rawDataEdit;
    QLabel* TextLabel1;
    QPushButton* fileBrowseButton1;
    QButtonGroup* ButtonGroup1;
    QRadioButton* textFileButton;
    QRadioButton* rawDataButton;
    QPushButton* fileBrowseButton2;
    QLabel* TextLabel2;
    QLineEdit* dwtvEdit;
    QGroupBox* GroupBox12;
    QSpinBox* stagesBox;
    QLabel* TextLabel5;
    QCheckBox* slowLoadingBox;
    QCheckBox* lineWiseBox;

protected:
    QGridLayout* Layout6;
    QGridLayout* Layout1;
    QGridLayout* Layout2;
    QGridLayout* Layout7;
    QGridLayout* Layout5;
};

#endif // VFGUI_H
