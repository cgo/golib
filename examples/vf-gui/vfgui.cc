/****************************************************************************
** Form implementation generated from reading ui file 'vfgui.ui'
**
** Created: Wed Jan 16 12:59:02 2002
**      by:  The User Interface Compiler (uic)
**
** WARNING! All changes made in this file will be lost!
****************************************************************************/
#include "vfgui.h"

#include <qbuttongroup.h>
#include <qcheckbox.h>
#include <qcombobox.h>
#include <qgroupbox.h>
#include <qlabel.h>
#include <qlineedit.h>
#include <qpushbutton.h>
#include <qradiobutton.h>
#include <qspinbox.h>
#include <qlayout.h>
#include <qvariant.h>
#include <qtooltip.h>
#include <qwhatsthis.h>

/* 
 *  Constructs a VFGUI which is a child of 'parent', with the 
 *  name 'name' and widget flags set to 'f' 
 */
VFGUI::VFGUI( QWidget* parent,  const char* name, WFlags fl )
    : QWidget( parent, name, fl )
{
    if ( !name )
	setName( "VFGUI" );
    resize( 584, 480 ); 
    setCaption( tr( "Volumefile Tool" ) );

    convertButton = new QPushButton( this, "convertButton" );
    convertButton->setGeometry( QRect( 20, 380, 120, 50 ) ); 
    convertButton->setText( tr( "Convert" ) );

    quitButton = new QPushButton( this, "quitButton" );
    quitButton->setGeometry( QRect( 230, 380, 109, 50 ) ); 
    quitButton->setText( tr( "Quit" ) );

    GroupBox4 = new QGroupBox( this, "GroupBox4" );
    GroupBox4->setGeometry( QRect( 20, 130, 500, 90 ) ); 
    GroupBox4->setTitle( tr( "Size" ) );

    QWidget* privateLayoutWidget = new QWidget( GroupBox4, "Layout6" );
    privateLayoutWidget->setGeometry( QRect( 10, 19, 396, 54 ) ); 
    Layout6 = new QGridLayout( privateLayoutWidget ); 
    Layout6->setSpacing( 6 );
    Layout6->setMargin( 0 );

    blockSizeBox1 = new QSpinBox( privateLayoutWidget, "blockSizeBox1" );
    blockSizeBox1->setMaxValue( 999999999 );
    blockSizeBox1->setLineStep( 1 );
    blockSizeBox1->setValue( 32 );

    Layout6->addWidget( blockSizeBox1, 1, 1 );

    TextLabel3_3 = new QLabel( privateLayoutWidget, "TextLabel3_3" );
    TextLabel3_3->setText( tr( "X" ) );

    Layout6->addWidget( TextLabel3_3, 0, 4 );

    TextLabel3 = new QLabel( privateLayoutWidget, "TextLabel3" );
    TextLabel3->setText( tr( "X" ) );

    Layout6->addWidget( TextLabel3, 0, 2 );

    TextLabel3_2 = new QLabel( privateLayoutWidget, "TextLabel3_2" );
    TextLabel3_2->setText( tr( "X" ) );

    Layout6->addWidget( TextLabel3_2, 1, 2 );

    volSizeBox2 = new QSpinBox( privateLayoutWidget, "volSizeBox2" );
    volSizeBox2->setMaxValue( 999999999 );
    volSizeBox2->setValue( 256 );

    Layout6->addWidget( volSizeBox2, 0, 3 );

    volSizeBox1 = new QSpinBox( privateLayoutWidget, "volSizeBox1" );
    volSizeBox1->setMaxValue( 999999999 );
    volSizeBox1->setValue( 256 );

    Layout6->addWidget( volSizeBox1, 0, 1 );

    blockSizeBox3 = new QSpinBox( privateLayoutWidget, "blockSizeBox3" );
    blockSizeBox3->setMaxValue( 999999999 );
    blockSizeBox3->setValue( 32 );

    Layout6->addWidget( blockSizeBox3, 1, 5 );

    TextLabel3_3_2 = new QLabel( privateLayoutWidget, "TextLabel3_3_2" );
    TextLabel3_3_2->setText( tr( "X" ) );

    Layout6->addWidget( TextLabel3_3_2, 1, 4 );

    TextLabel4_2 = new QLabel( privateLayoutWidget, "TextLabel4_2" );
    TextLabel4_2->setText( tr( "Block Size" ) );
    QToolTip::add(  TextLabel4_2, tr( "Make sure the block size is a power of two!" ) );

    Layout6->addWidget( TextLabel4_2, 1, 0 );

    volSizeBox3 = new QSpinBox( privateLayoutWidget, "volSizeBox3" );
    volSizeBox3->setMaxValue( 999999999 );
    volSizeBox3->setValue( 256 );

    Layout6->addWidget( volSizeBox3, 0, 5 );

    TextLabel4 = new QLabel( privateLayoutWidget, "TextLabel4" );
    TextLabel4->setText( tr( "Volume Size" ) );
    QToolTip::add(  TextLabel4, tr( "Enter the exact volume size" ) );

    Layout6->addWidget( TextLabel4, 0, 0 );

    blockSizeBox2 = new QSpinBox( privateLayoutWidget, "blockSizeBox2" );
    blockSizeBox2->setMaxValue( 999999999 );
    blockSizeBox2->setValue( 32 );

    Layout6->addWidget( blockSizeBox2, 1, 3 );

    GroupBox5 = new QGroupBox( this, "GroupBox5" );
    GroupBox5->setGeometry( QRect( 20, 310, 500, 60 ) ); 
    GroupBox5->setTitle( tr( "Type" ) );

    fileTypeBox = new QComboBox( FALSE, GroupBox5, "fileTypeBox" );
    fileTypeBox->insertItem( tr( "blockwise" ) );
    fileTypeBox->insertItem( tr( "bandwise" ) );
    fileTypeBox->setGeometry( QRect( 180, 20, 88, 22 ) ); 
    fileTypeBox->setCurrentItem( 1 );

    dataTypeBox = new QComboBox( FALSE, GroupBox5, "dataTypeBox" );
    dataTypeBox->insertItem( tr( "int8" ) );
    dataTypeBox->insertItem( tr( "uint8" ) );
    dataTypeBox->insertItem( tr( "int16" ) );
    dataTypeBox->insertItem( tr( "uint16" ) );
    dataTypeBox->insertItem( tr( "int32" ) );
    dataTypeBox->insertItem( tr( "uint32" ) );
    dataTypeBox->insertItem( tr( "float" ) );
    dataTypeBox->insertItem( tr( "double" ) );
    dataTypeBox->setGeometry( QRect( 40, 20, 88, 22 ) ); 
    dataTypeBox->setCurrentItem( 2 );

    GroupBox2 = new QGroupBox( this, "GroupBox2" );
    GroupBox2->setGeometry( QRect( 10, 0, 500, 108 ) ); 
    GroupBox2->setTitle( tr( "Filenames" ) );

    QWidget* privateLayoutWidget_2 = new QWidget( GroupBox2, "Layout1" );
    privateLayoutWidget_2->setGeometry( QRect( 11, 21, 297, 28 ) ); 
    Layout1 = new QGridLayout( privateLayoutWidget_2 ); 
    Layout1->setSpacing( 6 );
    Layout1->setMargin( 0 );

    rawDataEdit = new QLineEdit( privateLayoutWidget_2, "rawDataEdit" );

    Layout1->addWidget( rawDataEdit, 0, 1 );

    TextLabel1 = new QLabel( privateLayoutWidget_2, "TextLabel1" );
    TextLabel1->setText( tr( "Raw Data" ) );
    QToolTip::add(  TextLabel1, tr( "Enter the name of the raw volume data file or a text file containing the paths of JPEG images" ) );

    Layout1->addWidget( TextLabel1, 0, 0 );

    fileBrowseButton1 = new QPushButton( privateLayoutWidget_2, "fileBrowseButton1" );
    fileBrowseButton1->setText( tr( "Browse" ) );

    Layout1->addWidget( fileBrowseButton1, 0, 2 );

    ButtonGroup1 = new QButtonGroup( GroupBox2, "ButtonGroup1" );
    ButtonGroup1->setGeometry( QRect( 314, 21, 171, 70 ) ); 
    ButtonGroup1->setTitle( tr( "Raw data file is..." ) );

    textFileButton = new QRadioButton( ButtonGroup1, "textFileButton" );
    textFileButton->setGeometry( QRect( 11, 46, 149, 19 ) ); 
    textFileButton->setText( tr( "Text file w/ jpeg names" ) );
    textFileButton->setChecked( FALSE );

    rawDataButton = new QRadioButton( ButtonGroup1, "rawDataButton" );
    rawDataButton->setGeometry( QRect( 11, 21, 149, 19 ) ); 
    rawDataButton->setText( tr( "Raw data" ) );
    rawDataButton->setChecked( TRUE );

    QWidget* privateLayoutWidget_3 = new QWidget( GroupBox2, "Layout2" );
    privateLayoutWidget_3->setGeometry( QRect( 10, 60, 297, 28 ) ); 
    Layout2 = new QGridLayout( privateLayoutWidget_3 ); 
    Layout2->setSpacing( 6 );
    Layout2->setMargin( 0 );

    fileBrowseButton2 = new QPushButton( privateLayoutWidget_3, "fileBrowseButton2" );
    fileBrowseButton2->setText( tr( "Browse" ) );

    Layout2->addWidget( fileBrowseButton2, 0, 2 );

    TextLabel2 = new QLabel( privateLayoutWidget_3, "TextLabel2" );
    TextLabel2->setText( tr( "Transformed File" ) );
    QToolTip::add(  TextLabel2, tr( "Enter the name of the dwtv file" ) );

    Layout2->addWidget( TextLabel2, 0, 0 );

    dwtvEdit = new QLineEdit( privateLayoutWidget_3, "dwtvEdit" );

    Layout2->addWidget( dwtvEdit, 0, 1 );

    GroupBox12 = new QGroupBox( this, "GroupBox12" );
    GroupBox12->setGeometry( QRect( 20, 230, 500, 70 ) ); 
    GroupBox12->setTitle( tr( "Properties" ) );

    QWidget* privateLayoutWidget_4 = new QWidget( GroupBox12, "Layout7" );
    privateLayoutWidget_4->setGeometry( QRect( 10, 30, 131, 25 ) ); 
    Layout7 = new QGridLayout( privateLayoutWidget_4 ); 
    Layout7->setSpacing( 6 );
    Layout7->setMargin( 0 );

    stagesBox = new QSpinBox( privateLayoutWidget_4, "stagesBox" );
    stagesBox->setValue( 3 );

    Layout7->addWidget( stagesBox, 0, 1 );

    TextLabel5 = new QLabel( privateLayoutWidget_4, "TextLabel5" );
    TextLabel5->setText( tr( "DWT stages" ) );

    Layout7->addWidget( TextLabel5, 0, 0 );

    QWidget* privateLayoutWidget_5 = new QWidget( GroupBox12, "Layout5" );
    privateLayoutWidget_5->setGeometry( QRect( 170, 20, 259, 46 ) ); 
    Layout5 = new QGridLayout( privateLayoutWidget_5 ); 
    Layout5->setSpacing( 6 );
    Layout5->setMargin( 0 );

    slowLoadingBox = new QCheckBox( privateLayoutWidget_5, "slowLoadingBox" );
    slowLoadingBox->setText( tr( "Use slow loading procedure (low memory)" ) );

    Layout5->addWidget( slowLoadingBox, 1, 0 );

    lineWiseBox = new QCheckBox( privateLayoutWidget_5, "lineWiseBox" );
    lineWiseBox->setText( tr( "Use line-wise conversion (low memory)" ) );
    lineWiseBox->setChecked( TRUE );

    Layout5->addWidget( lineWiseBox, 0, 0 );
}

/*  
 *  Destroys the object and frees any allocated resources
 */
VFGUI::~VFGUI()
{
    // no need to delete child widgets, Qt does it all for us
}

