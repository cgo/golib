/****************************************************************************
** Form implementation generated from reading ui file 'rendergui.ui'
**
** Created: Sun Dec 16 19:22:20 2001
**      by:  The User Interface Compiler (uic)
**
** WARNING! All changes made in this file will be lost!
****************************************************************************/
#include "rendergui.h"

#include <qcheckbox.h>
#include <qframe.h>
#include <qgroupbox.h>
#include <qlabel.h>
#include <qlineedit.h>
#include <qpushbutton.h>
#include <qspinbox.h>
#include <qtabwidget.h>
#include <qtextbrowser.h>
#include <qtoolbutton.h>
#include <qlayout.h>
#include <qvariant.h>
#include <qtooltip.h>
#include <qwhatsthis.h>

/* 
 *  Constructs a RenderGUI which is a child of 'parent', with the 
 *  name 'name' and widget flags set to 'f' 
 */
RenderGUI::RenderGUI( QWidget* parent,  const char* name, WFlags fl )
    : QWidget( parent, name, fl )
{
    if ( !name )
	setName( "RenderDemo" );
    resize( 277, 479 ); 
    QFont f( font() );
    f.setFamily( "adobe-times" );
    setFont( f ); 
    setCaption( tr( "RenderDemo" ) );

    renderButton = new QPushButton( this, "renderButton" );
    renderButton->setGeometry( QRect( 30, 20, 83, 35 ) ); 
    QFont renderButton_font(  renderButton->font() );
    renderButton->setFont( renderButton_font ); 
    renderButton->setText( tr( "Render" ) );
    renderButton->setAccel( 4194304 );

    optionsTab = new QTabWidget( this, "optionsTab" );
    optionsTab->setGeometry( QRect( 30, 90, 230, 360 ) ); 
    QFont optionsTab_font(  optionsTab->font() );
    optionsTab->setFont( optionsTab_font ); 
    optionsTab->setTabShape( QTabWidget::Triangular );

    tab = new QWidget( optionsTab, "tab" );

    optionsBox1 = new QGroupBox( tab, "optionsBox1" );
    optionsBox1->setGeometry( QRect( 10, 0, 210, 110 ) ); 
    optionsBox1->setTitle( tr( "View Position" ) );

    TextLabel1 = new QLabel( optionsBox1, "TextLabel1" );
    TextLabel1->setGeometry( QRect( 10, 20, 16, 20 ) ); 
    TextLabel1->setText( tr( "X" ) );

    TextLabel1_2 = new QLabel( optionsBox1, "TextLabel1_2" );
    TextLabel1_2->setGeometry( QRect( 10, 50, 16, 20 ) ); 
    TextLabel1_2->setText( tr( "Y" ) );

    TextLabel1_3 = new QLabel( optionsBox1, "TextLabel1_3" );
    TextLabel1_3->setGeometry( QRect( 10, 80, 16, 20 ) ); 
    TextLabel1_3->setText( tr( "Z" ) );

    viewPosBoxY = new QSpinBox( optionsBox1, "viewPosBoxY" );
    viewPosBoxY->setGeometry( QRect( 30, 50, 56, 23 ) ); 
    viewPosBoxY->setMaxValue( 999999999 );
    viewPosBoxY->setMinValue( -999999999 );
    viewPosBoxY->setLineStep( 25 );

    viewPosBoxX = new QSpinBox( optionsBox1, "viewPosBoxX" );
    viewPosBoxX->setGeometry( QRect( 30, 20, 56, 23 ) ); 
    viewPosBoxX->setMaxValue( 999999999 );
    viewPosBoxX->setMinValue( -999999999 );
    viewPosBoxX->setLineStep( 25 );

    viewPosBoxZ = new QSpinBox( optionsBox1, "viewPosBoxZ" );
    viewPosBoxZ->setGeometry( QRect( 30, 80, 56, 23 ) ); 
    viewPosBoxZ->setMaxValue( 999999999 );
    viewPosBoxZ->setMinValue( -999999999 );
    viewPosBoxZ->setLineStep( 25 );
    viewPosBoxZ->setValue( -256 );

    GroupBox4 = new QGroupBox( tab, "GroupBox4" );
    GroupBox4->setGeometry( QRect( 10, 110, 210, 50 ) ); 
    GroupBox4->setTitle( tr( "View Depth" ) );

    viewDepthBox = new QSpinBox( GroupBox4, "viewDepthBox" );
    viewDepthBox->setGeometry( QRect( 30, 20, 70, 25 ) ); 
    viewDepthBox->setMaxValue( 99999999 );
    viewDepthBox->setMinValue( 0 );
    viewDepthBox->setLineStep( 32 );
    viewDepthBox->setValue( 32 );

    GroupBox5 = new QGroupBox( tab, "GroupBox5" );
    GroupBox5->setGeometry( QRect( 10, 160, 210, 50 ) ); 
    GroupBox5->setTitle( tr( "Eye Distance" ) );

    eyeDistanceBox = new QSpinBox( GroupBox5, "eyeDistanceBox" );
    eyeDistanceBox->setGeometry( QRect( 30, 20, 70, 25 ) ); 
    eyeDistanceBox->setMaxValue( 99999999 );
    eyeDistanceBox->setMinValue( 1 );
    eyeDistanceBox->setLineStep( 100 );
    eyeDistanceBox->setValue( 500 );
    optionsTab->insertTab( tab, tr( "View" ) );

    tab_2 = new QWidget( optionsTab, "tab_2" );

    GroupBox10 = new QGroupBox( tab_2, "GroupBox10" );
    GroupBox10->setGeometry( QRect( 10, 110, 210, 150 ) ); 
    GroupBox10->setTitle( tr( "Info" ) );

    TextBrowser1 = new QTextBrowser( GroupBox10, "TextBrowser1" );
    TextBrowser1->setGeometry( QRect( 10, 20, 190, 120 ) ); 
    TextBrowser1->setText( tr( "Translation without \nan input device \nis restricted.\nUse these keys \nto translate:\n-------------\nW - forward\nS - backward\nA - left\nD - right" ) );

    GroupBox2 = new QGroupBox( tab_2, "GroupBox2" );
    GroupBox2->setGeometry( QRect( 10, 0, 210, 110 ) ); 
    GroupBox2->setTitle( tr( "Rotation" ) );

    xRotateButton = new QToolButton( GroupBox2, "xRotateButton" );
    xRotateButton->setGeometry( QRect( 80, 20, 20, 21 ) ); 
    xRotateButton->setText( tr( "Go" ) );

    yRotateButton = new QToolButton( GroupBox2, "yRotateButton" );
    yRotateButton->setGeometry( QRect( 80, 50, 20, 21 ) ); 
    yRotateButton->setText( tr( "Go" ) );

    zRotateButton = new QToolButton( GroupBox2, "zRotateButton" );
    zRotateButton->setGeometry( QRect( 80, 80, 20, 21 ) ); 
    zRotateButton->setText( tr( "Go" ) );

    TextLabel1_4 = new QLabel( GroupBox2, "TextLabel1_4" );
    TextLabel1_4->setGeometry( QRect( 10, 20, 16, 20 ) ); 
    TextLabel1_4->setText( tr( "X" ) );

    TextLabel1_4_2 = new QLabel( GroupBox2, "TextLabel1_4_2" );
    TextLabel1_4_2->setGeometry( QRect( 10, 50, 16, 20 ) ); 
    TextLabel1_4_2->setText( tr( "Y" ) );

    TextLabel1_4_2_2 = new QLabel( GroupBox2, "TextLabel1_4_2_2" );
    TextLabel1_4_2_2->setGeometry( QRect( 10, 80, 16, 20 ) ); 
    TextLabel1_4_2_2->setText( tr( "Z" ) );

    xRot = new QSpinBox( GroupBox2, "xRot" );
    xRot->setGeometry( QRect( 30, 20, 40, 23 ) ); 
    xRot->setMaxValue( 360 );
    xRot->setMinValue( -360 );
    xRot->setValue( 2 );

    yRot = new QSpinBox( GroupBox2, "yRot" );
    yRot->setGeometry( QRect( 30, 50, 40, 23 ) ); 
    yRot->setMaxValue( 360 );
    yRot->setMinValue( -360 );
    yRot->setValue( 2 );

    zRot = new QSpinBox( GroupBox2, "zRot" );
    zRot->setGeometry( QRect( 30, 80, 40, 23 ) ); 
    zRot->setMaxValue( 360 );
    zRot->setMinValue( -360 );
    zRot->setValue( 2 );
    optionsTab->insertTab( tab_2, tr( "Motion" ) );

    tab_3 = new QWidget( optionsTab, "tab_3" );

    GroupBox13 = new QGroupBox( tab_3, "GroupBox13" );
    GroupBox13->setGeometry( QRect( 10, 0, 210, 80 ) ); 
    GroupBox13->setTitle( tr( "Sample Distance" ) );

    sampleDistanceEditY = new QLineEdit( GroupBox13, "sampleDistanceEditY" );
    sampleDistanceEditY->setGeometry( QRect( 50, 20, 30, 25 ) ); 
    sampleDistanceEditY->setText( tr( "1.0" ) );

    sampleDistanceEditZ = new QLineEdit( GroupBox13, "sampleDistanceEditZ" );
    sampleDistanceEditZ->setGeometry( QRect( 90, 20, 30, 25 ) ); 
    sampleDistanceEditZ->setText( tr( "1.0" ) );

    sampleDistanceEditX = new QLineEdit( GroupBox13, "sampleDistanceEditX" );
    sampleDistanceEditX->setGeometry( QRect( 10, 20, 30, 25 ) ); 
    sampleDistanceEditX->setText( tr( "1.0" ) );

    sampleDistanceChangeButton = new QPushButton( GroupBox13, "sampleDistanceChangeButton" );
    sampleDistanceChangeButton->setGeometry( QRect( 10, 50, 80, 26 ) ); 
    sampleDistanceChangeButton->setText( tr( "Change" ) );

    GroupBox9_2 = new QGroupBox( tab_3, "GroupBox9_2" );
    GroupBox9_2->setGeometry( QRect( 10, 80, 210, 70 ) ); 
    GroupBox9_2->setTitle( tr( "Maximal Resolution" ) );

    maxResBox = new QSpinBox( GroupBox9_2, "maxResBox" );
    maxResBox->setGeometry( QRect( 10, 20, 56, 23 ) ); 
    maxResBox->setMaxValue( 0 );
    QToolTip::add(  maxResBox, tr( "Maximal loaded resolution (stage)" ) );

    Line1 = new QFrame( GroupBox9_2, "Line1" );
    Line1->setGeometry( QRect( 80, 20, 20, 40 ) ); 
    Line1->setFrameStyle( QFrame::VLine | QFrame::Sunken );

    zoomCheckBox = new QCheckBox( GroupBox9_2, "zoomCheckBox" );
    zoomCheckBox->setGeometry( QRect( 120, 20, 70, 19 ) ); 
    zoomCheckBox->setText( tr( "Zoom" ) );
    zoomCheckBox->setChecked( TRUE );
    zoomCheckBox->setTristate( FALSE );
    QToolTip::add(  zoomCheckBox, tr( "Zoom in when maximal resolution is lowered" ) );

    sameSizeCheckBox = new QCheckBox( GroupBox9_2, "sameSizeCheckBox" );
    sameSizeCheckBox->setGeometry( QRect( 120, 40, 80, 19 ) ); 
    sameSizeCheckBox->setText( tr( "SameSize" ) );
    sameSizeCheckBox->setChecked( TRUE );
    sameSizeCheckBox->setTristate( FALSE );
    QToolTip::add(  sameSizeCheckBox, tr( "Regardless of zoom, keep the same view size" ) );

    GroupBox9 = new QGroupBox( tab_3, "GroupBox9" );
    GroupBox9->setGeometry( QRect( 10, 200, 210, 60 ) ); 
    GroupBox9->setTitle( tr( "Grey Values" ) );

    greyValuesOptButton = new QPushButton( GroupBox9, "greyValuesOptButton" );
    greyValuesOptButton->setGeometry( QRect( 10, 30, 80, 26 ) ); 
    greyValuesOptButton->setText( tr( "Opt" ) );

    GroupBox9_3 = new QGroupBox( tab_3, "GroupBox9_3" );
    GroupBox9_3->setGeometry( QRect( 10, 150, 211, 51 ) ); 
    GroupBox9_3->setTitle( tr( "Memory Usage" ) );

    memoryButton = new QPushButton( GroupBox9_3, "memoryButton" );
    memoryButton->setGeometry( QRect( 11, 21, 80, 26 ) ); 
    memoryButton->setText( tr( "Update" ) );

    memoryLabel = new QLabel( GroupBox9_3, "memoryLabel" );
    memoryLabel->setGeometry( QRect( 110, 20, 90, 26 ) ); 
    memoryLabel->setText( tr( "0" ) );
    optionsTab->insertTab( tab_3, tr( "Volume/Image" ) );
}

/*  
 *  Destroys the object and frees any allocated resources
 */
RenderGUI::~RenderGUI()
{
    // no need to delete child widgets, Qt does it all for us
}

/*  
 *  Main event handler. Reimplemented to handle application
 *  font changes
 */
bool RenderGUI::event( QEvent* ev )
{
    bool ret = QWidget::event( ev ); 
    if ( ev->type() == QEvent::ApplicationFontChange ) {
	QFont renderButton_font(  renderButton->font() );
	renderButton->setFont( renderButton_font ); 
	QFont optionsTab_font(  optionsTab->font() );
	optionsTab->setFont( optionsTab_font ); 
    }
    return ret;
}

