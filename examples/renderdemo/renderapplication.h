#ifndef RENDERAPPLICATION_H
#define RENDERAPPLICATION_H

#include <rendergui.h>
#include <transferfunctionframe.h>
//#include <posdisplay.h>
// #include <navigationframe.h>
#include <govolumenavigator.h>
#include <gostatusobject.h>
#include <gothread.h>
#include <qvariant.h>
#include <qwidget.h>
#include <qprogressbar.h>
#include <gojoystick.h>
#include <gonavkeyboard.h>

class RenderApplication : public RenderGUI, public goStatusObject
{
    Q_OBJECT

 public:
    RenderApplication( QWidget* parent = 0, 
		       const char* name = 0, 
		       WFlags fl = 0 );
    virtual ~RenderApplication();
	
	/// Loads most widgets with default values
	void loadDefaults();
	/// Sets most values in the navigator object
	void setAllValues();
    /*!
     * Is called in the constructor.
     * Initialized the screen image size.
     * @param w Width of screen image in pixels
     * @param h Height of screen image in pixels
     */
    void setImageSize (int w, int h);

   // MUST be set by another object/function (RenderMainWindow)
    QProgressBar	   *progressBar;
    QImage		   *image;
    Vol::goVolumeNavigator *navigator;
    // PosDisplay *posDisp;

	friend void* render_progress_thread (void*);
 public slots:
   void setVolumeFileName();
   void saveImage ();
   void render ();
   void changeViewPos();
   void rotateViewPosX();
   void rotateViewPosY();
   void rotateViewPosZ();

   void viewPosXChange (int p);
   void viewPosYChange (int p);
   void viewPosZChange (int p);

   void changeViewDepth (int d);
   void changeEyeDistance (int d);

   void changeSampleDistance();
   void optGreyValues();

   void updateMemory();

   void showOpacityTF();
   void showDensityTF();
   void showGreyTF();

   void setMinMaxGreyDetermination (bool);

   void changeGreyTF();
   void changeOpacityTF();
   void changeDensityTF();

   void changeMaxResolution(int);
   void changeZoom(bool);			// zoom checkbox
   void changeSameSize(bool);		// sameSize checkbox
   /*
    * widget slots:
    */ 
   void paintEvent(QPaintEvent*);

   void moveForward();
   void moveBackward();
   void moveLeft();
   void moveRight();
 protected:
   void keyPressEvent (QKeyEvent *e);

   goMutex updateMutex;  // Protects the repaintEvent
 private:
   
   /// Flags
   bool viewPosChanged;
   bool imageValid;

   /// Transfer function widgets
   TransferFunctionFrame *opacityTFFrame;
   TransferFunctionFrame *densityTFFrame;
   TransferFunctionFrame *greyTFFrame;
   
   bool setMinMaxGrey;

   // Thread that updates the progress bar when appropriate.
   goThread progressThread;
   bool     progressThreadRunning;
	
   // goNavDevice Joystick	
   Vol::goJoystick *joystick;
   // goNavDevice Keyboard
   Vol::goNavKeyboard *keyboard;

};

#endif

