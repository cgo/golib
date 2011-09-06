/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <renderapplication.h>

/* Qt */
#include <qstring.h>
#include <qfiledialog.h>
#include <qimage.h>
#include <qpainter.h>
#include <qspinbox.h>
#include <qcolor.h>		// qRgb
#include <qpushbutton.h>
#include <qmessagebox.h>
#include <qtoolbutton.h>
#include <qcheckbox.h>
#include <qlabel.h>

/* libGo */
#include <gostring.h>
#include <gotypes.h>
#include <goexception.h>
// #include <gothread.h>
#include <gomath.h>		// MAX
#include <govolumenavigator.h>
#include <govolumefile.h>

#include <iostream>
#include <math.h>		// M_PI


/**********************************************
 *
 * SDL part
 * 
 **********************************************/
#include <SDL.h>

/*
 * SDL part for displaying the rendered images. The SDL joystick interface is initialized here, too.
 */
namespace goSDL {

    SDL_Surface *surface;
    int width;
    int height;
	int bytesPP;
    
    void initVideo (int w, int h, int bpp)
    {
		if( (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_JOYSTICK) == -1) ) 
	    { 
			printf("Could not initialize SDL: %s.\n", SDL_GetError());
			exit(-1);
   	 	}
		surface = SDL_SetVideoMode (w, h, bpp, SDL_HWSURFACE|SDL_DOUBLEBUF);
		if (surface == NULL)
   	 	{
			cout << "SDL fucked up setting the video mode." << endl;
			exit(-1);
	    }
		width = w;
		height = h;
		bytesPP = surface->format->BytesPerPixel;
   	 }
    
    void drawImage (goSignal2D<goUInt32>* image)
    {
		cout << "Entered drawImage" << endl;
		Uint8 *p;
		Uint8  *px;
		p = (Uint8*)surface->pixels;
		SDL_LockSurface (surface);
		int x,y;
		for (y = 0; y < height; y++)
		{
			px = p;
			for (x = 0; x < width; x++)
		   	 {
				*px = (Uint8)*image->getPtr(x,y);
				*(px + 1) = *px;
				*(px + 2) = *px;
				px += bytesPP;
			 }
			p += surface->pitch;
		}
		SDL_UnlockSurface (surface);
		SDL_Flip(surface);
   	 }
    
    void finishVideo ()
    {
		SDL_Quit();
    }
};

/*
 * Waits for the RenderApplication's progress indicator to reach 100%, then displays an image.
 */
void*
render_progress_thread (void* p)
{
    cout << "*********************************************************" << endl;
    cout << "********** progress_thread started **********************" << endl;
    RenderApplication *ra = (RenderApplication*)p;
    goThread::setCancelType (0);
    while (true)
	{
		goFloat p;
	    p = ra->waitForProgress();
	    // cout << ra->getProgress() << endl;
	    // ra->progressBar->setProgress ((int)(ra->progressBar->totalSteps() * ra->getProgress()));
	    // ra->progressBar->update();
	    if (p == 1.0f)
		{
		    // ra->update();
		    // ra->setProgress(0);
		    //ra->progressBar->reset();
		    goSignal2D<goUInt32> *signal;
		    try {
				signal = ra->navigator->getFinalImage();
		    }
		    catch (goException& e)
			{
			    e.print();
			    return 0;
			}
			ra->updateMutex.lock();
		    goSDL::drawImage (signal);
			// ra->posDisp->updateGL();
			ra->updateMutex.unlock();
		}
	}
    return 0;
}

RenderApplication::RenderApplication ( QWidget* parent, 
				       const char* name, WFlags fl)
    : RenderGUI (parent, name, fl), goStatusObject()
{
    
    this->setFocusPolicy (StrongFocus);

  navigator = new Vol::goVolumeNavigator;
  // From an HP paper (UltraVis)
  setImageSize (256,256);   // initializes SDL, so must be called before joystick is initialized.
  joystick = new Vol::goJoystick;
  joystick->connect(navigator);
  joystick->run();
  keyboard = new Vol::goNavKeyboard;
  keyboard->connect(navigator);
  keyboard->run();
  opacityTFFrame = new TransferFunctionFrame ();
  densityTFFrame = new TransferFunctionFrame ();
  greyTFFrame = new TransferFunctionFrame ();
  opacityTFFrame->setCaption ("Opacity");
  densityTFFrame->setCaption ("Density");
  greyTFFrame->setCaption ("Image Grey Values");
  
  opacityTFFrame->hide();
  densityTFFrame->hide();
  greyTFFrame->hide();
  opacityTFFrame->resize (255,100);
  densityTFFrame->resize (255,100);
  greyTFFrame->resize (255,100);

  opacityTFFrame->setXMin (0.0f);
  opacityTFFrame->setYMin (0.0f);
  opacityTFFrame->setXMax (1.0f);
  opacityTFFrame->setYMax (1.0f);

  imageValid = false;
  renderButton->setEnabled (false);
  sampleDistanceChangeButton->setEnabled (false);

  setMinMaxGreyDetermination (true);
  
//   QObject::connect ((const QObject*)fileButton, SIGNAL(clicked()), 
// 		    (const QObject*)this, SLOT(setVolumeFileName()));
  QObject::connect ((const QObject*)renderButton, SIGNAL(clicked()), 
		    (const QObject*)this, SLOT(render()));
  /*
   * View position buttons
   */
  QObject::connect ((const QObject*)viewPosBoxX, SIGNAL(valueChanged(int)),
		    (const QObject*)this, SLOT(viewPosXChange(int)));
  QObject::connect ((const QObject*)viewPosBoxY, SIGNAL(valueChanged(int)),
		    (const QObject*)this, SLOT(viewPosYChange(int)));
  QObject::connect ((const QObject*)viewPosBoxZ, SIGNAL(valueChanged(int)),
		    (const QObject*)this, SLOT(viewPosZChange(int)));
  /*
   * Rotation buttons
   */
  QObject::connect ((const QObject*)xRotateButton, SIGNAL(clicked()),
		    (const QObject*)this, SLOT(rotateViewPosX()));
  QObject::connect ((const QObject*)yRotateButton, SIGNAL(clicked()),
		    (const QObject*)this, SLOT(rotateViewPosY()));
  QObject::connect ((const QObject*)zRotateButton, SIGNAL(clicked()),
		    (const QObject*)this, SLOT(rotateViewPosZ()));
  /*
   * view depth, eye distance, and sample distance
   */
  QObject::connect ((const QObject*)viewDepthBox, SIGNAL(valueChanged(int)),
		    (const QObject*)this, SLOT(changeViewDepth(int)));
  QObject::connect ((const QObject*)eyeDistanceBox, SIGNAL(valueChanged(int)),
		    (const QObject*)this, SLOT(changeEyeDistance(int)));
  QObject::connect ((const QObject*)sampleDistanceChangeButton, SIGNAL(clicked()),
		    (const QObject*)this, SLOT(changeSampleDistance()));
  QObject::connect ((const QObject*)greyValuesOptButton, SIGNAL(clicked()),
		    (const QObject*)this, SLOT(optGreyValues()));

  QObject::connect ((const QObject*)maxResBox, SIGNAL(valueChanged(int)),
		    (const QObject*)this, SLOT(changeMaxResolution(int)));
  
  /* Zoom */
  QObject::connect ((const QObject*)zoomCheckBox, SIGNAL(toggled(bool)),
		    (const QObject*)this, SLOT(changeZoom(bool)));
  /* Keep same size */
  QObject::connect ((const QObject*)sameSizeCheckBox, SIGNAL(toggled(bool)),
		    (const QObject*)this, SLOT(changeSameSize(bool)));
  
  QObject::connect ((const QObject*)memoryButton, SIGNAL(clicked()),
		    (const QObject*)this, SLOT(updateMemory()));
	

  /*
   * Transfer function buttons
   * Done by a higher level main widget.
   */ 
//   QObject::connect ((const QObject*)opacityButton, SIGNAL(clicked()),
// 		    (const QObject*)this, SLOT(showOpacityTF()));
//   QObject::connect ((const QObject*)densityButton, SIGNAL(clicked()),
// 		    (const QObject*)this, SLOT(showDensityTF()));
//   QObject::connect ((const QObject*)greyButton, SIGNAL(clicked()),
// 		    (const QObject*)this, SLOT(showGreyTF()));

  /*
   * Transfer function changes
   */
  QObject::connect ((const QObject*)greyTFFrame, SIGNAL(functionChanged()),
		    (const QObject*)this, SLOT(changeGreyTF()));
  QObject::connect ((const QObject*)opacityTFFrame, SIGNAL(functionChanged()),
		    (const QObject*)this, SLOT(changeOpacityTF()));
  QObject::connect ((const QObject*)densityTFFrame, SIGNAL(functionChanged()),
		    (const QObject*)this, SLOT(changeDensityTF()));

  /*
   * Create a thread to update the progress bar and the widget when a render process is done.
   * Update: Qt widgets can not handle async SIGNALs, so progress doen't work.
   */
  forwardProgress (navigator);	// forward the progress from the volume navigator
  progressThreadRunning = false;

  image = new QImage (256,256,32);

  changeGreyTF ();	// Initialize grey transfer function 

  changeZoom(true);

  // There was no time left to make the position display work.
  // It works by itself, but neither GLUT nor Qt seem to be capable of
  // handling update requests from a different thread than the main thread.
  //posDisp = new PosDisplay();
  //posDisp->setPosDisplay3D(navigator->getPosDisplay3D());
  //posDisp->resize(256,256);
  // posDisp->show();

  // Things I don't want to be used now
  eyeDistanceBox->setEnabled(false);
  Vol::setVolBehaviour(Vol::GO_BEHAVIOUR_DEFAULT);

  navigator->setMaxMemoryUsage(16 * 1024 * 1024);
  loadDefaults();
}


RenderApplication::~RenderApplication ()
{
	forwardProgressEnd();		//stop forwarding
	if (progressThreadRunning)
		progressThread.kill();	//kill progress thread
    goSDL::finishVideo();
	delete joystick;
	delete keyboard;
    delete navigator;
}

void
RenderApplication::loadDefaults()
{
	viewPosBoxX->setValue(0);
	viewPosBoxY->setValue(0);
	viewPosBoxZ->setValue(0);
	viewDepthBox->setValue(64);
	eyeDistanceBox->setValue(100);
	xRot->setValue(2);
	yRot->setValue(2);
	zRot->setValue(2);
	sampleDistanceEditX->setText("1");
	sampleDistanceEditY->setText("1");
	sampleDistanceEditZ->setText("1");
	zoomCheckBox->setChecked(true);
	sameSizeCheckBox->setChecked(true);
	memoryLabel->setText(0);
}

void
RenderApplication::setAllValues()
{
	changeViewPos(); 							//Set viewer position
	changeViewDepth(viewDepthBox->value());		//Set depth
	changeEyeDistance(eyeDistanceBox->value());	//Set eye distance
	changeSampleDistance();						//Set sample distance
}
	

void
RenderApplication::setImageSize (int w, int h)
{
    navigator->setScreenSize (w,h);
    goSDL::initVideo (w,h,32);
}

// Deprecated
void
RenderApplication::paintEvent(QPaintEvent* e)
{
    // return;

    updateMutex.lock();
    RenderGUI::paintEvent(e);
    if (!imageValid)
	{
	    updateMutex.unlock();
	    return;
	}
    // QImage img (imageFrame->width(), imageFrame->height(), 32);
    // img.fill (0);
    /*
     * Change all this getting and converting images. It is slow. For now it does what it should though.
     */
    goSignal2D<goUInt32> *finalImage;
    try
	{
	    finalImage = navigator->getFinalImage ();
	}
    catch (goExceptionString &e)
	{
	    e.print();
	    updateMutex.unlock();
	    return;
	}
    goSDL::drawImage(finalImage);

    updateMutex.unlock();
    return; 

}

void
RenderApplication::updateMemory()
{
	// Update memory usage
	QString s;
	s.setNum(navigator->memoryUsage());
	memoryLabel->setText(s);
	memoryLabel->resize(memoryLabel->sizeHint());
	memoryLabel->update();
}

void
RenderApplication::render ()
{
    // setProgress(0.0);
    if (!progressThreadRunning)
	{
	    progressThread.create (render_progress_thread, (void*)this, 1);
	    progressThreadRunning = true;
	}

    navigator->updateView();	// triggers view update and renderer
    imageValid = true;
	updateMemory();	
#if 0
    // progressBar->reset();
    goFloat p;
    int p_i = 0;
    while ( (p = waitForProgress()) < 1.0f )
	{
	    cout << "Setting progressbar to " << (int)(progressBar->totalSteps() * p) << endl;
 	    if ((int)(progressBar->totalSteps() * p) > p_i)
 		{
 		    p_i = (int)(progressBar->totalSteps() * p);
 		    //progressBar->setProgress(p_i);
 		    // this->update();
 		    //progressBar->update();
 		}
	}
    // progressBar->reset();
    this->update();
#endif
}

void
RenderApplication::setVolumeFileName ()
{
    QString s;
    s = QFileDialog::getOpenFileName (QString::null, QString("TransVol (*.dwtv)\nAll Files (*)"));
    if (s.isNull())
	{
	    cout << "No file name selected" << endl;
	    return;
	}
    goString filename;
    filename = (const char*)s;
    
    std::cout << "RenderApplication::setvolumeFileName() is initializing the volume navigator." << endl;
    std::cout << "\tFile name: " << filename << endl;
    // filename = "../cg120.dwtv";
    // Prevent from paining an invalid image
    imageValid = false;
    // Make this automatic, reading from the volume file or taking a user value
    // navigator->setDataType (GO_INT16);
    navigator->setDataType(filename.toCharPtr());
    navigator->setFileName(filename);		// Apply the file name
    try
	{
	    navigator->init();				// Finally, initialize the navigation and rendering system
	}
    catch (goException& e)
	{
	    e.print();
	    QString caption = "Exception";
	    QString msg = "There was an exception initializing the volume navigator.\nIst is possible that the file you tried to open\ndoes not exist or you don't have the appropriate access rights.";
	    QMessageBox::warning ((QWidget*)0, (const QString&)caption, (const QString&)msg, (int)QMessageBox::Ok, (int)QMessageBox::NoButton);
	    return;
	}

	loadDefaults();
	setAllValues();
	navigator->resetViewAngle();
	
	maxResBox->setMaxValue ((int)navigator->getFileInfo().stages);
	maxResBox->setValue (maxResBox->maxValue());
	changeMaxResolution (maxResBox->maxValue());
    renderButton->setEnabled(true);		// Allow user to render
    sampleDistanceChangeButton->setEnabled(true);
}

void
RenderApplication::saveImage()
{
    QString filename = QFileDialog::getSaveFileName (QString("image.pgm"),QString("*.pgm"));
    if (filename != QString::null)
	{
	    try
		{
		    navigator->saveImage ((const char*)filename);
		}
	    catch (goException& e) 
		{
		    e.print();
		    QString msg;
		    msg = "The image could not be saved. See libGo output for more information.";
		    QString caption = "Exception";
		    QMessageBox::warning ((QWidget*)0, (const QString&)caption, (const QString&)msg, (int)QMessageBox::Ok, (int)QMessageBox::NoButton);
		    return;
		}
	}
}


void
RenderApplication::changeViewPos()
{
    go3Vector<volFloat> pos;
    pos.x = viewPosBoxX->value();
    pos.y = viewPosBoxY->value();
    pos.z = viewPosBoxZ->value();
    navigator->moveViewTo (pos);
}

void
RenderApplication::viewPosXChange(int)
{
    changeViewPos();
}
void
RenderApplication::viewPosYChange(int)
{
    changeViewPos();
}
void
RenderApplication::viewPosZChange(int)
{
    changeViewPos();
}

void
RenderApplication::rotateViewPosX() 
{
    navigator->rotateView (M_PI/(float)180 * xRot->value(), Vol::GO_ROTATION_X);
    render();
}

void
RenderApplication::rotateViewPosY() 
{
    navigator->rotateView (M_PI/(float)180 * yRot->value(), Vol::GO_ROTATION_Y);
    render();
}

void
RenderApplication::rotateViewPosZ() 
{
    navigator->rotateView (M_PI/(float)180 * zRot->value(), Vol::GO_ROTATION_Z);
    render();
}

void
RenderApplication::changeViewDepth(int d) 
{
    navigator->setViewDepth ((goSize_t)d);
}

void
RenderApplication::changeEyeDistance(int d)
{
    navigator->setViewEyeDistance ((volFloat)d);
}

void
RenderApplication::changeSampleDistance ()
{
    goFloat x,y,z;
    x = (goFloat)sampleDistanceEditX->text().toFloat();    
    y = (goFloat)sampleDistanceEditY->text().toFloat();
    z = (goFloat)sampleDistanceEditZ->text().toFloat();
    navigator->setSampleDistance (x,y,z);
}

void
RenderApplication::optGreyValues ()
{
   navigator->optGreyValues();
}

void
RenderApplication::showOpacityTF()
{
    opacityTFFrame->show();
}

void
RenderApplication::showDensityTF()
{
    densityTFFrame->show();
}

void
RenderApplication::showGreyTF()
{
    greyTFFrame->show();
}

void
RenderApplication::changeGreyTF()
{
    navigator->setTransferFunctionPoints (greyTFFrame->getPointsX(), greyTFFrame->getPointsY(), Vol::GO_TF_GREY);
    this->update();
}

void
RenderApplication::changeOpacityTF()
{
    navigator->setTransferFunctionPoints (opacityTFFrame->getPointsX(), opacityTFFrame->getPointsY(), Vol::GO_TF_OPACITY);
    this->update();
}

void
RenderApplication::changeDensityTF()
{
    navigator->setTransferFunctionPoints (densityTFFrame->getPointsX(), densityTFFrame->getPointsY(), Vol::GO_TF_DENSITY);
    this->update();
}

void
RenderApplication::setMinMaxGreyDetermination (bool b)
{
    setMinMaxGrey = b;
}

void
RenderApplication::changeMaxResolution (int r)
{
    navigator->setMaxResolution (r);
}

void
RenderApplication::moveForward()
{
    go3Vector<volFloat> v;
    v.z = 10;
    v.x = 0;
    v.y = 0;
    navigator->moveView (v);
}

void
RenderApplication::moveBackward()
{
    go3Vector<volFloat> v;
    v.z = -10;
    v.x = 0;
    v.y = 0;
    navigator->moveView (v);
}

void
RenderApplication::moveLeft()
{
    go3Vector<volFloat> v;
    v.x = -10;
    v.y = 0;
    v.z = 0;
    navigator->moveView (v);
}

void
RenderApplication::moveRight()
{
    go3Vector<volFloat> v;
    v.x = 10;
    v.y = 0;
    v.z = 0;
    navigator->moveView (v);
}


void
RenderApplication::keyPressEvent (QKeyEvent *e)
{
#if 0
    switch (e->key())
	{
	case Qt::Key_W:
	    moveForward(); render(); break;
	case Qt::Key_S:
	    moveBackward(); render(); break;
	case Qt::Key_A:
	    moveLeft(); render(); break;
	case Qt::Key_D:
	    moveRight(); render(); break;
	default:
	    RenderGUI::keyPressEvent (e);
	    break;
	}
#endif
}

void
RenderApplication::changeZoom(bool z)
{
	if (z)
	{
		Vol::setVolBehaviour(Vol::getVolBehaviour() | Vol::GO_BEHAVIOUR_ZOOM);
		sameSizeCheckBox->setChecked(true);
		sameSizeCheckBox->setEnabled(false);
	} else
	{
		Vol::setVolBehaviour(Vol::getVolBehaviour() & (~Vol::GO_BEHAVIOUR_ZOOM));
		sameSizeCheckBox->setEnabled(true);
	}
}

void
RenderApplication::changeSameSize(bool z)
{
	if (!z)
	{
		Vol::setVolBehaviour(Vol::getVolBehaviour() | Vol::GO_BEHAVIOUR_RES_DEPENDENT_VIEW);
	} else
	{
		Vol::setVolBehaviour(Vol::getVolBehaviour() & (~Vol::GO_BEHAVIOUR_RES_DEPENDENT_VIEW));
	}
}
