/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goposdisplay3d.h>
#include <govol.h>
#include <gotypes.h>
#include <go3vector.h>
#include <gomath.h> // MAX
#include <gothread.h>

namespace Vol {

static void goPosDisplay_display(void);
static void goPosDisplay_motion(int x, int y);
static void goPosDisplay_mouse(int button, int state, int x, int y);


goPosDisplay3D::goPosDisplay3D()
{
	initialized = false;
}

goPosDisplay3D::~goPosDisplay3D()
{
}

int goPosDisplay_x = 0;
int goPosDisplay_y = 0;
int goPosDisplay_button = 0;
GLfloat goPosDisplay_cone_angle = 0;
GLfloat goPosDisplay_viewPosX = 0.0f;
GLfloat goPosDisplay_viewPosY = 0.0f;
GLfloat goPosDisplay_viewPosZ = 0.0f;
go3Vector<volFloat> goPosDisplay_viewN;
go3Vector<volFloat> goPosDisplay_viewPos;
go3Vector<volFloat> goPosDisplay_scale;


static void goPosDisplay_motion(int x, int y)
{
	// goPosDisplay_cone_angle = (float)(y - goPosDisplay_y);
	/*
	if (goPosDisplay_button == GLUT_LEFT_BUTTON)
	{
		goPosDisplay_viewPos.x += ((x - goPosDisplay_x) / 256.0f);
		goPosDisplay_viewPos.y += (-(y - goPosDisplay_y) / 256.0f);
		goPosDisplay_display();
		goPosDisplay_x = x;
		goPosDisplay_y = y;
	}
	*/
	
	if (goPosDisplay_button == GLUT_RIGHT_BUTTON)
	{
		glRotatef((float)(x - goPosDisplay_x) , 0.0, 1.0, 0.0);
		glRotatef((float)(y - goPosDisplay_y) , 1.0, 0.0, 0.0);
		goPosDisplay_display();
		goPosDisplay_x = x;
		goPosDisplay_y = y;
	}
	/*
	if (goPosDisplay_button == GLUT_MIDDLE_BUTTON)
	{
		goPosDisplay_viewPos.z += ((y - goPosDisplay_y) / 256.0f);
		goPosDisplay_display();
		goPosDisplay_x = x;
		goPosDisplay_y = y;
	}
	*/
		
}

static void goPosDisplay_mouse(int button, int state, int x, int y)
{
	goPosDisplay_button = button;
	// if (button == GLUT_LEFT_BUTTON)
	{
		if (state == GLUT_DOWN)
		{
			goPosDisplay_x = x;
			goPosDisplay_y = y;
		}
	}
}

static void goPosDisplay_display(void)
{
/*  clear all pixels  */
    glClear (GL_COLOR_BUFFER_BIT);

/*  draw white polygon (rectangle) with corners at
 *  (0.25, 0.25, 0.0) and (0.75, 0.75, 0.0)  
 */
    glColor3f (1.0, 1.0, 1.0);
	glPushMatrix();
	glScalef (goPosDisplay_scale.x,
			  goPosDisplay_scale.y,
			  goPosDisplay_scale.z);
	glutWireCube(1.0);
	glPopMatrix();
	
	/* Koordinatensystem */
	glColor3f (1.0, 0.0, 0.0);
	glBegin(GL_LINES);
		glVertex3f (0.0, 0.0, 0.0);
		glVertex3f (1.0, 0.0, 0.0);
	glEnd();
	glColor3f (0.0, 1.0, 0.0);
	glBegin(GL_LINES);
		glVertex3f (0.0, 0.0, 0.0);
		glVertex3f (0.0, 1.0, 0.0);
	glEnd();
	glColor3f (0.0, 0.0, 1.0);
	glBegin(GL_LINES);
		glVertex3f (0.0, 0.0, 0.0);
		glVertex3f (0.0, 0.0, 1.0);
	glEnd();

	glPushMatrix();
	glTranslatef(goPosDisplay_viewPos.x,
				 goPosDisplay_viewPos.y,
				 goPosDisplay_viewPos.z);
	glColor3f(0.0,1.0,0.0);
	glutSolidSphere(0.1,
                 	20, 20);
	glColor3f(1.0,0.0,0.0);
	glBegin(GL_LINES);
		glVertex3f(0.0,0.0,0.0);
		glVertex3f(goPosDisplay_viewN.x,
				   goPosDisplay_viewN.y,
				   goPosDisplay_viewN.z);
	glEnd();
	glPopMatrix();
/*  don't wait!  
 *  start processing buffered OpenGL routines 
 */
    glFlush ();
	glutSwapBuffers();
}

void*
goPosDisplay_thread (void*)
{
	cout << "Glutinit\n";
	cout << "Glutinitdisplaymode\n";
    glutInitDisplayMode (GLUT_DOUBLE | GLUT_RGB);
	cout << "windowsize\n";
	
    glutInitWindowPosition (100, 100);
	
    /*  select clearing (background) color       */
    glClearColor (0.0, 0.0, 0.0, 0.0);

	/*  initialize viewing values  */
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glFrustum(-1.0, 1.0, -1.0, 1.0, 0.01, 1.0);
	// glOrtho(0.0, 1.0, 0.0, 1.0, -1.0, 1.0);
    //gluLookAt(0.0,0.0,0.0,
	//		  0.0,0.0,0.0,
	//		  0.0,-1.0,0.0);
	glMatrixMode(GL_MODELVIEW);	
	glShadeModel(GL_SMOOTH);
	glutCreateWindow ("View Position");
    glutDisplayFunc (goPosDisplay_display); 
	glutMouseFunc (goPosDisplay_mouse);
	glutMotionFunc (goPosDisplay_motion);
	glutMainLoop();
	return 0;
}

void
goPosDisplay3D::init(int argc, char** argv, int width, int height)
{
	thread.cancel();
	glutInit(&argc, argv);
    glutInitWindowSize (width, height); 
	thread.create (goPosDisplay_thread,0,1);
}

void
goPosDisplay3D::setViewVolume(goViewVolume& v)
{
	
	goPosDisplay_viewPos = v.getEyePos();
	goPosDisplay_viewN   = v.getNormal();
	goPosDisplay_viewPos.x /= (float)volumeSize.x;
	goPosDisplay_viewPos.y /= (float)volumeSize.y;
	goPosDisplay_viewPos.z /= (float)volumeSize.z;
	if (initialized)
		goPosDisplay_display();
}

void
goPosDisplay3D::setVolumeSize(goSize3D& sz)
{
	volumeSize = sz;
	volFloat scale = 1 / (float)(MAX((MAX(sz.x,sz.y)),sz.z));
	goPosDisplay_scale.x = sz.x * scale;
	goPosDisplay_scale.y = sz.y * scale;
	goPosDisplay_scale.z = sz.z * scale;
}


};
