/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gotypes.h>
#include <qcheckbox.h>
#include <vfapplication.h>
#include <vfinterface.h>
#include <iostream.h>
#include <qspinbox.h>
#include <qcombobox.h>
#include <qradiobutton.h>
#include <qwidget.h>
#include <qfiledialog.h>

VFApplication::VFApplication(QWidget* parent, const char* name, WFlags fl)
	: VFGUI(parent, name, fl)
{
	QObject::connect((const QObject*)convertButton, SIGNAL(clicked()),
					 (QObject*)this, SLOT(convert()));
	QObject::connect((const QObject*)fileBrowseButton1, SIGNAL(clicked()),
					 (QObject*)this, SLOT(browseRawFileName()));
	QObject::connect((const QObject*)fileBrowseButton2, SIGNAL(clicked()),
					 (QObject*)this, SLOT(browseTransFileName()));
	textFileButton->setEnabled(false);
}	

VFApplication::~VFApplication()
{
}

#define VF_APP_CONVERT(TYPE) {\
	VFInterface<TYPE> vfi; \
	if (lineWiseBox->isChecked()) \
		vfi.setMaxMemory(0); \
	if(rawDataButton->isChecked()) \
	{ \
		vfi.raw2dwtv((const char*)rawDataEdit->text(), \
					 (const char*)dwtvEdit->text(), \
					 size_x, size_y, size_z, \
					 blocksize_x, blocksize_y, blocksize_z, \
				 	 file_type, stagesBox->value(), slowLoadingBox->isChecked()); \
	} else \
	{ \
		vfi.jpeg2dwtv((const char*)rawDataEdit->text(), \
					  (const char*)dwtvEdit->text(), \
					  size_x, size_y, size_z, \
					  blocksize_x, blocksize_y, blocksize_z, \
				 	  file_type, stagesBox->value()); \
	} \
}

void
VFApplication::convert()
{
	goSize_t size_x = volSizeBox1->value();
	goSize_t size_y = volSizeBox2->value();
	goSize_t size_z = volSizeBox3->value();
	goSize_t blocksize_x = blockSizeBox1->value();
	goSize_t blocksize_y = blockSizeBox2->value();
	goSize_t blocksize_z = blockSizeBox3->value();
	int data_type;
	int file_type;
		
	switch(dataTypeBox->currentItem())
	{
		case 0: data_type = GO_INT8; break;
		case 1: data_type = GO_UINT8; break;
		case 2: data_type = GO_INT16; break;
		case 3: data_type = GO_UINT16; break;
		case 4: data_type = GO_INT32; break;
		case 5: data_type = GO_UINT32; break;
		case 6: data_type = GO_FLOAT; break;
		case 7: data_type = GO_DOUBLE; break;
	}
	
	switch(fileTypeBox->currentItem())
	{
		case 0: file_type = Vol::GO_VOLUMEFILE_BLOCKWISE; break;
		case 1: file_type = Vol::GO_VOLUMEFILE_BANDWISE; break;
	}
	switch(data_type)
	{
		case GO_INT8: VF_APP_CONVERT(goInt8); break;
		case GO_UINT8: VF_APP_CONVERT(goUInt8); break;
		case GO_INT16: VF_APP_CONVERT(goInt16); break;
		case GO_UINT16: VF_APP_CONVERT(goUInt16); break;
/*
		case GO_INT32: VF_APP_CONVERT(goInt32); break;
		case GO_UINT32: VF_APP_CONVERT(goUInt32); break;
		case GO_FLOAT: VF_APP_CONVERT(goFloat); break;
		case GO_DOUBLE: VF_APP_CONVERT(goDouble); break;
*/
		default: break;
	}
	cout << "convert() called\n";
}

void
VFApplication::browseRawFileName()
{
	QString s;
	s = QFileDialog::getOpenFileName();
	if (!s.isNull())
	{
		rawDataEdit->setText(s);
	}
}

void
VFApplication::browseTransFileName()
{
	QString s;
	s = QFileDialog::getSaveFileName();
	if (!s.isNull())
	{
		dwtvEdit->setText(s);
	}
}
