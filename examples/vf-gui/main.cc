#include <vfapplication.h>
#include <qapplication.h>
#include <qplatinumstyle.h>

int main(int argc, char* argv[])
{
	QApplication app(argc, argv);
	VFApplication vfapp;
	app.setStyle(new QPlatinumStyle);
	QObject::connect((const QObject*)vfapp.quitButton,SIGNAL(clicked()),
					 (QObject*)&app,SLOT(quit()));
	app.setMainWidget(&vfapp);
	vfapp.show();
	return app.exec();
}

