/****************************************************************************
** RenderGUI meta object code from reading C++ file 'rendergui.h'
**
** Created: Thu Jan 17 10:21:04 2002
**      by: The Qt MOC ($Id: rendergui_h.cc,v 1.12 2002/01/17 16:45:56 christian Exp $)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#if !defined(Q_MOC_OUTPUT_REVISION)
#define Q_MOC_OUTPUT_REVISION 9
#elif Q_MOC_OUTPUT_REVISION != 9
#error "Moc format conflict - please regenerate all moc files"
#endif

#include "rendergui.h"
#include <qmetaobject.h>
#include <qapplication.h>



const char *RenderGUI::className() const
{
    return "RenderGUI";
}

QMetaObject *RenderGUI::metaObj = 0;

void RenderGUI::initMetaObject()
{
    if ( metaObj )
	return;
    if ( qstrcmp(QWidget::className(), "QWidget") != 0 )
	badSuperclassWarning("RenderGUI","QWidget");
    (void) staticMetaObject();
}

#ifndef QT_NO_TRANSLATION

QString RenderGUI::tr(const char* s)
{
    return qApp->translate( "RenderGUI", s, 0 );
}

QString RenderGUI::tr(const char* s, const char * c)
{
    return qApp->translate( "RenderGUI", s, c );
}

#endif // QT_NO_TRANSLATION

QMetaObject* RenderGUI::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    (void) QWidget::staticMetaObject();
#ifndef QT_NO_PROPERTIES
#endif // QT_NO_PROPERTIES
    QMetaData::Access *slot_tbl_access = 0;
    metaObj = QMetaObject::new_metaobject(
	"RenderGUI", "QWidget",
	0, 0,
	0, 0,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    metaObj->set_slot_access( slot_tbl_access );
#ifndef QT_NO_PROPERTIES
#endif // QT_NO_PROPERTIES
    return metaObj;
}
