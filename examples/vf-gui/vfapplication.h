#include <vfgui.h>

class VFApplication : public VFGUI
{
	Q_OBJECT
	public:
		VFApplication(QWidget *parent = 0, const char* name = 0, WFlags f = 0);
		~VFApplication();
	
	public slots:
		void convert();
		void browseRawFileName();
		void browseTransFileName();		
	protected:
	private:	
};
