#include <goposdisplay3d.h>
#include <gotypes.h>
int main(int argc, char** argv)
{
	Vol::goPosDisplay3D disp;
	goSize3D sz;
	sz.x = 2136;
	sz.y = 1932;
	sz.z = 1539;
	disp.setVolumeSize(sz);
	goViewVolume v;
	v.setPosition(0,0,0);
	//v.setNormal(0,0,1);
	//v.setUp(0,1,0);
	v.update();
	disp.setViewVolume(v);	
	disp.init(argc, argv);
	char c;
	cin >> c;
	return 1;
}
