#ifndef GOVGLBLOCKRENDERER_H
#define GOVGLBLOCKRENDERER_H
 
#include <vgl/scene.h>
#include <gosblockrenderer.h>
#include <goarray.h>
#include <gotypes.h>
#include <gosignal3d.h>

class VGLSampleGridExternData;
class VGLPerspectiveCamera;
class VGLDirectionalLight;
class VGLSampleMaterial;
class VGLVolumeRenderObject;

// template<class T> class goSignal3D;
class goViewVolume;


namespace Vol {

template <class T>
class goVGLBlockRenderer : public goSBlockRenderer<T>
{
	public:
		goVGLBlockRenderer();
		virtual ~goVGLBlockRenderer();

    	virtual void init ();
		/// Starts rendering
	    //virtual void render			(goArray<goSize_t>* indices);
	    virtual void renderThread	(goArray<goSize_t>* indices);
    	//virtual void renderSubImages(goArray<goSize_t>* indices);
		
		virtual void setViewPlane (goViewVolume& vp);
	
		VGLSampleGridExternData *createSampleGrid(goSignal3D<T> *block);	
	protected:
		void createMaterial();
		void addObjectToScene(VGLVolumeRenderObject& o, goSize_t blockIndex);		
	private:	
		VGLScene				*scene;
		VGLPerspectiveCamera	*camera;
		VGLDirectionalLight		*light;
		VGLSampleMaterial		*material;
};

};
#endif


