#include <gogl/textureimage.h>
#include <gogl/texture.h>
#include <goautoptr.h>
#include <golist.h>
#include <gomatrix.h>
#include <gosignalhelper.h>

namespace goGL
{
    class TextureImagePrivate
    {
        public:
            TextureImagePrivate () 
                : textures (), 
                  textureCoordinates (),
                  textureCoordinatesStepX (1.0),
                  textureCoordinatesStepY (1.0)
            {};
            ~TextureImagePrivate () {};

            goList<goAutoPtr<Texture> > textures;
            goMatrixf                   textureCoordinates;

            goDouble textureCoordinatesStepX;
            goDouble textureCoordinatesStepY;
    };
};

goGL::TextureImage::TextureImage ()
    : DrawableObject (),
      myPrivate (0)
{
    myPrivate = new TextureImagePrivate;
}

goGL::TextureImage::~TextureImage ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void goGL::TextureImage::setImage (const goSignal3DBase<void>& image)
{
    myPrivate->textures.erase ();

    const goSignal3DBase<void>* sig = &image;

    goSize_t signalSizeX = sig->getSizeX();
    goSize_t signalSizeY = sig->getSizeY();
    // goSize_t signalSizeZ = sig->getSizeZ();
    goSize_t tileSizeX   = sig->getBlockSizeX();
    goSize_t tileSizeY   = sig->getBlockSizeY();
    while (tileSizeX > signalSizeX)
    {
        tileSizeX >>= 1;
    }
    while (tileSizeY > signalSizeY)
    {
        tileSizeY >>= 1;
    }
    if (tileSizeX == 0 || tileSizeY == 0)
    {
        return;
    }
    goSize_t tileCountX = (signalSizeX + tileSizeX - 1) / tileSizeX;
    goSize_t tileCountY = (signalSizeY + tileSizeY - 1) / tileSizeY;
    
    goSize_t tileCount = tileCountX * tileCountY;

    // myPrivate->textureNames.resize       (tileCount);
    myPrivate->textureCoordinates.resize (tileCount, 2);

    GLdouble tileCoordinateStepX = 1.0 / (double)(signalSizeX) * tileSizeX;
    GLdouble tileCoordinateStepY = -1.0 / (double)(signalSizeY) * tileSizeY;
    
    myPrivate->textureCoordinatesStepX = tileCoordinateStepX;
    myPrivate->textureCoordinatesStepY = tileCoordinateStepY;
   
    //myPrivate->textureNames.fill (0);

//#ifdef DEBUG
    printf ("tileCount == %d\n", tileCount);
    printf ("tile size == %d x %d\n", tileSizeX, tileSizeY);
//#endif

//    glGenTextures (tileCount, &myPrivate->textureNames[0]);
//    if (glGetError() != GL_NO_ERROR)
//    {
//        std::cout << "GL error generating textures!\n";
//    }

    goSize_t i;
    // GLushort* tileData = new GLushort [tileSizeX * tileSizeY];
    // Create a subsignal and move it over the original signal,
    // creating textures for each tile on the way.
    goSubSignal3D<void> subSignal (const_cast<goSignal3DBase<void>*>(sig), tileSizeX, tileSizeY, 1);
    goSignal3D<void> tile;
    tile.setDataType (image.getDataType().getID());
    tile.make (goSize3D (tileSizeX, tileSizeY, 1), goSize3D (tileSizeX, tileSizeY, 1), goSize3D (0, 0, 0), image.getChannelCount());

    goPosition tilePosition (0, 0, 0);
    GLdouble tileCoordinateX = -0.5; //0.0;
    GLdouble tileCoordinateY = 0.5; //1.0;

//    goArray<goUInt16> LUT;
//    goUInt16*         LUTOrigin = 0;
//    goType            targetType (GO_UINT16);
//    goIndex_t minIndex = sig->getDataType().getMinIndex();
//    goIndex_t maxIndex = sig->getDataType().getMaxIndex();

    // NOTE: Scaling in quantization tables does not work yet -- 
    //       so float and double are scaled
    //       "by hand" if needed.
//    goSignal3D<void> tempSig;
//    if (goNormalizeSignal (sig, &tempSig))
//    {
//        sig = &tempSig;
//    }
//    LUTOrigin = goCreateQuantizationTable (sig->getDataType(),
//                                           (goUInt16)targetType.getMinimum(),
//                                           (goUInt16)targetType.getMaximum(),
//                                           minIndex, maxIndex, LUT);
//    goIndexFunction indexFunction = sig->getDataType().getIndexFunction();

    for (i = 0; i < tileCount; ++i)
    {
        subSignal.setPosition (tilePosition);
        goCopySignal (&subSignal, &tile);
//        GLushort* tileDataP = tileData;
        // GO_SIGNAL3D_EACHELEMENT_GENERIC (*(tileDataP++) = *(goUInt16*)__ptr, subSignal);
//        GO_SIGNAL3D_EACHELEMENT_GENERIC (*(tileDataP++) = LUTOrigin[indexFunction(__ptr)], subSignal);

        myPrivate->textures.append (goAutoPtr<Texture> (new Texture (tile)));

//        glBindTexture (GL_TEXTURE_2D, myPrivate->textureNames[i]);
//        if (glGetError() != GL_NO_ERROR)
//        {
//            std::cout << "GL error binding texture!\n";
//        }

        myPrivate->textureCoordinates (i, 0) = tileCoordinateX;
        myPrivate->textureCoordinates (i, 1) = tileCoordinateY;

//        myPrivate->textureCoordinates[2*i] = tileCoordinateX;
//        myPrivate->textureCoordinates[2*i + 1] = tileCoordinateY;

//        glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,     GL_REPEAT);
//        glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,     GL_REPEAT);
//        glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
//        glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
//        if (glGetError() != GL_NO_ERROR)
//        {
//            std::cout << "GL error setting texture parameters!\n";
//        }
//                   
//        glTexImage2D (GL_TEXTURE_2D, 0, GL_RGB, tileSizeX, 
//                      tileSizeY, 0, GL_LUMINANCE, GL_UNSIGNED_SHORT, 
//                      tileData);
//        if (glGetError() != GL_NO_ERROR)
//        {
//            std::cout << "GL error! Could not create texture image\n";
//        }

        tilePosition.x  += tileSizeX;
        tileCoordinateX += tileCoordinateStepX;
        if (tilePosition.x >= (goIndex_t)signalSizeX)
        {
            tilePosition.x   = 0;
            tilePosition.y  += tileSizeY;
            tileCoordinateX  = -0.5;
            tileCoordinateY += tileCoordinateStepY;
        }
    }
}

bool goGL::TextureImage::operator () () const
{
    glPushMatrix ();
    this->setRenderParameters ();
    const goVectorf& t = this->getTranslation();
    const goVectorf& r = this->getRotation();
    const goVectorf& s = this->getScale();
    glTranslatef (t[0], t[1], t[2]);
    glScalef (s[0], s[1], s[2]);
    glRotatef (r[0], r[1], r[2], r[3]);
    bool ok = this->draw ();
    glPopMatrix ();
    return ok;
}

bool goGL::TextureImage::draw () const
{
    if (myPrivate->textures.getSize() == 0)
    {
        return false;
    }

    if (myPrivate->textures.getSize() != (goIndex_t)myPrivate->textureCoordinates.getRows())
    {
        goLog::error ("goGL::TextureImage::draw(): Wrong number of texture coordinates.");
        return false;
    }

    glEnable (GL_TEXTURE_2D);

//    GLdouble    x     = 0.0;
//    GLdouble    y     = 1.0;
//    goIndex_t   yi    = 0;
//    GLfloat     f     = 0.0;

    glTexEnvf (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);

    goList<goAutoPtr<Texture> >::Element* el = myPrivate->textures.getFrontElement();
    goSize_t i = 0;
    goSize_t sz = myPrivate->textureCoordinates.getRows ();
    while (el && i < sz)
    {
        glBindTexture (GL_TEXTURE_2D, el->elem->getName());
        if (glGetError() != GL_NO_ERROR)
        {
            printf ("GL error binding texture!\n");
        }

        goFloat xx = myPrivate->textureCoordinates(i,0);
        goFloat yy = myPrivate->textureCoordinates(i,1);

        glBegin (GL_QUADS);
            glTexCoord2d (0.0, 0.0);
            glVertex3d (xx, yy, 0.0); 
            glTexCoord2d (1.0, 0.0);
            glVertex3d (xx + myPrivate->textureCoordinatesStepX, yy, 0.0);
            glTexCoord2d (1.0, 1.0);
            glVertex3d (xx + myPrivate->textureCoordinatesStepX, 
                        yy + myPrivate->textureCoordinatesStepY, 0.0);
            glTexCoord2d (0.0, 1.0);
            glVertex3d (xx, yy + myPrivate->textureCoordinatesStepY, 0.0);
        glEnd ();
        if (glGetError() != GL_NO_ERROR)
        {
            printf ("GL error!\n");
        }

        el = el->next;
        ++i;
    }

    glDisable (GL_TEXTURE_2D);

    return true;
}


bool goGL::TextureImage::init () //= Do stuff like creating lists etc.
{
    glEnable (GL_TEXTURE_2D);
    return true;
}
