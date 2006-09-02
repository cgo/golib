#ifndef GOGAUSSIMAGE_H
#define GOGAUSSIMAGE_H

#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif

class goGaussImagePrivate;

class goGaussImage
{
    public:
        goGaussImage ();
        virtual ~goGaussImage ();

        void update (const goSignal3DBase<void>& u);
        void reset  ();
        const goSignal3DBase<void>& getMean () const;
        const goSignal3DBase<void>& getVariance () const;

    private:
        goGaussImagePrivate* myPrivate;

    private:
        goGaussImage (goGaussImage&);
        goGaussImage& operator= (goGaussImage&);
};

#endif
