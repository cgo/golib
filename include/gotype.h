#ifndef GOTYPE_H
#define GOTYPE_H

#include <gotypes.h>
#ifndef GOSTRING_H
# include <gostring.h>
#endif
#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif

typedef bool (*goCompareFunction)(const void*, const void*);

class goTypePrivate;
/*!
 * \addtogroup types
 * @{
 */
/*! 
 * \brief Provides type information.
 */
class goType : public goObjectBase
{
    public:
        goType (goTypeEnum t);
        goType (const goType& other);
        virtual ~goType ();

        static goSize_t  getSize   (goTypeEnum t);
        static bool      isSigned  (goTypeEnum t);
        static void      getString (goTypeEnum t, goString& stringRet);

        bool              setID     (goTypeEnum t);
        goSize_t          getSize   () const;
        bool              isSigned  () const;
        const goString&   getString () const;
        goTypeEnum        getID     () const;
        goCompareFunction getLowerThanFunction   () const;
        goCompareFunction getGreaterThanFunction () const;
        goCompareFunction getEqualFunction       () const;
       
        goDouble          getMinimum () const;
        goDouble          getMaximum () const;
        
        const goType& operator= (const goType& other);

    private:
        goTypePrivate* myPrivate;
};
/*! @} */
#endif
