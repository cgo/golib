#ifndef GOTYPE_H
#define GOTYPE_H

#include <gotypes.h>
#ifndef GOSTRING_H
# include <gostring.h>
#endif
#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif

/*! 
 * \brief Provides type information.
 *
 * \addtogroup types
 * @{
 */

class goTypePrivate;

class goType : public goObjectBase
{
    public:
        goType (goTypeEnum t);
        virtual ~goType ();

        static goSize_t  getSize   (goTypeEnum t);
        static bool      isSigned  (goTypeEnum t);
        static void      getString (goTypeEnum t, goString& stringRet);

        goSize_t        getSize   () const;
        bool            isSigned  () const;
        const goString& getString () const;
        goTypeEnum      getID     () const;

    private:
        goTypePrivate* myPrivate;
};

/*! @} */
#endif
