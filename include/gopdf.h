#ifndef GOPDF_H
#define GOPDF_H

#include <goobjectbase.h>
#ifndef GODEFS_H
# include <godefs.h>
#endif

namespace goMath
{

/*! \addtogroup math
* @{
*/
/*! \brief Probability density function base template.
*
* output_type will typically be of double or float type.
* input_type is the function's input, e.g. float, double, or a vector of floats or doubles.
*/
template <class input_type, class output_type>
class PDF : public goObjectBase
{
    public:
        virtual ~PDF () {};

        virtual output_type operator() (const input_type&) const = 0;

        // FIXME: add things common to all probability density functions here

    protected:
        PDF () { this->setClassID(GO_PDF); };
};
/*! @} */

};
#endif
