#ifndef GOQUANTIZER_H
#define GOQUANTIZER_H

#include <goobjectbase.h>

/*!
 * \addtogroup signal
 * @{
 */
/**
 * @brief Quantizer base class.
 *
 * Derive from this class for quantizers.
 *
 * \author Christian Gosch
 **/
template <class _input_type, class _output_type>
class goQuantizer : public goObjectBase
{
    public:
        goQuantizer(); 
        virtual~ goQuantizer();

    public:
        virtual _output_type quantize (_input_type) const;

    private:
        goQuantizer (goQuantizer<_input_type, _output_type>& other);
        goQuantizer<_input_type, _output_type>& operator= (goQuantizer<_input_type, _output_type>& other); 
};
/*! @} */
#endif
