#ifndef GOUNIFORMQUANTIZER_H
#define GOUNIFORMQUANTIZER_H

#ifndef GOQUANTIZER_H
# include <goquantizer.h>
#endif
#include <gotypes.h>


template <class _input_type,class _output_type> class goUniformQuantizerPrivate;

/*!
 * \addtogroup signal
 * @{
 */
/**
 * @brief Uniform quantizer.
 *
 * @param _input_type   Input data type.
 * @param _output_type  Output data type.
 *
 * \author Christian Gosch
 **/
template <class _input_type, class _output_type>
class goUniformQuantizer : public goQuantizer <_input_type, _output_type>
{
    public:
        goUniformQuantizer (goSize_t quantizationSteps = 255);
        goUniformQuantizer (_input_type  delta_input,
                            _input_type  min_input,
                            _input_type  max_input,
                            _output_type min_output,
                            _output_type max_output);
        virtual ~goUniformQuantizer ();

    public:
        virtual _output_type quantize (_input_type) const;

    private:
       goUniformQuantizerPrivate<_input_type, _output_type>* myPrivate; 
        
    private:
        goUniformQuantizer<_input_type, _output_type>& operator= (goUniformQuantizer<_input_type, _output_type>& other);
        goUniformQuantizer (goUniformQuantizer<_input_type, _output_type>& other);
};
/*! @} */
#endif
