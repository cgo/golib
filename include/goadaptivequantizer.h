#ifndef GO_ADAPTIVEQUANTIZER_H
#define GO_ADAPTIVEQUANTIZER_H

#include <gotypes.h>
#include <goquantizer.h>

template <class _input_type, class _output_type> class _goAdaptiveQuantizer;

template <class _input_type, class _output_type>
class goAdaptiveQuantizer : public goQuantizer
{
    public:
        goAdaptiveQuantizer ();
        virtual ~goAdaptiveQuantizer ();

    public:
        virtual _output_type quantize (_input_type) const;

    private:
        _goAdaptiveQuantizer* myPrivate;
};

#endif
