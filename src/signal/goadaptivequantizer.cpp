#include <goadaptivequantizer.h>

template <class _input_type, class _output_type>
class _goAdaptiveQuantizer
{
    public:
        _goAdaptiveQuantizer ();
        ~_goAdaptiveQuantizer ();

        goSignalPDF myPDF;
};

template <class _input_type, class _output_type>
_goAdaptiveQuantizer<_input_type, _output_type>::_goAdaptiveQuantizer ()
    : myPDF ()
{
}

template <class _input_type, class _output_type>
_goAdaptiveQuantizer<_input_type, _output_type>::~_goAdaptiveQuantizer ()
{
}

// ======================================================================

template <class _input_type, class _output_type>
goAdaptiveQuantizer<_input_type, _output_type>::goAdaptiveQuantizer ()
    : goQuantizer ()
{
    this->setClassID(GO_ADAPTIVEQUANTIZER);
    myPrivate = new _goAdaptiveQuantizer;
}

template <class _input_type, class _output_type>
goAdaptiveQuantizer<_input_type, _output_type>::~goAdaptiveQuantizer ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = NULL;
    }
}

template <class _input_type, class _output_type>
_output_type
goAdaptiveQuantizer<_input_type, _output_type>::quantize (_input_type value) const
{
    
}
