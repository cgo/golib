#ifndef GOUNIFORMQUANTIZER_HPP
#define GOUNIFORMQUANTIZER_HPP

// #include <gouniformquantizer.h>
#include <limits>
#include <gotypes.h>

template <class _input_type, class _output_type>
class
goUniformQuantizerPrivate
{
    public:
        goUniformQuantizerPrivate (goSize_t quantizationSteps); 
        goUniformQuantizerPrivate (_input_type  delta_input, 
                                   _input_type  min_input,
                                   _input_type  max_input,
                                   _output_type min_output,
                                   _output_type max_output);
        ~goUniformQuantizerPrivate ();
    
        goDouble     myDeltaInput_rec;
        _input_type  myDeltaInput;
        _input_type  myMinInput;
        _input_type  myMaxInput;
        goDouble     myDeltaOutput;
        _output_type myMinOutput;
        _output_type myMaxOutput;
};

template <class _input_type, class _output_type>
goUniformQuantizerPrivate<_input_type, _output_type>::goUniformQuantizerPrivate (goSize_t quantizationSteps)
    :
    myDeltaInput_rec (0.0),
    myDeltaInput  (0),
    myMinInput    (0),
    myMaxInput    (0),
    myDeltaOutput (0.0),
    myMinOutput   (0),
    myMaxOutput   (0)
{
    myMinInput  = std::numeric_limits<_input_type>::min (); 
    myMaxInput  = std::numeric_limits<_input_type>::max (); 
    myMinOutput = std::numeric_limits<_output_type>::min(); 
    myMaxOutput = std::numeric_limits<_output_type>::max();

    myDeltaInput  = (myMaxInput - myMinInput) / (_input_type)quantizationSteps;
    myDeltaOutput = (myMaxOutput - myMinOutput) / (_input_type)quantizationSteps;
    myDeltaInput_rec = 1.0 / (goDouble)myDeltaInput;
}

template <class _input_type, class _output_type>
goUniformQuantizerPrivate<_input_type,_output_type>::goUniformQuantizerPrivate (_input_type  delta_input, 
     _input_type  min_input,
     _input_type  max_input,
     _output_type min_output,
     _output_type max_output)
    :
    myDeltaInput_rec (0.0),
    myDeltaInput  (delta_input),
    myMinInput    (min_input),
    myMaxInput    (max_input),
    myDeltaOutput (0),
    myMinOutput   (min_output),
    myMaxOutput   (max_output)
{
    myDeltaOutput = (max_output - min_output) / ((max_input - min_input) / delta_input);
    myDeltaInput_rec = 1.0 / (goDouble)myDeltaInput;
}

template <class _input_type, class _output_type>
goUniformQuantizerPrivate<_input_type, _output_type>::~goUniformQuantizerPrivate ()
{
}

//=======================================================================//

template <class _input_type, class _output_type>
goUniformQuantizer<_input_type, _output_type>::goUniformQuantizer (goSize_t quantizationSteps)
    : goQuantizer<_input_type, _output_type> ()
{
    this->setClassName ("goUniformQuantizer");
    myPrivate = new goUniformQuantizerPrivate<_input_type, _output_type> (quantizationSteps);
}

template <class _input_type, class _output_type>
goUniformQuantizer<_input_type, _output_type>::goUniformQuantizer (
        _input_type  delta_input,
        _input_type  min_input,
        _input_type  max_input,
        _output_type min_output,
        _output_type max_output)
{
    this->setClassName ("goUniformQuantizer");
    myPrivate = new goUniformQuantizerPrivate<_input_type, _output_type> (delta_input, min_input, max_input, min_output, max_output);
}

template <class _input_type, class _output_type>
goUniformQuantizer<_input_type, _output_type>::~goUniformQuantizer ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = NULL;
    }
}

template <class _input_type, class _output_type>
_output_type
goUniformQuantizer<_input_type, _output_type>::quantize (_input_type input) const
{
    goSize_t d = (goSize_t)((input - myPrivate->myMinInput) * myPrivate->myDeltaInput_rec);
    return (_output_type)(myPrivate->myMinOutput + myPrivate->myDeltaOutput * d);
}
#endif
