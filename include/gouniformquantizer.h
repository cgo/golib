#ifndef GOUNIFORMQUANTIZER_H
#define GOUNIFORMQUANTIZER_H

#ifndef GOQUANTIZER_H
# include <goquantizer.h>
#endif

template <class _input_type, class _output_type>
class goUniformQuantizer : public goQuantizer <_input_type, _output_type>
{
    public:
        goUniformQuantizer ();
        virtual ~goUniformQuantizer ();

    public:
        virtual _output_type quantize (_input_type) const;
        
    private:
        goUniformQuantizer<_input_type, _output_type>& operator= (goUniformQuantizer<_input_type, _output_type>& other);
        goUniformQuantizer (goUniformQuantizer<_input_type, _output_type>& other);
};

#endif
