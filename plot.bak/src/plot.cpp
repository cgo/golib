/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goplot/plot.h>
// #include <list>
#include <exception>

namespace NSPACE
{
    class GraphPrivate
    {
        public:
            GraphPrivate (int d) 
                : axes (0), dim (0)
            { 
                axes = new GraphAxis [d];
                dim = d;
            };

            ~GraphPrivate ()
            {
                if (axes)
                {
                    delete[] axes;
                    axes = 0;
                }
            };

            GraphAxis *axes;
            int dim;
    };
};

NSPACE ::Graph::Graph (int dim)
{
    myPrivate = new GraphPrivate (dim);
}

NSPACE ::Graph::Graph (const Graph& other)
{
    *this = other;
}

int NSPACE ::Graph::dim () const
{
    return myPrivate->dim;
}

NSPACE ::Graph&
NSPACE ::Graph::operator= (const Graph& other)
{
    if (myPrivate)
        delete myPrivate;

    myPrivate = new GraphPrivate (other.dim ());
    
    int sz = other.dim ();
    try
    {
        for (int i = 0; i < sz; ++i)
        {
            myPrivate->axes[i] = other.axis (i);
        }
    }
    catch (std::exception& ex)
    {
        throw;
    }

    return *this;
}

NSPACE ::Graph::~Graph ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

NSPACE ::GraphAxis& NSPACE ::Graph::axis (int i)
{
    if (i < this->dim ())
        return myPrivate->axes[i];

    throw std::exception (); // ("Graph::axis(): Index out of range");
}

const NSPACE ::GraphAxis& NSPACE ::Graph::axis (int i) const
{
    if (i < this->dim ())
        return myPrivate->axes[i];

    throw std::exception (); // ("Graph::axis(): Index out of range");
}
