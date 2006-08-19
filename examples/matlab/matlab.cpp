#define HAVE_MATLAB 1

#include <engine.h>
#include <gomatlab.h>
#include <stdlib.h>

int main (int argc, char* argv[])
{
    goMatlab matlab;

    matlab.doubleToVariable (1.0, "my_variable");
    goString result;
    result.resize(1024);
    if (matlab.matlabCall ("my_variable", &result))
    {
        printf ("Ok, result: %s\n", result.toCharPtr());
    }
    else
    {
        printf ("Failure.\n");
    }
    exit(1);
}
