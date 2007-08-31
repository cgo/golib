#define HAVE_MATLAB 1

#include <engine.h>
#include <gomatlab.h>
#include <stdlib.h>
#include <goprocess.h>

#include <iostream>
int main (int argc, char* argv[])
{
    //Engine* engine = engOpen("/opt/matlab73/bin/matlab -nodesktop -nosplash");
    Engine* engine = engOpen("matlab");
    if (!engine)
        printf ("First attempt failed.\n");
    else
        printf ("First attempt succeeded.\n");
    char i;
    std::cin >> i;

    goProcess p;
    p.run ("matlab", "");

    std::cin >> i;

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
