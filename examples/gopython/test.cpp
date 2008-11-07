#include <stdlib.h>
#include <stdio.h>
#include <gopython.h>

int main ()
{
    goString cmd;
    cmd = "import distutils.sysconfig; print distutils.sysconfig.get_config_var('BLDLIBRARY')\n";
    cmd += "import golib\nfrom golib import *\n";
    cmd += "M = goMatrixf()\nM.setIdentity()\nM._print()";
    goPython::init ();
    goPython::call (cmd);

    cmd = "l = [1,2,3,9,8,7]";
    goPython::call (cmd); 

    goVectorf v;
    PyObject* obj = goPython::getObject ("l");
    if (!obj)
    {
        printf ("Could not get object l!");
    }
    goPython::convert (obj, v);
    printf ("v: "); v.print ();

    goPython::final ();
    exit (1);
}
