/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
