#include <stdlib.h>
#include <stdio.h>
#include <gopython.h>

int main ()
{
    goPython gp;
    goString cmd;
    cmd = "import distutils.sysconfig; print distutils.sysconfig.get_config_var('BLDLIBRARY')\n";
    cmd += "import golib\nfrom golib import *\n";
    cmd += "M = goMatrixf()\nM.setIdentity()\nM._print()";
    gp.call (cmd);
    exit (1);
}
