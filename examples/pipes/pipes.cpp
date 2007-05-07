#include <stdio.h>
#include <stdlib.h>
#include <goprocess.h>

#include <unistd.h>

int main ()
{
    goProcess process;

    int pipe1[2];   // 0: input 1: output
    int pipe2[2];

    if (pipe(pipe1) != 0)
    {
        printf ("Error opening pipe 1\n");
        exit(1);
    }
    if (pipe(pipe2) != 0)
    {
        printf ("Error opening pipe 2\n");
        exit(1);
    }

    //= Make the pipes the standard i/o file descriptors of the process.
    process.setInput(pipe1[0]);    // read end of pipe 1
    process.setOutput(pipe2[1]);   // write end of pipe 2

    goFixedArray<goString> cl (3);
    cl[0] = "-i";
    cl[1] = "-c";
    cl[2] = "\"gnuplot\"";
    process.run ("bash",cl);

    //=
    //= Close the old read/write ends which are now stdin/stdout of the child process.
    //=
    close (pipe1[0]);
    close (pipe2[1]);

    goString command = "test\npause -1\n";
    write(pipe1[1], command.getPtr(), command.getSize() * sizeof(char));
    goString buffer;
    buffer.resize (1024);
    while (read(pipe2[0],buffer.getPtr(),1024))
    {
        printf ("%s", buffer.toCharPtr());
    }

    exit(1);
}
