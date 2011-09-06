/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goprocess.h>
#include <godefs.h>

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>

goProcess::goProcess ()
    : goObjectBase (),
      running (false),
      pid (0),
      inputFD (-1),
      outputFD (-1),
      tempArgs (0),
      argCount (0)
{
    this->setClassID(GO_PROCESS);
    running = false;
}

goProcess::~goProcess () 
{
    if (this->inputFD >= 0)
    {
        ::close (this->inputFD);
    }
    if (this->outputFD >= 0)
    {
        ::close (this->outputFD);
    }
    this->deleteTemp ();
}

void goProcess::setInput (int fd)
{
    if (this->inputFD >= 0)
    {
        ::close (this->inputFD);
    }
    this->inputFD = fd;
}

void goProcess::setOutput (int fd)
{
    if (this->outputFD >= 0)
    {
        ::close (this->outputFD);
    }
    this->outputFD = fd;
}

void goProcess::deleteTemp ()
{
    if (tempArgs)
    {
        for (int i = 0; i < argCount; ++i) 
        {
            delete[] tempArgs[i];
            tempArgs[i] = 0;
        }
        delete[] tempArgs;
        tempArgs = 0;
    }
}

void goProcess::redirectIO ()
{
    if (this->inputFD >= 0)
    {
        dup2 (this->inputFD, STDIN_FILENO);
    }
    if (this->outputFD >= 0)
    {
        dup2 (this->outputFD, STDOUT_FILENO);
    }
}

/**
 * @brief Calls a program by filename.
 *
 * @param filename  Filename of the program.
 * @param arg  Argument string for the program. 
 *
 * @return The PID of the new process.
 **/
int
goProcess::run (const char* filename, const char* arg) {
  int id = fork();
  running = true;
  if (id == 0) 
  {
      this->redirectIO ();
      if (execlp (filename, filename, arg, NULL) < 0)
      {
          //= An error has occurred.
          running = false;
          exit (-1);
      }
  }
  pid = id;
  return id;
}

/**
 * @brief Runs a program by filename.
 *
 * @param filename  Filename of the program.
 * @param argv      Array of arguments for the program.
 *
 * @return The PID of the new process.
 **/
int
goProcess::run (const char* filename, char *const argv[]) {
  int id = fork();
  running = true;
  if (id == 0) 
  {
      this->redirectIO ();
      if (execvp (filename, argv) < 0)
      {
          //= An error has occurred.
          running = false;
          exit (-1);
      }
  }
  pid = id;
  return id;
}

/**
 * @brief Runs a program by filename.
 *
 * @param filename  Filename of the program.
 * @param argv      goArray of string pointers to the arguments.
 *
 * @return The PID of the new process.
 **/
int 
goProcess::run (const char* filename_, const goFixedArray<goString>& argv_) 
{
  goFixedArray<goString> argv = argv_;
  goString filenameString = filename_;
  const char* filename = filenameString.toCharPtr();
  int id = (int)fork();
  running = true;
  if (id == 0) 
  {
      this->redirectIO ();
      this->deleteTemp ();
      tempArgs = new char*[argv.getSize() + 2];
      tempArgs[0] = new char[strlen(filename)];
      this->argCount = argv.getSize() + 2;
      strcpy (tempArgs[0], filename);
      for (int i = 1; i <= (int)argv.getSize(); i++) {
          tempArgs[i] = new char[argv[i-1].getSize()];
          strcpy (tempArgs[i], argv[i-1].toCharPtr());
      }
      tempArgs[argv.getSize() + 1] = NULL;
      if (execvp (filename, (char* const*)tempArgs) < 0)
      {
          //= Returns at all, so this must be an error.
          running = false;
          this->deleteTemp ();
          exit (-1);
      }
  }
  pid = id;
  return id;
}

/**
 * @brief Waits for the process to return.
 **/
void
goProcess::wait () {
  int status;
  if (running) {
    waitpid (pid, &status, 0);
  }
  running = false;
}

/**
 * @brief Ends the child process by sending a SIGKILL signal.
 **/
void
goProcess::kill () {
  if (running) {
    if (::kill (pid, SIGKILL) != 0) {
        std::cout << "Unable to kill process " << pid << "\n";
    } else {
      wait ();
    }
  }
  running = false;
}

/**
 * @brief Suspends the child process.
 *
 * Resume it with the resume() method.
 **/
void
goProcess::suspend () {
  if (running) {
    if (::kill (pid,SIGSTOP) != 0) {
        std::cout << "unable to stop process " << pid << "\n";
    }
  }
}

/**
 * @brief Resumes the suspended child process.
 **/
void
goProcess::resume () {
  if (running) {
    if (::kill (pid,SIGCONT) != 0) {
        std::cout << "unable to continue process " << pid << "\n";
    }
  }
}
