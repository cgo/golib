#include <goprocess.h>
#include <godefs.h>

#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <string.h>

goProcess::goProcess ()
    : goObjectBase ()
{
    this->setClassID(GO_PROCESS);
    running = false;
}

goProcess::~goProcess () {
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
  if (id == 0) {
    return execlp (filename, filename, arg, NULL);
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
  if (id == 0) {
    return execvp (filename, argv);
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
goProcess::run (const char* filename, goFixedArray<goString> argv) {

  int id = (int)fork();
  running = true;
  if (id == 0) 
  {
      char **tmp;
      tmp = new char*[argv.getSize() + 2];
      tmp[0] = new char[strlen(filename)];
      strcpy (tmp[0], filename);
      for (int i = 1; i <= (int)argv.getSize(); i++) {
          tmp[i] = new char[argv[i-1].getSize()];
          strcpy (tmp[i], argv[i-1].toCharPtr());
      }
      tmp[argv.getSize() + 1] = NULL;
      int temp_id = execvp (filename, (char* const*)tmp);

      for (int i = 1; i < ((int)argv.getSize() + 2); i++) 
      {
          delete[] tmp[i];
          tmp[i] = 0;
      }
      delete[] tmp;
      tmp = 0;
      return temp_id;
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














