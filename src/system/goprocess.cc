#include <goprocess.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <string.h>

goProcess::goProcess () {
  running = false;
}

goProcess::~goProcess () {
}

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

int 
goProcess::run (const char* filename, goArray<goString* >& argv) {
  char **tmp;
  tmp = new char*[argv.getSize() + 2];
  tmp[0] = new char[strlen(filename)];
  strcpy (tmp[0], filename);
  for (int i = 1; i <= argv.getSize(); i++) {
    tmp[i] = new char[argv[i-1]->getSize()];
    strcpy (tmp[i], argv[i-1]->toCharPtr());
  }
  tmp[argv.getSize() + 1] = NULL;

  int id = fork();
  running = true;
  if (id == 0) {
    return execvp (filename, (char* const*)tmp);
  }

  for (int i = 1; i < (argv.getSize() + 2); i++) {
    delete[] tmp[i];
  }
  delete[] tmp;
  pid = id;
  return id;
}

void
goProcess::wait () {
  int status;
  if (running) {
    waitpid (pid, &status, 0);
  }
  running = false;
}

void
goProcess::kill () {
  if (running) {
    if (::kill (pid, SIGKILL) != 0) {
      cout << "Unable to kill process " << pid << "\n";
    } else {
      wait ();
    }
  }
  running = false;
}

void
goProcess::suspend () {
  if (running) {
    if (::kill (pid,SIGSTOP) != 0) {
      cout << "unable to stop process " << pid << "\n";
    }
  }
}

void
goProcess::resume () {
  if (running) {
    if (::kill (pid,SIGCONT) != 0) {
      cout << "unable to continue process " << pid << "\n";
    }
  }
}














