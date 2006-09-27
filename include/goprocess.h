#ifndef GOPROCESS_H
#define GOPROCESS_H

#include <gotypes.h>
#ifndef GOFIXEDARRAY_H
# include <gofixedarray.h>
#endif
#include <gostring.h>
#include <unistd.h>
#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif

/*!
 * \addtogroup system
 * @{
 */
/*!
 * \brief Process interface for Unix platforms (so far). 
 *
 * The OS has to provide the necessary exec* functions and a fork() function.
 * I recommend using the run ( ..., goArray<goString* >& argv) member for 
 * starting the child process.
 * In this case, only the real parameters have to be provided by the user.
 * When using the run(const char* filename, char *const argv[]) member, 
 * the user has to provide argv[0] 
 * as the programname (as usual) and the array has to be ended by 
 * a NULL pointer.
 *
 * \todo If needed, add Windoze support.
 *
 * @author Christian Gosch
 */
class goProcess : public goObjectBase {
 public:
  ///
  goProcess ();
  ///
  virtual ~goProcess ();
  
  ///
  int run (const char* filename, const char* arg);
  ///
  int run (const char* filename, char *const argv[]);
  ///
  int run (const char* filename, const goFixedArray<goString>& argv);
  /// Waits until the child returns.
  void wait ();
  /// Kills the child using SIGKILL.
  void kill ();
  ///
  void suspend ();
  ///
  void resume ();
 protected:

  void deleteTemp ();

  bool running;
  int pid;
    
 private:
  char **tempArgs;
  int argCount;
};
/*!
 * @}
 */


#endif













