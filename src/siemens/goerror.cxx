#include "goerror.h"

void
goError::print (const char *caller, const char *s) {
  cout << caller << ": golib error: " << s << endl;
}

void
goError::note (const char* caller, const char* s) {
  cout << caller << ": " << s << endl;
}
