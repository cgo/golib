#include "goerror.h"
class goString;

void
goError::print (const char *caller, const char *s) {
  cout << caller << ": golib error: " << s << endl;
}

void
goError::print (const char *caller, const goString& s) {
  cout << caller << ": golib error: " << s << endl;
}

void
goError::note (const char* caller, const char* s) {
  cout << caller << ": " << s << endl;
}

void
goError::note (const char* caller, const goString& s) {
  cout << caller << ": " << s << endl;
}
