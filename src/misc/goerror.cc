#include "goerror.h"
class goString;

void
goError::print (const char *caller, const char *s) {
    std::cout << caller << ": golib error: " << s << std::endl;
}

void
goError::print (const char *caller, const goString& s) {
    std::cout << caller << ": golib error: " << s << std::endl;
}

void
goError::note (const char* caller, const char* s) {
    std::cout << caller << ": " << s << std::endl;
}

void
goError::note (const char* caller, const goString& s) {
    std::cout << caller << ": " << s << std::endl;
}
