/* --*- C++ -*-- */
/* goRandom | random number generator */

#include <gorandom.h>
#include <stdlib.h>
#include <time.h>

float goRandom (bool init) {
  float r;
  if (init) srand ( (unsigned int) time ((time_t*) NULL));
  r = rand () / (float) RAND_MAX;
  return r;
}

void goRandomSeed (unsigned int seed) {
  if (seed != 0) srand (seed);
}
