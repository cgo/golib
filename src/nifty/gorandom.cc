/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


/* --*- C++ -*-- */
/* goRandom | random number generator */

#include <gorandom.h>
#include <stdlib.h>
#include <time.h>

/**
 * @brief Random number generation.
 *
 * @param init  If true, random generator will be initialized using the current
 *              time. If false, random generator will not be initialized.
 *
 * @return A random number between 0.0 and 1.0
 **/
float goRandom (bool init) {
  float r;
  if (init) srand ( (unsigned int) time ((time_t*) NULL));
  r = rand () / (float) RAND_MAX;
  return r;
}

/**
 * @brief Initializes the random generator.
 *
 * @param seed  Seed to initialize the random generator with.
 **/
void goRandomSeed (unsigned int seed) {
  if (seed != 0) srand (seed);
}
