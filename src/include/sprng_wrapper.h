#ifndef SPRNG_WRAPPER_H
#define SPRNG_WRAPPER_H

#include "sprng.h"
#include "sprng_cpp.h"

extern "C" int sprng_default(void);
extern "C" int new_seed(void);
extern "C" Sprng * new_rng(int);
extern "C" void init_rng(Sprng *, int, int, int, int);
extern "C" int get_rn_int(Sprng *);
extern "C" float get_rn_flt(Sprng *);
extern "C" double get_rn_dbl(Sprng *);
extern "C" void free_rng(Sprng *);
extern "C" void print_rng(Sprng *);
extern "C" Sprng **spawn_rng(Sprng *, int);
extern "C" void free_spawn_buffer(Sprng **);

#endif
