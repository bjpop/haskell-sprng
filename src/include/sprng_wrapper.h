#ifndef SPRNG_WRAPPER_H
#define SPRNG_WRAPPER_H

#include "sprng.h"
#include "sprng_cpp.h"

typedef enum { LFG, LCG, LCG64, CMRG, MLFG, PMLCG } Rng_type;

//extern "C" Sprng * new_rng(Rng_type);
extern "C" Sprng * new_rng(int);
extern "C" void init_rng(Sprng *, int, int, int, int);
extern "C" int get_rn_int(Sprng *);
extern "C" float get_rn_flt(Sprng *);
extern "C" double get_rn_dbl(Sprng *);
extern "C" void free_rng(Sprng *);
extern "C" void print_rng(Sprng *);

#endif
