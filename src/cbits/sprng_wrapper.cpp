#include "sprng_wrapper.h"

// Sprng * new_rng(Rng_type type)
// XXX should use the enum type instead.
Sprng * new_rng(int type)
{
   return SelectType(type);
}

void init_rng(Sprng *rng, int streamnum, int nstreams, int seed, int pa)
{
   rng->init_rng(streamnum, nstreams, seed, pa);
}

int get_rn_int(Sprng *rng)
{
   return rng->get_rn_int();
}

float get_rn_flt(Sprng *rng)
{
   return rng->get_rn_flt();
}

double get_rn_dbl(Sprng *rng)
{
   return rng->get_rn_dbl();
}

void free_rng(Sprng *rng)
{
   rng->free_rng();
}

void print_rng(Sprng *rng)
{
   rng->print_rng();
}

Sprng **spawn_rng(Sprng *rng, int num)
{
   Sprng **new_rngs;
   rng->spawn_rng(num, &new_rngs);
   return new_rngs;
}
