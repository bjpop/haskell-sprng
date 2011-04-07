#include "sprng_wrapper.h"

Sprng * new_rng(int type)
{
   return SelectType(type);
}

void init_rng(Sprng *rng, int streamnum, int nstreams, int seed, int param)
{
   rng->init_rng(streamnum, nstreams, seed, param);
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

void free_spawn_buffer(Sprng **buffer)
{
   free(buffer);
}
