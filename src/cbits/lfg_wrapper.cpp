#include "lfg_wrapper.h"

LFG * new_lfg(void)
{
   return new LFG;
}

void init_lfg(LFG *lfg, int streamnum, int nstreams, int seed, int pa)
{
   lfg->init_rng(streamnum, nstreams, seed, pa);
}

int get_rn_int_lfg(LFG *lfg)
{
   return lfg->get_rn_int();
}

float get_rn_flt_lfg(LFG *lfg)
{
   return lfg->get_rn_flt();
}

double get_rn_dbl_lfg(LFG *lfg)
{
   return lfg->get_rn_dbl();
}

void free_lfg(LFG *lfg)
{
   lfg->free_rng();
}
