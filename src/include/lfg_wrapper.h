#ifndef LFG_WRAPPER_H
#define LFG_WRAPPER_H

#include "sprng.h"
#include "lfg.h"

extern "C" LFG * new_lfg(void);
extern "C" void init_lfg(LFG *, int, int, int, int);
extern "C" int get_rn_int_lfg(LFG *);
extern "C" float get_rn_flt_lfg(LFG *);
extern "C" double get_rn_dbl_lfg(LFG *);
extern "C" void free_lfg(LFG *);

#endif
