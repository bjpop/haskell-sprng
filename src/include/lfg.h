#ifndef _lfg_h
#define _lfg_h

typedef struct
{
  int rng_type;
  char *gentype;
  unsigned *si;      // sets next branch seed
  unsigned *r0;      // pointer to the even generator
  unsigned *r1;      // pointer to the odd generator
  int stream_number;
  int hptr;          // integer pointer into fill
  int seed;
  int init_seed;
  int lval, kval;
  int param;
} LFG;

extern int get_rn_int(LFG *);
extern double get_rn_dbl(LFG *);
extern float get_rn_flt(LFG *);
extern LFG *new_rng(void);
extern void free_rng(LFG *);
extern int init_rng(LFG *, int, int, int, int);
extern LFG **spawn_rng_wrapper(LFG *, int);
extern void free_spawn_buffer(LFG **);

#endif
