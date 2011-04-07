#include "sprng.h"
#include "sprng_cpp.h"

void printRandInts(Sprng *rng, int num)
{
   int i;
   rng->print_sprng();
   for (i = 0; i < num; i++)
   {
      printf ("%d\n", rng->isprng());
   }
}

void printRandDoubles(Sprng *rng, int num)
{
   int i;
   rng->print_sprng();
   for (i = 0; i < num; i++)
   {
      printf ("%.16lf\n", rng->sprng());
   }
}

int main (void)
{
   int seed = 42;
   Sprng * gen1, **gens;
   gen1 = SelectType(SPRNG_LFG);
   gen1->init_sprng(0, 1, seed, 0);
   gen1->spawn_sprng(3, &gens);
   printRandInts(gen1, 10);
   printRandDoubles(gen1, 10);
   printRandInts(gens[0], 10);
   printRandDoubles(gens[0], 10);
   printRandInts(gens[1], 10);
   printRandDoubles(gens[1], 10);
   printRandInts(gens[2], 10);
   printRandDoubles(gens[2], 10);
   return 0;
}
