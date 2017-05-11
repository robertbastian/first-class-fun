#include "obx.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

unsigned prim_check = 0;

#define args (fp + HEAD)

static void Lib_Print(value *sp) {
     value *fp = sp;
     printf(" %d", args[0].i);
}

static void Lib_Print_B(value *fp) {
     printf(args[0].i ? " TRUE" : " FALSE");
}

char fmt[10];
static void Lib_Print_F(value *fp) {
     sprintf(fmt, " %%.%df", args[1].i);
     printf(fmt, args[0].i/pow(10,args[1].i));
}

static void Lib_Newline(value *sp) {
     printf("\n");
}

int rand_init = FALSE;
static void Lib_Rand(value *fp) {
      if (!rand_init) {
        srand(getenv("IS_TEST") ? 0 : time(NULL));
        rand_init = TRUE;
      }
      ob_res.i = rand() % args[0].i;
}

void dltrap(value *sp) {
     fprintf(stderr, "Oops: dltrap called!\n");
     exit(2);
}

primitive *primtab[] = {
     interp, dltrap, Lib_Print, Lib_Print_B, Lib_Newline, Lib_Print_F, Lib_Rand,
     NULL
};

char *primname[] = {
     "INTERP", "DLTRAP", "Lib_Print", "Lib_Print_B", "Lib_Newline", "Lib_Print_F",
     "Lib_Rand"
};

