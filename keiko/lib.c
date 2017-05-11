#include "obx.h"
#include <stdio.h>

unsigned prim_check = 0;

#define args (fp + HEAD + 1)

static void Lib_Print(value *sp) {
     value *fp = sp;
     printf(" %d", args[0].i);
}

static void Lib_Print_B(value *fp) {
     printf(args[0].i ? " TRUE" : " FALSE");
}

static void Lib_Newline(value *sp) {
     printf("\n");
}

void dltrap(value *sp) {
     fprintf(stderr, "Oops: dltrap called!\n");
     exit(2);
}

primitive *primtab[] = {
     interp, dltrap, Lib_Print, Lib_Print_B, Lib_Newline,
     NULL
};

char *primname[] = {
     "INTERP", "DLTRAP", "Lib_Print", "Lib_Print_B", "Lib_Newline"
};

