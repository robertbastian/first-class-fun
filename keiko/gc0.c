/*
 * gc0.c
 * 
 * This file is part of the Oxford Oberon-2 compiler
 * Copyright (c) 2006 J. M. Spivey
 * All rights reserved
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id: gc.c 1557 2010-01-24 20:59:31Z mike $
 */

#include "obx.h"

void *scratch_alloc(unsigned size, boolean atomic) {
     void *p = NULL;

     if (size % 4096 != 0)
          p = malloc(size);
     else {
          if (posix_memalign(&p, 4096, size) < 0) p = NULL;
     }

     if (p == NULL)
          panic("Out of memory");

     return p;
}

/* gc_init -- initialise everything */
int alloc_c = 0, dealloc_c = 0;
void gc_init(void) {
}

void gc_finish(void) {
  if (alloc_c != dealloc_c) {
    printf("Memory leak:\nAllocated:   %d bytes\nDeallocated: %d bytes\n", alloc_c, dealloc_c);
  }
}

// BEGIN HACK
void* alloc(unsigned size) {
  alloc_c += size;
  return malloc(size);
}

void dealloc(void* p, unsigned size) {
  dealloc_c += size;
  free(p);
}
// END HACK

value* make_env(value* cp, value* sp) {
  value* env = (value*) alloc(4*(AR_HEAD+cp[CP_FRAME].i));
  env[AR_REFC].i = 1;
  env[AR_CODE].p = cp;
  env[AR_SLINK].p = sp;
  // printf("%x->%x (%x) created\n", (unsigned) env, (unsigned) env[AR_SLINK].p, (unsigned) env[AR_CODE].p);
  return env;
}

void inc_ref_count(value* env) {
  if (env != 0) env[AR_REFC].i++;
}

void dec_all_ref_counts(value* env) {
  dec_ref_count(env[AR_SLINK].p);
  for (int i = 0, n = (env[AR_CODE].p)[CP_FRAME].i; i < n; i++) {
     if ((1 << i) & (env[AR_CODE].p)[CP_MAP].i) {
         value* p = (value *) getenvt(env[AR_HEAD+i].i);
         dec_ref_count(p);
     }
  } 
}

void dec_ref_count(value* env) {
  if (env != 0) {
    if (--env[AR_REFC].i == 0) {
      // printf("%x->%x (%x) deleted\n", (unsigned) env, (unsigned) env[AR_SLINK].p, (unsigned) env[AR_CODE].p);
      dec_all_ref_counts(env);
      dealloc(env, 4*(AR_HEAD+(env[AR_CODE].p)[CP_FRAME].i));
    }
  }
}
