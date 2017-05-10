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

void* alloc(unsigned size) {
  alloc_c += size;
  return malloc(size);
}

void dealloc(void* p, unsigned size) {
  dealloc_c += size;
  // free(p);
}

value* make_env(value* cp) {
  value* env = (value*) alloc(4*(CL_HEAD+getcount(cp[CP_CAPTS].i)));
  env[CL_REFC].i = 1;
  env[CL_CODE].p = cp;
  return env;
}

void inc_all_ref_counts(value* env) {
  value* p = env[CL_CODE].p;
  int size = getcount(p[CP_CAPTS].i);
  int map = getmap(p[CP_CAPTS].i);
  for (int i = 0; i < size; i++) {
     if ((1 << i) & map) inc_ref_count(env[CL_HEAD+i].p);
  } 
}

void inc_ref_count(value* env) {
  if (env != 0) {
    env[CL_REFC].i++;
  }
}

void free_range(value* start, int length, int map) {
  for (int i = 0; i < length; i++) {
     if ((1 << i) & map) dec_ref_count(start[i].p);
  }
}

void dec_ref_count(value* env) {
  if (env != 0) {
    if (--env[CL_REFC].i == 0) {
      value* p = env[CL_CODE].p;
      free_range(&env[CL_HEAD], getcount(p[CP_CAPTS].i), getmap(p[CP_CAPTS].i));
      dealloc(env, 4*(CL_HEAD+getcount(p[CP_CAPTS].i)));
    }
  }
}
