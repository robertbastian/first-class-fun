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
value* heap0;
value* heap0_old;
value* heap;
value* scratch;
value* heap_end;
value* scratch_end;
int heap_size = 512;
void gc_init(void) {
  heap0 = heap0_old = (value*) calloc(4, heap_size);
  heap = heap0;
  scratch = heap0 + heap_size/2;
  heap_end = heap;
  scratch_end = scratch;
}


#define RED   "\x1B[31m"
#define GRN   "\x1B[32m"
#define RESET "\x1B[0m"
void gc_dump() {
  printf("\n%x\n", (unsigned) heap);
  for (value* hp = heap; hp < heap_end; hp++){
    printf(GRN "%08x ", hp[0].i);
  }
  for (value* hp = heap_end; hp < heap + heap_size/2; hp++){
    printf(RESET "%08x ", 0);
  } printf("\n%x\n", (unsigned) scratch);

  for (value* hp = scratch; hp < scratch_end; hp++){
    printf(RED "%08x ", hp[0].i);
  } 
  for (value* hp = scratch_end; hp < scratch + heap_size/2; hp++){
    printf(RESET "%08x ", 0);
  } printf("\n\n");
}

void gc_finish(void) {
  free((void*) heap0);
}

value* alloc(unsigned words, value* fp) {
  if (&heap_end[words] >= heap + heap_size/2){
    gc_collect(fp);
  }
  if (&heap_end[words] >= heap + heap_size/2){
    panic("heap to small");
  }
  value* p = heap_end;
  heap_end += words;
  return p;
}

#define vars(env) ((env[AR_CODE].p)[CP_FRAME].i)
#define map(env) ((env[AR_CODE].p)[CP_MAP].i)
#define is_ref(i, map) ((1 << (i)) & map)
#define size(env) AR_HEAD+vars(env)

int total_words;
void gc_mark_from_p(value* p) {
  total_words += size(p);
  p[AR_BKPTR].p = NULL;

  // In AR_MARK we keep track of how many of the variables we have already
  //  followed. In AR_BKPTR we keep track of where to go back to once we 
  //  finished discovering this node.
  while (p != NULL) {
    if (p[AR_MARK].i == 0) {
      // printf("Marking %02x\n",p-heap0);
      p[AR_MARK].i = 1;
      if (p[AR_SLINK].p != NULL && (p[AR_SLINK].p)[AR_MARK].i == 0) {
        (p[AR_SLINK].p)[AR_BKPTR].p = p;
        p = p[AR_SLINK].p;
      }
    } else {
      //     offset is available          not a packed var
      while (p[AR_MARK].i-1 < vars(p) && (!(is_ref(p[AR_MARK].i-1, map(p))) || 
        // null pointer
        getenvt(p[AR_HEAD+p[AR_MARK].i-1].i, heap0_old) == NULL ||
        // already discovered
        getenvt(p[AR_HEAD+p[AR_MARK].i-1].i, heap0_old)[AR_MARK].i)){
          p[AR_MARK].i++;
      }
      if (p[AR_MARK].i-1 < vars(p)) {
        value* pn = getenvt(p[AR_HEAD+p[AR_MARK].i-1].i, heap0_old);
        pn[AR_BKPTR].p = p;
        p = pn;
      } else if (p[AR_MARK].i-1 == vars(p)) {
        // have discovered all of this env
        p = p[AR_BKPTR].p;
      }
    }
  }
}

void gc_mark(value* fp) {

  total_words = 0;
  value* estack = &fp[4+(fp[CP].p)[CP_STACK].i];

  if (fp[HEAD].p) gc_mark_from_p(fp[HEAD].p);
  // frame is just being created, arguments are still on the stack
  for (int i = 0; &fp[4+i] < estack; i++){
    if (is_ref(i, (fp[CP].p)[CP_MAP].i)) {
      gc_mark_from_p(getenvt(fp[4+i].i, heap0_old));
    }
  }

  fp = fp[FP].p;
  while (fp[FP].p != NULL) {
    if (fp[HEAD].p) gc_mark_from_p(fp[HEAD].p);
    while (estack < fp){
      if (might_be_packed(estack->i, heap0_old)){
        gc_mark_from_p(getenvt(estack->i, heap0_old));
      }
      estack++;
    }
    estack += FRAME_SHIFT / 4; //skip frame header 
    fp = fp[FP].p;
  }
}

void redirect(value* x) {
  if (x->p != NULL) {
    // printf("@%p: %x -> %x\n", x, x->p-heap0, x->p[AR_BKPTR].p-heap0); 
    x->p = x->p[AR_BKPTR].p;
  }
}

void redirect_pack(value* x) {
  unsigned old = x->i;
  value* env = getenvt(old, heap0_old);
  value* new = env ? env[AR_BKPTR].p : NULL;
  if (env){
    x->i = pack(getcode(old, heap0_old), new, heap0);
  }
  // printf("@%x: %x -u> %x -> %x -p> %x\n", x-heap0, old, env-heap0, new-heap0, x->i);
}


void gc_redirect_stack(value* fp) {

  value* estack = &fp[4+(fp[CP].p)[CP_STACK].i];

  redirect(&fp[HEAD]);
  // frame is just being created, arguments are still on the stack
  for (int i = 0; &fp[4+i] < estack; i++){
    if (is_ref(i, (fp[CP].p)[CP_MAP].i)) {
      redirect_pack(&fp[4+i]);
    }
  }

  fp = fp[FP].p;
  while (fp[FP].p != NULL) {
    redirect(&fp[HEAD]);
    while (estack < fp){
      if (estack->p >= heap && estack->p <= heap+(heap0_old==heap0 ? heap_size / 2 : heap_size)) {
        redirect(estack);
      }
      if (might_be_packed(estack->i, heap0_old)){
        printf("ATTENTION SOMETHING MIGHT BE PACKED\n");
        redirect_pack(estack);
      }
      estack++;
    }
    estack += FRAME_SHIFT / 4; //skip frame header 
    fp = fp[FP].p;
  }
}

void gc_redirect_heap() {

  value* p = scratch;
  while (p < scratch_end) {
    p[AR_MARK].i = 0;
    redirect(&p[AR_SLINK]);

    for (int i = 0, n = vars(p); i < n; i++) {
      if (is_ref(i, map(p))) redirect_pack(&p[AR_HEAD+i]);
    }
    p += size(p);
  }
}

void gc_move_space(){
  value* heappointer = heap;
  value* scratchpointer = scratch;

  // go through all of the heap and copy active parts to scratch
  while (heappointer < heap_end) {
    // if pointer is still accessible
    if (heappointer[AR_MARK].i) {
      // move into scratch
      // printf("Copying %d bytes %02x -> %02x\n", size(heappointer), heappointer - heap0, scratchpointer -heap0);
      for (int i = 0; i < size(heappointer); i++) {
        // if (heappointer - heap0 == 0x40 && scratchpointer - heap0 == 0x3e && i==4) gc_dump();
        scratchpointer[i].i = heappointer[i].i;
      }
      // store new location
      heappointer[AR_BKPTR].p = scratchpointer;
      scratchpointer += size(heappointer);
    }
    heappointer += size(heappointer);
  }
  scratch_end = scratchpointer;
  heap_end = heappointer;
}


void gc_collect(value* fp) {
  // gc_dump();

  gc_mark(fp);

  if (total_words * 4 / 3 > heap_size / 2){
    // over 75% used, double
    heap_size *= 2;
    heap0_old = heap0;
    heap0 = (value*) calloc(4, heap_size);
    memset((uchar*) heap0, 0, 4*heap_size);
    scratch = scratch_end = heap0;
  }

  // gc_dump();

  gc_move_space();

  // gc_dump();

  gc_redirect_stack(fp);
  gc_redirect_heap();

  // gc_dump();

  if (heap0_old != heap0){
    free((void*) heap0_old);
    heap0_old = heap0;
    heap = scratch;
    heap_end = scratch_end;
    scratch = scratch_end = heap + heap_size / 2;
  } else {
    value* t = heap;
    heap = scratch;
    heap_end = scratch_end;
    scratch = scratch_end = t;
  }

  // gc_dump();
  // printf("DONE COLLECTING\n");
}


value* make_env(value* fp) {
  value* env = alloc(AR_HEAD+(fp[CP].p)[CP_FRAME].i, fp);
  env[AR_MARK].i = 0;
  env[AR_CODE].p = fp[CP].p;
  env[AR_SLINK].p = fp[HEAD].p;
  return env;
}
