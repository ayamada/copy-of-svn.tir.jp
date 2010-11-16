/* $Id$ */

/* linked list malloc
 * $B8zN($O0-$$$,!"$^$H$a$F(Bfree$B$G$-$k!"%F!<%V%k<0%a%b%j4IM}%b%8%e!<%k(B
 */

#ifndef _LLMALLOC_H_
#define _LLMALLOC_H_

#include <stdlib.h>

/* $B9=B$E*$K$O!"(BGauche$B$N(Butil.queue$B$HF1$8(B */
typedef struct _PtrLinkedList {
  void *ptr;
  size_t size;
  struct _PtrLinkedList *next;
} PtrLinkedList;

typedef struct _LLMalloc {
  PtrLinkedList *head;
  PtrLinkedList *tail;
} LLMalloc;

LLMalloc *llmalloc_table_format (LLMalloc *blank_table);
void *llmalloc_malloc (LLMalloc *table, size_t size);
void llmalloc_free (LLMalloc *table, void *ptr);
void llmalloc_table_freeall (LLMalloc *table);


#endif /* _LLMALLOC_H_ */

/* vim:set ft=c sw=2 ts=2 sts=2 et: */
