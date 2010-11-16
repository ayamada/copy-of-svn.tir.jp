/* $Id$ */

/* linked list malloc
 * 効率は悪いが、まとめてfreeできる、テーブル式メモリ管理モジュール
 */

#ifndef _LLMALLOC_H_
#define _LLMALLOC_H_

#include <stdlib.h>

/* 構造的には、Gaucheのutil.queueと同じ */
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
