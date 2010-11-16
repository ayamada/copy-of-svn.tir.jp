/* $Id$ */

#include "llmalloc.h"


/* ToDo: あとで利用手順等を書く事 */

LLMalloc *llmalloc_table_format (LLMalloc *blank_table) {
  blank_table->head = NULL;
  blank_table->tail = NULL;
  return blank_table;
}

void *llmalloc_malloc (LLMalloc *table, size_t size) {
  void *newmemory = malloc(size);
  if (newmemory == NULL) return NULL;
  PtrLinkedList *newll = malloc(sizeof(PtrLinkedList));
  if (newll == NULL) return NULL;

  newll->ptr = newmemory;
  newll->size = size;
  newll->next = table->head;
  table->head = newll;
  if (table->tail == NULL) table->tail = newll; /* first only */

  return newmemory;
}

void llmalloc_free (LLMalloc *table, void *ptr) {
  if (table == NULL) return;
  if (table->head == NULL) return;
  if (table->tail == NULL) return;
  PtrLinkedList *cur = table->head;
  while (1) {
    /* ToDo: freeしたらそのセルは切り離すのを忘れてた。
     *       しかし、セルを切り離していいのか、ちょっと微妙。
     *       とりあえず仮に、ptrをNULLにするだけにしておく。
     *       (これならfreeall時にもう一回freeされても安全)
     */
    if (cur == NULL) break;
    if (cur->ptr == ptr) {
      free(cur->ptr);
      cur->ptr = NULL; /* 仮の処置 */
      return;
    }
    cur = cur->next;
  }
  return;
}

void llmalloc_table_freeall (LLMalloc *table) {
  if (table == NULL) return;
  if (table->head == NULL) return;
  if (table->tail == NULL) return;

  PtrLinkedList *cur = table->head;
  while (1) {
    PtrLinkedList *next = cur->next;
    free(cur->ptr);
    free(cur);
    if (next == NULL) break;
    cur = next;
  }
  /* cleanup */
  table->head = NULL;
  table->tail = NULL;
  return;
}


/* vim:set ft=c sw=2 ts=2 sts=2 et: */
