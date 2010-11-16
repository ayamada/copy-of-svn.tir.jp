/* $Id$ */

#include "llmalloc.h"


/* ToDo: ���Ȥ����Ѽ������񤯻� */

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
    /* ToDo: free�����餽�Υ�����ڤ�Υ���Τ�˺��Ƥ���
     *       ��������������ڤ�Υ���Ƥ����Τ�������ä���̯��
     *       �Ȥꤢ�������ˡ�ptr��NULL�ˤ�������ˤ��Ƥ�����
     *       (����ʤ�freeall���ˤ⤦���free����Ƥ����)
     */
    if (cur == NULL) break;
    if (cur->ptr == ptr) {
      free(cur->ptr);
      cur->ptr = NULL; /* ���ν��� */
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
