;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; html�θ����ܤ�������ݻ�����ɬ�פ˱�����print out���륯�饹(���cgi��)
;;; print out������text.tree�ʤΤǡ�ɬ�פ˱�����tree->string����Ԥ�����
;;; ����ƥ�����Τ�print out����:body����������ǻ��ꤹ�롣

;;; (���ߤΤȤ����)�ǥե�����Ͱ���:
;;; - :encoding (symbol->string (gauche-character-encoding))
;;; - :robots "NOINDEX,NOFOLLOW"

(define-module tir03.cgi.html-render
  ;(use gauche.parameter)

  ;(use text.tree)
  ;(use text.html-lite)
  ;(use www.cgi)
  (use srfi-1)

  (use tir03.cgi)

  (export
    <html-render>
    spawn
    ;; �ץ��ȥ������ѥ᥽�å�
    render-to-html-tree
    render-to-cgi-tree
    render-to-http-tree
    ))
(select-module tir03.cgi.html-render)


;;; --------


(define *default-encoding* (symbol->string (gauche-character-encoding)))
(define *empty* (gensym))

(define (uniq src-list)
  ;; note: ���ΤȤ���eq?�ǤΤ�Ƚ���Ԥ����ͤȤ���
  (let loop ((left src-list)
             (result '()))
    (if (null? left)
      result
      (loop
        (cdr left)
        (if (memq (car left) result)
          result
          (cons (car left) result))))))

(define-macro (define-class-<html-render>)
  (eval
    `(define-class <html-render> ()
       ,(map
          (lambda (slot-symbol)
            `(,slot-symbol
               :init-keyword ,(make-keyword slot-symbol)
               :init-value *empty*))
          (uniq
            (append
              (get-html-tree-keyword-symbols)
              (get-http-tree-keyword-symbols)
              (get-cgi-tree-keyword-symbols)))))
    (current-module)))
(define-class-<html-render>)


(define-method initialize ((self <html-render>) initargs)
  (next-method)
  ;; ���ΤȤ����̵��
  )


;;; --------

;; ����åȥ���ܥ��list
(define *slots* (map (cut car <>)
                     (class-slots <html-render>)))

(define-method spawn ((original <html-render>) . override-keywords)
  (let1 new-instance (make <html-render>)
    (for-each
      (lambda (slot-symbol)
        (set!
          (ref new-instance slot-symbol)
          (get-keyword*
            (make-keyword slot-symbol)
            override-keywords
            (ref original slot-symbol))))
      *slots*)
    new-instance))


(define (render self render-proc slot-name-list extra-keywords)
  (define (process-keywords)
    ;; ��������:
    ;; - self���顢slot-name-list�ˤ���slot����������ͤ���Ф�
    ;; - ���κݤ˹��ˡ��ͤ�*empty*�Τ�ΤϽ�������
    ;; - �嵭�η�̤��Ф��ơ�extra-keywords�Ǿ�񤭤���
    ;; - �Ǹ�ˡ����ꥹ��åȤ�*empty*���ä��٤�¸�ߤ��ʤ����Τߡ�
    ;;   �ǥե�����ͤȤ����ɲ�(cons)���롣
    ;; �ºݤν���:
    ;; - �嵭�ν������פ�������Ū�ʤΤǡ��ʲ��Τ褦�˹Ԥ���
    ;; - �ʲ��Υꥹ�Ȥ��礹�롣
    ;; -- extra-keywords����
    ;; -- self��slot���⡢�ͤ�*empty*�ǤϤʤ���extra-keywords��̵�����
    ;; - �嵭�η�̤ˤ�����slot���ޤޤ�Ƥ��ʤ��ʤ顢
    ;;   ����slot��ǥե�����ͤȤ����ɲ�(cons)����
    (let1 interim-result (fold
                           (lambda (slot-symbol tail)
                             (let1 val (ref self slot-symbol)
                               (if (eq? val *empty*)
                                 tail
                                 (let1 key (make-keyword slot-symbol)
                                   (if (eq? *empty*
                                            (get-keyword key
                                                         extra-keywords
                                                         *empty*))
                                     (list* key val tail)
                                     tail)))))
                           extra-keywords
                           slot-name-list)
      (define (append-default-kv key default-value kv-list)
        (if (eq? *empty* (get-keyword key kv-list *empty*))
          (list* key default-value kv-list)
          kv-list))

      (append-default-kv :encoding *default-encoding*
                         (append-default-kv :robots "NOINDEX,NOFOLLOW"
                                            interim-result))))

  (let1 result-keywords (process-keywords)
    (apply render-proc result-keywords)))

(define-method render-to-html-tree ((self <html-render>) . extra-keywords)
  (render self html-tree-make (get-html-tree-keyword-symbols) extra-keywords))


(define-method render-to-cgi-tree ((self <html-render>) . extra-keywords)
  (render self cgi-tree-make (get-cgi-tree-keyword-symbols) extra-keywords))


(define-method render-to-http-tree ((self <html-render>) . extra-keywords)
  (render self http-tree-make (get-http-tree-keyword-symbols) extra-keywords))


;;; --------


(provide "tir03/cgi/html-render")
