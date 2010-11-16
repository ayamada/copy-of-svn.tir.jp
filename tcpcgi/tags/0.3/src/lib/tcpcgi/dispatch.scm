;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme sw=2 ts=2 et:
;;; $Id$

;;; ToDo : vhost��regexp������ʬ���ꡢư���ǧ���ޤ��礦��
;;; ToDo : apache��ServerAlias����

;;; ToDo : ��ǽ�ʤ顢lazy��ɾ����Ԥ��褦�ˤ�����
;;;        ��autoload�Τ褦�ˡ�ɬ�פ���ʬ�Τ߼¹Ԥ���褦�ˤ�������

(define-module tcpcgi.dispatch
  (use gauche.regexp) ; regexp-quote
  (use srfi-1) ; filter-map
  (use srfi-2) ; and-let*
  (use text.tree)
  (use util.list) ; alist->hash-table intersperse

  (export
    <tcpcgi.dispatch.vhost>
    <tcpcgi.dispatch.path>
    <tcpcgi.dispatch.none>
    make-dispatch-vhost
    make-dispatch-path
    make-dispatch-none
    dispatch.vhost->dispatch.path
    dispatch.path->dispatch.none
    dump-dispatch.none
    ))
(select-module tcpcgi.dispatch)




(define-class <tcpcgi.dispatch.vhost> ()
  (
   ;; key��vhost-name, value��<tcpcgi.dispatch.path>
   (dispatch-table :accessor dispatch-table-of
                   :init-keyword :dispatch-table
                   :init-value #f)
   ;; ��Υơ��֥��key�����뤫�ɤ���������ɽ���ˤ������
   (dispatch-regexp :accessor dispatch-regexp-of
                    :init-keyword :dispatch-regexp
                    :init-value #f)
   ))
(define-class <tcpcgi.dispatch.path> ()
  (
   ;; key��script-name, value��<tcpcgi.dispatch.none>
   (dispatch-table :accessor dispatch-table-of
                   :init-keyword :dispatch-table
                   :init-value #f)
   ;; ��Υơ��֥��key�����뤫�ɤ���������ɽ���ˤ������
   (dispatch-regexp :accessor dispatch-regexp-of
                    :init-keyword :dispatch-regexp
                    :init-value #f)
   ))
(define-class <tcpcgi.dispatch.none> ()
  (
   (cgi-target :accessor cgi-target-of
               :init-keyword :cgi-target
               :init-value #f)
   (nph :accessor nph-of
        :init-keyword :nph
        :init-value #f)
   ))



(define-syntax string-size-sort-maker
  (syntax-rules ()
    ((_ comparison)
     (lambda (target-list)
       (sort
         target-list
         (lambda (x y)
           (comparison (string-size x) (string-size y))))))))

(define string-size-sort
  (string-size-sort-maker <))
(define string-size-reverse-sort
  (string-size-sort-maker >))


;; 1:�����ޥå������̾����Ĺ����
;; 2:�磻��ɥ����ɥޥå������̾����Ĺ����
;; �ǡ�vhost-dispatch-regexp���������롣
;; â����fallback�ϥ��줫��������롣
;; �ǽ�Ū�ˡ��ʲ��Τ褦��ʸ�������������
;; "((?i:^aaa\\.bb)|(?i:^cc\\.dd)|(?i:\\.ggg\\.hh))(?:\\:\d+)?$"
;; ������vhost��ʸ����ϥ��������פ��Ƥ�������
;; ����ϡ�aaa.bb, cc.dd, *.ggg.hh�˥ޥå����롣
;; ���Ȥʤ�hash-table�Υ������㤨�С�"aaa.bb" "cc.dd" ".ggg.hh"����
;; (match-obj 1)��dispatch-hash������٤�̾���������롣
(define (make-dispatch-vhost-re ht)
  ;; �ޤ��������ޥå��ȥ磻��ɥ����ɥޥå�����Ĥ�ʬ�ह�롣
  ;; �Ĥ��ǤʤΤǡ������ʳ��ǡ�regexp-quote��Ԥ�
  ;; ��complete-vhost-list�Τߡ�Ƭ��^��Ĥ���ɬ�פ�����١�
  (receive (complete-vhost-list wildcard-vhost-list)
    (let loop ((paths
                 (string-size-sort ; ��̹��ۻ��˼�����reverse�����
                   (hash-table-keys ht)))
               (c-result '())
               (w-result '())
               )
      (if (null? paths)
        (values c-result w-result)
        (apply
          loop
          (cdr paths)
          (let1 path (car paths)
            (if (#/^\./ path)
              (list
                c-result
                (cons (regexp-quote path) w-result))
              (list
                (cons (list "^" (regexp-quote path)) c-result)
                w-result))))))
    ;; ������ĤΥꥹ�Ȥ�Ȥä�regexp���ۤ��롣
    (string->regexp
      (tree->string
        (list
          "("
          (intersperse
            "|"
            (map
              (lambda (item)
                (list
                  "(?i:" ; host̾����ʸ����ʸ������̤��ʤ�
                  item ; ����regexp-quote��
                  ")"))
              (append
                complete-vhost-list
                wildcard-vhost-list)))
          ")(?:\\:\d+)?$"))))) ; port-number��̵�뤹��





;; �ǽ�Ū�ˡ��ʲ��Τ褦��ʸ�������������
;; "^((?:/aaa/bbb)|(?:/ccc/ddd)|(?:/eee/fff))((?:\\/.*)?)$"
;; ��(match 1)��script-name����(match 2)��path-info�������Ǥ���
(define (make-dispatch-path-re ht)
  (string->regexp
    (tree->string
      (list
        "^("
        (intersperse
          "|"
          (map
            (lambda (item)
              (list
                "(?:"
                (regexp-quote item)
                ")"))
            (string-size-reverse-sort
              (hash-table-keys ht))))
        ")((?:\\/.*)?)$"))))




(define (make-dispatch-vhost alist)
  (and
    alist
    (list? alist)
    (not (null? alist))
    (let* ((ht (alist->hash-table
                 (map
                   (lambda (set)
                     (cons
                       (car set)
                       (make-dispatch-path (cadr set))))
                   alist)
                 'string=?))
           (re (make-dispatch-vhost-re ht))
           )
      (make <tcpcgi.dispatch.vhost>
        :dispatch-table ht
        :dispatch-regexp re
        ))))

(define (make-dispatch-path alist)
  (and
    alist
    (list? alist)
    (not (null? alist))
    (let* ((ht (alist->hash-table
                 (map
                   (lambda (set)
                     (cons
                       (car set)
                       (make-dispatch-none (cdr set))))
                   alist)
                 'string=?))
           (re (make-dispatch-path-re ht))
           )
      (make <tcpcgi.dispatch.path>
        :dispatch-table ht
        :dispatch-regexp re
        ))))

(define (make-dispatch-none param)
  (and
    (not (null? param)) ; '()�Ǥ�̵�������ݾ�
    (let* ((param-is-list? (list? param))
           (cgi-target (if param-is-list?
                         (car param)
                         param))
           (keywords (if param-is-list?
                         (cdr param)
                         '()))
           )
      (apply
        make <tcpcgi.dispatch.none>
        :cgi-target cgi-target
        keywords))))



;; (value dispatch-path server-name) ���֤���
;; �ޥå����ʤ��ä����ϡ�dispatch-path�Ȥ���#f���֤���
(define-method dispatch.vhost->dispatch.path ((dispatch-vhost
                                                <tcpcgi.dispatch.vhost>)
                                              target)
  (or
    (and-let* ((re (dispatch-regexp-of dispatch-vhost))
               (m (re target))
               (match-name (m 1))
               (dispatch-path
                 (ref (dispatch-table-of dispatch-vhost) match-name))
               )
      (values
        dispatch-path
        target))
    (values #f #f)))


;; (value dispatch-none script-name plain-path-info) ���֤���
;; �ޥå����ʤ��ä����ϡ�dispatch-none�Ȥ���#f���֤���
;; ��â����"/"�Τߤλ��꤬���ä����ϡ����̰����Ȥ��ơ�
;;   path-info�ǤΥޥå��󥰤�Ԥ�ʤ���
(define-method dispatch.path->dispatch.none ((dispatch-path
                                               <tcpcgi.dispatch.path>)
                                             target)
  (or
    (and-let* ((re (dispatch-regexp-of dispatch-path))
               (m (re target))
               (script-name (m 1))
               (plain-path-info (m 2))
               (not-root? (or
                            ;; script-name��/�Ǥʤ��ʤ�����̵��
                            (not (string=? "/" script-name))
                            ;; script-name��/�ǡ�path-info��̵���Τ�ok
                            ;; �������������Ǥʤ��ʤ�Ѳ�
                            (string=? "" plain-path-info)))
               (dispatch-none
                 (ref (dispatch-table-of dispatch-path) script-name))
               )
      (values
        dispatch-none
        script-name
        (and
          (not (string=? "" plain-path-info))
          plain-path-info)))
    (values #f #f #f)))


;; (list cgi-target :nph boolean) ���֤���
;; ���������ʬ�Ϻ����ɲäˤʤ��auth������ɤȤ��ˡ�
(define-method dump-dispatch.none ((dispatch-none <tcpcgi.dispatch.none>))
  (list
    (cgi-target-of dispatch-none)
    :nph (nph-of dispatch-none)
    ))



(provide "tcpcgi/dispatch")

