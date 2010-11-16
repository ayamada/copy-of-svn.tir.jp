;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$



(define-module tcpcgi.dispatch
  (use gauche.parameter)
  (use gauche.regexp) ; regexp-quote
  (use srfi-1) ; filter-map
  (use srfi-2) ; and-let*
  (use rfc.uri)
  (use text.tree)
  (use util.list) ; alist->hash-table intersperse
  (use www.cgi)

  (use tcpcgi.execute)
  (export
    <tcpcgi.dispatch>

    make-dispatch-for-fallback
    make-dispatch-for-path
    make-dispatch-for-vhost

    dispatch-from-fallback ; fallback�ǥǥ����ѥå���Ԥ�����̤�����
    dispatch-from-path ; path�ǥǥ����ѥå���Ԥ�����̤�����
    dispatch-from-vhost ; vhost�ǥǥ����ѥå���Ԥ�����̤�����
    ))
(select-module tcpcgi.dispatch)


;; utility
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

(define (alist? alist)
  (and
    alist
    (null? alist)
    (list? (car alist))))



(define-class <tcpcgi.dispatch> ()
  (
   (dispatch-type :accessor dispatch-type-of
                  :init-keyword :dispatch-type
                  :init-value #f) ; 'vhost 'path 'fallback �Τɤ줫

   ;; �ʲ��ϡ������ѥ���å�
   (dispatch-execute :accessor dispatch-execute-of
                     :init-keyword :dispatch-execute
                     :init-value #f)
   (dispatch-table :accessor dispatch-table-of
                   :init-keyword :dispatch-table
                   :init-value #f)
   ;; ��Υơ��֥��key������ɽ����������Ρ�
   ;; �ޥå��󥰸塢(match-obj 1)��Ȥäƾ�Υơ��֥������Ȥ������ͤ���ա�
   (dispatch-regexp :accessor dispatch-regexp-of
                    :init-keyword :dispatch-regexp
                    :init-value #f)
   ))




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








;; note :
;; - make-dispatch-for-*�ˤϡ�ɬ����Ĥΰ������Ϥ�(keyword�Ϥ��ʤΤ�)��
;; - make-dispatch-for-*�ΰ���target�ϡ��ʲ��Τɤ줫��
;; -- #f
;; --- ���⤻���ˡ�#f���֤�
;; -- thunk
;; --- `(script ,thunk) �ξ�ά��
;; -- ʸ����
;; --- `(filer ,ʸ����) �ξ�ά��
;; -- list
;; --- (make <tcpcgi.execute>) ��keyword�Ȥ����Ϥ��ꥹ��
;; -- alist
;; --- vhost, path�ι��ۻ��˻Ȥ�
;; -- null list
;; --- #f���֤�������alist���ä����ˡ����줬�Ϥ�����ǽ��������

(define (make-dispatch-for-fallback target)
  (if (or (not target) (null? target))
    #f
    (if (alist? target)
      (error "invalid target")
      (let1 e (make <tcpcgi.execute>
                :execute-list target)
        (and
          e
          (make <tcpcgi.dispatch>
            :dispatch-type 'fallback
            :dispatch-execute e))))))
(define (make-dispatch-for-path target)
  (if (or (not target) (null? target))
    #f
    (if (not (alist? target))
      (make-dispatch-for-fallback target)
      (let* ((ht (alist->hash-table
                   (map
                     (lambda (key&vals)
                       (cons
                         (car key&vals)
                         (make-dispatch-for-fallback (cdr key&vals))))
                     target)
                   'string=?))
             (re (make-dispatch-path-re ht))
             )
        (make <tcpcgi.dispatch>
          :dispatch-type 'path
          :dispatch-table ht
          :dispatch-reqexp re)))))
(define (make-dispatch-for-vhost target)
  (if (or (not target) (null? target))
    #f
    (if (not (alist? target))
      (make-dispatch-for-fallback target)
      (let* ((ht (alist->hash-table
                   (map
                     (lambda (key&vals)
                       (cons
                         (car key&vals)
                         (if (alist? (cadr key&vals))
                           (make-dispatch-for-path (cadr key&vals))
                           (make-dispatch-for-fallback (cdr key&vals)))))
                     target)
                   'string=?))
             (re (make-dispatch-vhost-re ht))
             )
        (make <tcpcgi.dispatch>
          :dispatch-type 'vhost
          :dispatch-table ht
          :dispatch-reqexp re)))))







(define-method dispatch-from-fallback ((self <tcpcgi.dispatch>))
  ;; executee�ޤ���#f���֤�
  (dispatch-execute-of self))

(define-method dispatch-from-path ((self <tcpcgi.dispatch>) target-path)
  ;; (list executee script-name plain-path-info)�ޤ���#f���֤�
  (and-let* ((ht (dispatch-table-of self))
             (re (dispatch-regexp-of self))
             (m (re target-path))
             (script-name (m 1))
             (pre-plain-path-info (m 2))
             (executee (hash-table-get ht script-name))
             )
    (let1 plain-path-info (if (string=? "" pre-plain-path-info)
                            #f
                            pre-plain-path-info)
      (if (and
            (string=? "/" script-name)
            plain-path-info)
        #f ; script-name��"/"�λ��Τߡ�path-info�����ʤ��褦�ˤ���ɬ�פ�����
        (list executee script-name plain-path-info)))))

(define-method dispatch-from-vhost ((self <tcpcgi.dispatch>) target-host
                                                             target-path)
  ;; (list executee script-name plain-path-info server-name)�ޤ���#f
  ;; �ޤ���server-name�Τ�(vhost�ϥޥå�������path�ޥå����Ի�)���֤�
  (and-let* ((ht (dispatch-table-of self))
             (re (dispatch-regexp-of self))
             (server-name (or
                            (and-let* ((m (#/\:\d+$/ target-host)))
                              (m 'before))
                            target-host))
             (m (re server-name))
             (match-name (m 1))
             (path-instance (hash-table-get ht match-name))
             )
    (let1 dispatch-path-list (dispatch-from-path path-instance target-path)
      (if dispatch-path-list
        (append! dispatch-path-list (list server-name))
        server-name))))



(provide "tcpcgi/dispatch")

