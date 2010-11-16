;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$


;;; ToDo : filer���μ���
;;; ToDo : filer���Τ��ٱ�¹Ԥ��뤬��������¾�ξ�����֤��٤�
;;;        ���󥿡��ե��������Ѱդ���

;;; ToDo : nph��Ʊ���褦���ٱ�¹Ԥ��뤬��
;;;        CGI/1.1�η�̤�HTTP/1.1���Ѵ�����execute-type���Ѱդ��롣���Ȥǡ�
;;;        ����ˤϡ�vport�λ��Ѥ�ɬ�ܤȤʤ�ʥ��饤�����ľ��˸���������١�
;;;        �ޤ������饤����Ȥؤϡ�chunked����������ɬ�פ����롣
;;;        �ʤ������ʤ���persistent connection��ݻ�����ʤ���
;;;        apache�ϴ��˥����������Ƥ���(pipe�Ǥ���)��

;;; note : filer�Τߡ��ǥ��쥯�ȥ�򰷤��롣
;;;        ����ʳ���asis-file���ϥǥ��쥯�ȥ�򰷤��ʤ�(ľ�ܥե��������Τ�)

;;; ToDo : ;͵�����褿�顢file, directory�Ϻ�ʬ�������롩

;;; ToDo : filer�ʳ��Υե�������Ϥ�404�б�
;;;        - filer���ٱ�¹ԤʤΤǡ��ɤλ�����404���֤�������̯


(define-module tcpcgi.execute
  (use gauche.parameter)
  (use gauche.regexp) ; regexp-quote
  (use srfi-1) ; filter-map
  (use srfi-2) ; and-let*
  (use file.util)
  (use rfc.uri)
  (use text.tree)
  (use util.list) ; alist->hash-table intersperse
  (use www.cgi)

  (use tcpcgi.execute.filer)
  (export
    *execute-type-canonical-table*

    <tcpcgi.execute>

    lazy-execute?
    always-connection-close?
    filer?
    execute ; (ɬ�פʤ顢�¹Ԥ���)�Ƽ���������
    lazy-execute ; (�ٱ�¹ԥ����פΤ�)�ٱ�¹Ԥ���
    ))
(select-module tcpcgi.execute)


(define *execute-type-canonical-table*
  (hash-table
    'eq?
    ;; type-name(alias ok) -> true-type-name(alias ng)
    '(script . script) ; �̾��cgi
    '(cgi . script) ; alias
    '(cgi-thunk . script) ; alias
    '(cgi-script . script) ; alias
    '(redirect . redirect) ; path-info�����Ѥ���302������쥯��
    '(path-redirect . redirect) ; alias
    '(location . location) ; path-info̵���302������쥯��
    '(filer . filer) ; �ǥ��쥯�ȥ�/�ե�����ɽ��
    '(file . filer) ; alias
    '(directory . filer) ; alias
    '(mount . filer) ; alias
    '(asis-file . asis-file) ; asis�ե�����ɽ��
    '(asis . asis-file) ; alias
    '(nph-script . nph-script) ; nphư���cgi
    '(nph . nph-script) ; alias
    '(nph-cgi-script . nph-script) ; alias
    '(pc-nph-script . pc-nph-script) ; persistent connection��ݻ�����nph
    '(pc-nph . pc-nph-script) ; alias
    ;; �ʲ��ϡ�path-dispatch��vhost-dispatch��ľ���ꤵ�줿����
    '(host-redirect . host-redirect) ; �ۥ���̾�Ǥ�302������쥯��
    '(vhost-redirect . host-redirect) ; alias
    ))


;; ToDo : execute-list����åȤ�������������execute-slot-update!��Ƥ�
(define-class <tcpcgi.execute> ()
  (
   ;; '(symbol target . keywords) ; �����Υꥹ�ȡ�
   ;; symbol��ư����סʤ��ξ���target�η���ư����ꤹ��ˡ���ά��ǽ��
   ;; target�ˤ��̾cgi-thunk�ޤ���pathʸ�������ꤹ�롣
   ;; keywords�ˤ�ǧ�ڥѥ�᡼�����������ǽ��̤�����ˡ�
   (execute-list :accessor execute-list-of
                 :init-keyword :execute-list
                 :init-value #f)
   ;; �ʲ��Υ���åȤϡ�execute-list����åȤ��鼫ư��������롣
   ;; ư����ס�symbol��
   (execute-type :accessor execute-type-of
                 :init-value #f)
   ;; ư������åȡ��ʲ��Τɤ줫��
   ;; proc, ʸ����, <tcpcgi.execute.filer>
   (execute-target :accessor execute-target-of
                   :init-value #f)
   ;; ���ꥭ����ɡ�execute-list���������
   (execute-keywords :accessor execute-keywords-of
                     :init-value '())
   ))


(define-method execute-slot-update! ((self <tcpcgi.execute>))
  (define (make-filer path . keywords)
    (apply
      make <tcpcgi.execute.filer>
      :root-path (sys-normalize-pathname
                   path
                   :absolute #t
                   :expand #t
                   :canonicalize #t)
      keywords))

  (let1 pre-execute-list (execute-list-of self)
    (when pre-execute-list ; #f�ξ��ϲ��⤷�ʤ�
      (let* (
             ;; execute-list�ϡ�list�ǤϤʤ�thunk��ʸ�����Ĥ����ξ�礬����
             (execute-list (if (list? pre-execute-list)
                             pre-execute-list
                             (list pre-execute-list)))
             (car-execute-list (car execute-list))
             (pre-execute-type (cond
                                 ((symbol? car-execute-list) car-execute-list)
                                 ((procedure? car-execute-list) 'script)
                                 ((string? car-execute-list) 'filer)
                                 (else (error "invalid execute-list"))))
             (execute-type (hash-table-get
                             *execute-type-canonical-table* pre-execute-type))
             (cdr-execute-list (if (symbol? car-execute-list)
                                 (cdr execute-list)
                                 execute-list))
             (car-cdr-execute-list (car cdr-execute-list))
             (execute-target (case execute-type
                               ('filer (apply make-filer cdr-execute-list))
                               (else car-cdr-execute-list)))
             (execute-keywords (cdr cdr-execute-list))
             )
        (set! (ref self 'execute-type) execute-type)
        (set! (ref self 'execute-target) execute-target)
        (set! (ref self 'execute-keywords) execute-keywords)
        ))))


(define-method initialize ((self <tcpcgi.execute>) initargs)
  (next-method)
  (execute-slot-update! self))






(define-method lazy-execute? ((self <tcpcgi.execute>))
  (memq
    (execute-type-of self)
    '(filer
      nph-script
      pc-nph-script
      )))
(define-method always-connection-close? ((self <tcpcgi.execute>))
  (memq
    (execute-type-of self)
    '(nph-script
      )))
(define-method filer? ((self <tcpcgi.execute>))
  (eq? 'filer (execute-type-of self)))







(define (execute-script self input-port)
  (with-output-to-string
    (lambda ()
      (with-input-from-port
        input-port
        (execute-target-of self))))) ; thunk���֤ꡢ���줬�¹Ԥ����
(define (execute-redirect self input-port)
  (list
    302
    :location (string-append
                (execute-target-of self)
                (or (cgi-get-metavariable "PATH_INFO") ""))))
(define (execute-location self input-port)
  (list
    302
    :location (execute-target-of self)))
(define (execute-host-redirect self input-port)
  (list
    302
    :location (let* ((scheme (if (cgi-get-metavariable "HTTPS")
                               "https"
                               "http"))
                     (host (execute-target-of self))
                     (default-port (if (string=? "http" scheme)
                                     80 443))
                     (port (and-let* ((server-port-string
                                        (cgi-get-metavariable "SERVER_PORT"))
                                      (server-port
                                        (x->number server-port-string))
                                      )
                             (and
                               (not (eqv? default-port server-port))
                               server-port)))
                     (path* (or (cgi-get-metavariable "REQUEST_URI") "/"))
                     )
                (uri-compose
                  :scheme scheme
                  :host host
                  :port port
                  :path* path*
                  ))))
(define (execute-asis-file self input-port)
  (let1 file (execute-target-of self)
    (cond
      ((not (file-exists? file)) (list 404))
      ((not (file-is-readable? file)) (list 401))
      ;; ToDo : request-method�Υ����å�����Ԥ�ɬ�פ�����
      (else (file->string file)))))

;; nph-script�ϡ��ٱ�¹Ԥ���롣ľ�˥��饤����Ȥ˥���ƥ�Ĥ��֤���
(define (execute-filer self input-port)
  ;; filer��method��ƤӽФ���Ŭ�ڤ�200, 404, 301�����֤��褦�ˤ���
  ;; ToDo : �ޤ�
  ;; ToDo : asis-file�ȶ��̲���ǽ����ʬ�϶��̲�����
  (let1 file (execute-target-of self)
    (cond
      ((not (file-exists? file)) (list 404))
      ((not (file-is-readable? file)) (list 401))
      ;; ToDo : request-method�Υ����å�����Ԥ�ɬ�פ�����
      (else (list 200)))))
(define (execute-nph-script self input-port)
  (list #f))





;; note : �ʲ��Υơ��֥�ϳ�procedure���������������ɬ�פ�����
(define *execute-table*
  (hash-table
    'eq?
    ;; type-name -> procedure
    `(script . ,execute-script)
    `(redirect . ,execute-redirect)
    `(location . ,execute-location)
    `(host-redirect . ,execute-host-redirect)
    `(asis-file . ,execute-asis-file)

    `(filer . ,execute-filer)
    `(nph-script . ,execute-nph-script)
    `(pc-nph-script . ,execute-nph-script)
    ))



;; ����method�����������ϡ����Τɤ��餫
;; - cgi/1.1-response (ʸ����)
;; - (cons response-code response-keywords)
;; -- response-keywords�ϡ���response-code�˸�ͭ��ɬ�׾���
;; cgi-thunk�¹Ի��˥��顼�㳰��ȯ��������硢���Τޤ��㳰���ꤲ����Τ�
;; Ŭ�ڤ˥���å��������ɬ�ס�
(define-method execute ((self <tcpcgi.execute>) . optional)
  (let* (
         (input-port (get-optional optional (current-input-port)))
         (execute-type (execute-type-of self))
         (proc (hash-table-get *execute-table* execute-type))
         )
    (proc self input-port)))







(define (lazy-execute-filer self)
  ;;;; ToDo : �ޤ�
  #f)
(define (lazy-execute-nph-script self)
  ;; thunk��¹Ԥ������
  ((execute-target-of self)))


;; note : �ʲ��Υơ��֥�ϳ�procedure���������������ɬ�פ�����
(define *lazy-execute-table*
  (hash-table
    'eq?
    ;; type-name -> procedure
    `(filer . ,lazy-execute-filer)
    `(nph-script . ,lazy-execute-nph-script)
    `(pc-nph-script . ,lazy-execute-nph-script) ; ��³�ݻ���̵ͭ���㤦����
    ))

(define-method lazy-execute ((self <tcpcgi.execute>) . optional)
  (and
    (lazy-execute? self)
    (let* (
           (input-port (get-optional optional (current-input-port)))
           (execute-type (execute-type-of self))
           (proc (hash-table-get *lazy-execute-table* execute-type))
           )
      (with-input-from-port
        input-port
        (lambda ()
          ;; �ٱ�¹Ի��ˤϡ�stdout��ľ�ܷ�̤��֤�
          (proc self)
          #t))))) ; �ٱ�¹Ի��ˤϡ��֤��ͤϰ�̣������ʤ��������





(provide "tcpcgi/execute")

