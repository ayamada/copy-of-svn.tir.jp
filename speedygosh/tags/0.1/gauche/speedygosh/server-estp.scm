;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ToDo: gauche���֤���speedygosh�����Хץ��������椹����ʤȤ��ơ�
;;;       ɬ�פ��ѿ����Ѱդ��ơ�speedygosh.scm�Ȥ���ʬΥ�������

;;; ToDo: script-main�˸����������ϡ�close�к��Ȥ��ơ�vport��Ȥ�
;;;       (close���줽���ˤʤä��顢�ºݤˤ�flush����shutdown��������)
;;;       socket�ʤΤǡ�close����Ƥ��ޤ��ȡ����Ϥ������Ĥ����Ƥ��ޤ��١�
;;;       �����ơ�socket�δ������Ǥ��ץ���ư����̤괰λ�����Ȥ�������
;;;       ���������ܤ��ͤƤ���١�ư������Ϥ��ˤʤäƤ��ޤ���

;;; ToDo: standard-input-port���γ�Υ������ʤ顢exec����¦�ǹԤ��٤���������

;;; ToDo: isolate-stdio-port�ϡ�/dev/null��Ȥ��ΤǤϤʤ���
;;;       vport��ȤäƼ�������

;;; note: ������˥��顼�����ä����ϡ�SIGCHLD�ˤ�äƿƤ˥��顼�����Τ��롣
;;; note: �����socket�����ޤǴ�λ�������򼨤��ˤϡ�
;;;       stdout�˲��Ԥ�ή�����Ǥ����Ƥ����Τ��롣
;;;       (�Ƥϥѥ��׵�ư���Ƥ��롢�Ȥ�������)
;;;       ���顼��λ���ˤϲ��Ԥ�����ʤ�����

;;; note: .stderr��.exit�ե�����κ���ϥ��饤�����¦��Ǥ����
;;;       (���饤����Ȥ����Ȥ��륿���ߥ󥰤������ʰ�)

;;; TODO: sys-putenv�ǴĶ��ѿ������ꤹ��褦��ľ����

(define-module speedygosh.server-estp
  (use gauche.interactive)
  (use gauche.net)
  (use gauche.selector)
  (use gauche.fcntl)
  (use gauche.vport)
  (use gauche.uvector)
  (use gauche.parameter)
  ;(use www.cgi)
  (use srfi-1)
  (use speedygosh)

  (export
    speedygosh-server-boot
    ))
(select-module speedygosh.server-estp)

;; import from speedygosh
(define powered-by-speedygosh
  (with-module speedygosh powered-by-speedygosh))
(define terminate-request
  (with-module speedygosh terminate-request))
(define speedygosh-termination-handlers
  (with-module speedygosh speedygosh-termination-handlers))


;(define *debug-log* "/tmp/sglog")
(define *debug-log* #f)
(define-macro (debugf formatter . args)
  (if *debug-log*
    `(with-output-to-file
       *debug-log*
       (lambda ()
         (format #t ,formatter ,@args))
       :if-exists :append)
    #t))

;; note: */dev/null*��Ŭ���ʥե�������ѹ�������ǡ��ǥХå����䤹���ʤ�
;;       (stdout��stderr��ή��Ƥ��ޤä���Τ��ɤ��Τǡ�
;;        ���Τޤޤ���/dev/null�˾ä��Ƥ��ޤ��Τ�print�ǥХå�������ˤ���)
(define */dev/null* "/dev/null")
(define (isolate-stdio-port thunk)
  (let ((in (open-input-string ""))
        (out (open-output-file */dev/null*)))
    (with-input-from-port
      in
      (lambda ()
        (with-output-to-port
          out
          thunk)))))


(define (load-script-to-module script-file)
  ;; ������script-file��ɾ������ݤˡ�stdin/stdout��;�פʥǡ�����ή����ޤʤ�
  ;; �褦�ˤ���ɬ�פ����뤬�����顼���ˤ����stderr�ˤ�ή�������Τǡ�
  ;; stdin/stdout�Τߥե��륿��󥰤���ɾ�����롣
  ;; �ġĤȻפä�����load-script-to-module��ƤӽФ���¦�Ǵ���
  ;; isolate-stdio-port��Ƥ�Ǥ��ä��Τǡ������ǤϾ�ά���롣
  (let1 script-module (make-module #f)
    ;; ͽ�ᡢ*program-name* �� *argv* ��user����import���Ƥ���ɬ�פ�����
    (with-module user
      (export *program-name* *argv*))
    (eval '(import user) script-module)
    ;; ������ץȤ��ɤ߹��ࡣ���顼�㳰�ˤʤä��顢���Τޤޥ��顼���ꤲ�롣
    ;; ���顼���Ƥ�stderr��ή����
    (guard (e (else
                ;; �ʤ󤫾�꤯�����å��ȥ졼����ɽ�����Ƥ���ʤ��Τǡ�
                ;; ������ɽ��������
                (report-error e)
                (exit)))
      (load script-file :paths '(".") :environment script-module))
    script-module))


;; note: socket�������Ǥ��ʤ��ä��饨�顼�㳰���ꤲ���
(define (make-session-socket-or-error session-path-prefix)
  (let1 socket-path (string-append session-path-prefix ".sock")
    ;; �ޤ��������ˡ�����socket��̵�������ǧ�����̣��ޤ��unlink���Ƥ���
    (sys-unlink socket-path)
    (make-server-socket 'unix socket-path)))


(define (lock-session! session-path-prefix)
  (let* ((lock-file (string-append session-path-prefix ".lock"))
         (lock-port (open-output-file lock-file))
         (flock (make <sys-flock>)))
    (set! (ref flock 'type) F_WRLCK)
    (set! (ref flock 'whence) 0) ; SEEK_SET = 0
    (set! (ref flock 'start) 0)
    (set! (ref flock 'len) 0)
    (if (sys-fcntl lock-port F_SETLK flock)
      lock-port
      #f)))

(define (make-error-vport path)
  ;; ���ƽ񤭹��ޤ줿�ʳ��ǥե������open����vport���֤�
  ;; (�פ���ˡ������Ȥ��ʤ��ä���file����ʤ�vport����)
  (let* ((output-port-true #f)
         (vport (make <buffered-output-port>)))
    (set! (ref vport 'flush)
      (lambda (buf flag)
        (let1 len (u8vector-length buf)
          (if (zero? len)
            len
            (begin
              (unless output-port-true
                (set! output-port-true
                  (open-output-file path
                                    :buffering :none)))
              (write-block buf output-port-true)
              len)))))
    (set! (ref vport 'close)
      (lambda ()
        (when output-port-true
          (close-output-port output-port-true))))
    (set! (port-buffering vport) :none)
    vport))

(define (incomplete-filter string-list)
  (map
    (cut string-incomplete->complete <> :omit)
    string-list))

(define (estp-receive socket-in)
  ;; note: ���λ����ǡ��Դ���ʸ����Υե��륿��󥰤�Ԥ���
  ;; ���Ϥ����꤬���ä�����(values #f #f)���֤�
  ;; ToDo: ESTP/0.1, ESTP/0.3�ˤ��б��������
  (guard (e (else
              (debugf "estp error: ~s\n" e)
              (values #f #f)))
    (let1 protocol-symbol (read socket-in)
      (debugf "dump protocol: ~s\n" protocol-symbol)
      (unless (eq? protocol-symbol 'ESTP/0.2)
        (error "invalid protocol"))
      (let* ((env (read socket-in))
             (argv (read socket-in)))
        (debugf "dump env: ~s\n" env)
        (debugf "dump argv: ~s\n" argv)
        (values (map incomplete-filter env) (incomplete-filter argv))))))

(define (with-env env argv thunk)
  ;; TODO: sys-environ�ѥå���񤤤ơ����Ѥ��Ƥ��ä��顢
  ;;       sys-environ��ȤäƴĶ��ѿ���ʤ��褦��ľ����
  ;;       (�����Ǥϡ��Ķ��ѿ���merge���֤ˤʤäƤ���Τǡ�
  ;;        estp�������ʤ��ä��Ķ��ѿ��ϡ�
  ;;        ��ư���δĶ��ѿ���ϳ��Ƥ��ޤäƤ���)
  (let1 old-env '()
    (dynamic-wind
      (lambda ()
        ;; �Ť��Ķ��ѿ��򵭲�����
        (set! old-env
          (fold
            (lambda (name+val prev)
              (cons
                (list (car name+val)
                      ;; �Ķ��ѿ���¸�ߤ��ʤ�����#f�Ȥ��Ƶ�������
                      (sys-getenv (car name+val)))
                prev))
            '()
            env))
        ;; �������Ķ��ѿ��򥻥åȤ���
        (for-each
          (lambda (name+val)
            (sys-setenv (car name+val) (cadr name+val) #t))
          env))
      (lambda ()
        ;; note: *program-name* �� *argv* ��fluid-let ���ʤ��ƤϤʤ�ʤ�
        ;; ��������������Ĥ�user�⥸�塼������Τ����ꡢ
        ;; ���˥�����ץȥ⥸�塼���export����Ƥ���Τǡ�
        ;; user�⥸�塼�������fluid-let�����ok��
        (with-module user
          (fluid-let ((*program-name* (guard (e (else *program-name*))
                                        (car argv)))
                      (*argv* (guard (e (else *argv*))
                                (cdr argv))))
            (thunk))))
      (lambda ()
        ;; �Ť��Ķ��ѿ�����᤹
        (for-each
          (lambda (name+val)
            (if (cadr name+val)
              (sys-setenv (car name+val) (cadr name+val) #t)
              (sys-unsetenv (car name+val))))
          old-env)
        ;; ���ǰ�ΰ١����ꥢ���Ƥ���
        (set! old-env '())))))

(define (server-start
          socket script-main session-path-prefix timeout maxruns errorlog)
  (define (return-code-set! return-code)
    (unless (zero? return-code)
      (with-output-to-file
        (string-append
          session-path-prefix ".exit")
        (cut write return-code))))

  (let ((selector (make <selector>))
        (error-flag #f) ; script-main�ǥ��顼����������#t�ˤ���
        (lock-port #f) ; ñ�ˤ�����gc�����ʤ��٤������Ѱ�
        (runs 0) ; �¹Բ��
        )
    (selector-add!
      selector
      (socket-fd socket)
      (lambda (fd flag)
        (debugf "client connected\n")
        (let1 client-socket (socket-accept socket)
          (let ((in  (socket-input-port client-socket))
                (out (socket-output-port client-socket))
                (err (or
                       (and
                         errorlog
                         (not (string=? errorlog ""))
                         (guard (e (else #f))
                           (open-output-file errorlog
                                             :if-exists :append
                                             :buffering :none)))
                       (make-error-vport (string-append
                                           session-path-prefix ".stderr"))))
                )
            ;; �ޤ���ESTP����env��argv��������롣
            (receive (env argv) (estp-receive in)
              (if (not env)
                (socket-disconnect/ignore-error client-socket)
                (with-error-handler
                  (lambda (e)
                    (debugf "error occured: ~a\n" (ref e 'message))
                    (set! error-flag #t)
                    (report-error e)
                    ;; ���顼���������餽�Τޤޥץ�����λ����Τǡ�
                    ;; ���ν�����Ԥ���
                    ;; â�������饤����ȤȤ��̿������Ǥ�
                    ;; ���ֺǸ�ˤ��ʤ��ƤϤʤ�ʤ���
                    ;; (��å���Ϣ��race condition���򤱤��)
                    ;; �����Ǥϡ�unix domain socket��ͳ�Ȥ�������ǡ�
                    ;; client-socket�ϺǸ�ޤ�����Ū�����Ǥ����ˡ�
                    ;; ���Τޤޥץ�����λ�������
                    ;; �̿������Ǥ�����Ȥ��롣
                    ;; (tcp�Ȥ�����¿ʬ����Ϥޤ���)
                    (return-code-set! 70))
                  (lambda ()
                    (debugf "communicate start\n")
                    (with-input-from-port
                      in
                      (lambda ()
                        (with-output-to-port
                          out
                          (lambda ()
                            (with-error-to-port
                              err
                              (lambda ()
                                (with-env
                                  env argv
                                  (lambda ()
                                    (let1 return-code (x->integer
                                                        (script-main argv))
                                      (debugf "execute done\n")
                                      (inc! runs)
                                      (return-code-set! return-code))))))))))
                    ;; ��λ������Ԥ�
                    ;; ToDo: �������Ĥ��Ƥ����˼�����³���褿��硢
                    ;;       ���ǽ�λ����ͽ����ä�����race condition��
                    ;;       ȯ���������ʵ�������Τǡ����Ȥǥ����å�����ľ����
                    (close-output-port err)
                    (close-output-port out)
                    (close-input-port in)
                    (socket-disconnect/ignore-error client-socket))))))))
      '(r))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (let loop ()
          (debugf "selecting\n")
          (let1 counter (selector-select selector `(,timeout 0))
            ;(debugf "runs=~d maxruns=~d cond=~s\n" runs maxruns (< runs maxruns))
            (cond
              (error-flag #f) ; ���顼���������ʤ餹����λ
              ((and
                 (not (zero? counter)) ; ������Ԥä���
                 (< runs maxruns) ; �ޤ�������������ã���Ƥʤ���
                 (not (terminate-request)) ; �����н�λ�ե饰��Ω�äƤ��ʤ���
                 )
               (loop)) ; ���̤˽�����������³���Ԥ�
              (else
                ;; �����ॢ���Ȥ����Τǡ������н�λ������Ԥ�
                ;; �桼�������ꤷ����λ�ϥ�ɥ��¹Ԥ���
                (for-each
                  (lambda (x)
                    (x))
                  (speedygosh-termination-handlers))
                ;; �⤦���饤����Ȥ��Ĥʤ��Ǥ��ʤ��褦�ˡ���å�����
                ;; (race condition�ɻߤΰ�̣�⤢��Τ�ɬ��)
                (set! lock-port (lock-session! session-path-prefix))
                ;; ��å������������餽�Τޤ�dynamic-wind�ν�λ�����ء�
                ;; �����Ǥʤ���С�
                (unless lock-port
                  ;; ��å����ԡ����饤����Ȥ���³�����ΤǺƼ¹�
                  (set! timeout 0) ; â�����⤦�Ԥ��ʤ����ɤ�
                  (loop)))))))
      (lambda ()
        ;; ����������ʥ뤬ή�줿���⡢������ɬ��cleanup�������Ԥ��롣
        ;; ToDo: �������������ʥ뤬ή�줿����error-flag��#t�ˤ�������
        (selector-delete! selector #f #f #f) ; selector�򥯥ꥢ����
        (server-shutdown socket session-path-prefix lock-port error-flag)))))

(define-syntax ignore-error
  (syntax-rules ()
    ((_ . bodies)
     (guard (e (else e)) . bodies))))

(define (socket-disconnect/ignore-error socket)
  (ignore-error
    (socket-shutdown socket 2))
  (ignore-error
    (socket-close socket)))

;; note:
;; - error-flag��#f�λ��ϡ��ʲ���ư���Ԥ�
;; -- selector�Υ��ꥢ
;; -- socket���Ĥ���unlink����
;; -- ��å��ե�����κ��(��å����Ƥ�������ΰ�)
;; - error-flag��#t�λ��ϡ��ʲ���ư���Ԥ�
;; -- selector�Υ��ꥢ
;; -- socket���Ĥ���unlink����
;; -- ��å��ϥ��饤�����¦����Ԥ��Ƥ���Ȧ�ʤΤǡ�������ʤ���
(define (server-shutdown socket session-path-prefix lock-port error-flag)
  (debugf "server shutdown\n")
  ;; selector�򥯥ꥢ����
  ;; socket���Ĥ���unlink����
  (socket-disconnect/ignore-error socket)
  (sys-unlink (string-append session-path-prefix ".sock"))
  ;; �󥨥顼��λ(=�����ॢ����)�ʤ顢����ե�����κ����ǰ�ΰٹԤ�
  (unless error-flag
    (sys-unlink (string-append session-path-prefix ".exit"))
    (sys-unlink (string-append session-path-prefix ".stderr")))
  ;; �Ǹ�ˡ��󥨥顼��λ(=�����ॢ����)�ʤ顢��ʬ�ǥ�å��ե������������
  (unless error-flag
    (sys-unlink (string-append session-path-prefix ".lock"))
    ;; �������å��ե������close���Ƥ���
    (when lock-port
      (ignore-error
        (close-output-port lock-port)))))

;;; ư���:
;;; - speedygosh���ޥ�ɤ��顢�ʲ��Τ褦�ʰ����ǵ�ư���Ԥ��롣
;;;   gosh -b -uspeedygosh.server-estp -Espeedygosh-main -Eexit -- \
;;;   script-file session-path-prefix timeout maxruns errorlog |
;;;   �����������ͤ��Դ���ʸ����Ǥ����ǽ�������롣
;;;   script-name�ϡ�*program-name*�Ǽ�����ǽ��
;;;   �ޤ���*argv*�ˤϡ�(session-path-prefix timeout)�����롣
;;;   ���Υץ�����stdout�Ͽƥץ����˥ѥ�����³����Ƥ��ꡢ
;;;   ���줬���Ǥ���������������ư��ν�����λ�ι�ޤȤʤ롣
(define (speedygosh-server-boot)
  (set-signal-handler! SIGPIPE #f) ; SIGPIPE�Ϥ�������̤�̵�뤹��
  (isolate-stdio-port ; stdio�ϼΤƤ�(�������ޤ����ǤϤ��ʤ�)
    (lambda ()
      (let ((script-file (with-module user *program-name*))
            (session-path-prefix (car (with-module user *argv*)))
            (timeout (x->number (list-ref (with-module user *argv*) 1 90)))
            (maxruns (x->number (list-ref (with-module user *argv*) 2 1024)))
            (errorlog (list-ref (with-module user *argv*) 3 #f))
            )
        ;; ToDo: �����Υ����å�
        (parameterize ((powered-by-speedygosh #t)
                       (terminate-request #f)
                       (speedygosh-termination-handlers '())
                       )
          ;; �ޤ�������ץȤ��ɤ߹��ߡ�����main�ؿ����������
          ;; �������κݤ�stderr�˥��顼���Ƥ�ή����뤫���Τ�ʤ��١�
          ;; �ޤ������ʳ��Ǥ�stdin/stdout/stderr���ڤ�Υ���ϹԤ�ʤ���
          (let* ((script-module (load-script-to-module script-file))
                 (script-main (global-variable-ref script-module 'main #f)))
            (unless script-main
              (error "cannot found proc of main" script-file))
            ;; socket����������
            (let1 socket (make-session-socket-or-error session-path-prefix)
              ;; socket�����������������Τǡ������ʳ��Ȥ��ƤϤ⤦
              ;; ���顼�ˤʤ����̵��Ȧ�ʤΤǡ�stderr��ΤƤ�
              (with-error-to-port
                (current-output-port) ; stdout��ή�Ѥ���
                (lambda ()
                  ;; socket����������socket����������˴�λ�������򼨤��٤ˡ�
                  ;; ���Ԥ����äƤ��顢�ƤȤΥѥ��פ����Ǥ������Τ��롣
                  ;; �Ĥ��Ǥ�stdin��stderr���Ĥ��Ƥ���
                  ;; (�������ޤޤ��ȿƥץ����ν�λ�˻پ㤬����)
                  (close-input-port (standard-input-port))
                  (newline (standard-output-port)) ; ��ޤȤ���ɬ��
                  (close-output-port (standard-output-port))
                  (close-output-port (standard-error-port))
                  ;; ToDo: standard-input-port�������Τޤޤ��ȡ�
                  ;;       sys-system�¹Ի����Ǥ��������褬����ˤʤ�١�
                  ;;       ���餫�μ��ʤ��Ѱդ������������⤽���ǽ�ʤΤ���
                  ;; �������Ԥ������򳫻Ϥ���
                  (debugf "server start\n")
                  (server-start socket
                                script-main
                                session-path-prefix
                                timeout
                                maxruns
                                errorlog
                                ))))))))))


(provide "speedygosh/server-estp")

