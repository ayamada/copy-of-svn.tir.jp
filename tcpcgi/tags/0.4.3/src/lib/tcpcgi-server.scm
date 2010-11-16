;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; note : tcp待ち受けの関係上、ほぼ常に一匹の子プロセスが
;;;        ゾンビ状態で残っているように見えますが、害はありません。
;;;        次にアクセスが来た時に回収されます。
;;;        （代わりに、新しいアクセスで生まれた子プロセスがゾンビ状態になる）

;;; ToDo : selectでSIGCHLDを監視する事で、コネクション待ち時に子プロセスが
;;;        死んだ事を確認したら、すぐに回収するようにする

;;; ToDo : シグナルを送って挙動を制御できるようにする事

;;; ToDo : reverse proxyとして動作する機能を追加する事（予定）
;;; ToDo : 上記と関連して、unix domain socketをlistenするモードも追加する事



(define-module tcpcgi-server
  (use gauche.net)
  (use srfi-1) ; delete
  (use srfi-2) ; and-let*
  (use util.list) ; assv-ref

  (use tcpcgi)
  (extend tcpcgi)
  (export
    <tcpcgi-server>
    tcpcgi-server-main
    ))
(select-module tcpcgi-server)




(define-class <tcpcgi-server> (<tcpcgi>)
  (
   (connection-log-port :accessor connection-log-port-of
                        :init-keyword :connection-log-port
                        :init-form #f)
   (server-addr :accessor server-addr-of
                :init-keyword :server-addr
                :init-value "0.0.0.0")
   (server-port :accessor server-port-of
                :init-keyword :server-port
                :init-value 8888)
   (server-name :accessor server-name-of
                :init-keyword :server-name
                :init-value #f)
   (max-clients :accessor max-clients-of
                :init-keyword :max-clients
                :init-value 32)
   (child-uid :accessor child-uid-of
              :init-keyword :child-uid
              :init-value #f)
   (child-gid :accessor child-gid-of
              :init-keyword :child-gid
              :init-value #f)
   ))


(define (logging self str . params)
  (and-let* ((log-port (connection-log-port-of self)))
    (display
      (string-append
        "tcpcgi-server: "
        (apply
          format
          str
          params)
        "\n")
      log-port)))


(define (spawn-tcpcgi self)
  (let1 tcpcgi (make <tcpcgi>)
    (for-each
      (lambda (slot-name)
        (set!
          (ref tcpcgi slot-name)
          (ref self slot-name)))
      (map slot-definition-name (class-slots <tcpcgi>)))
    tcpcgi))


(define (exec-tcpcgi-main&quit! self client-socket)
  (define (sockaddr->addr&port-string sockaddr)
    (let1 m (#/\:/ (sockaddr-name sockaddr))
      (cons (m 'before) (m 'after))))

  (define (setuidgid self)
    (let ((uid (child-uid-of self))
          (gid (child-gid-of self)))
      (when gid
        (sys-setgid (if (string? gid)
                      (sys-group-name->gid gid)
                      gid)))
      (when uid
        (sys-setuid (if (string? uid)
                      (sys-user-name->uid uid)
                      uid)))))

  (let* (
         (incoming (socket-input-port client-socket :buffering :full))
         (outgoing (socket-output-port client-socket :buffering :none))
         (server-sockaddr (socket-getsockname client-socket))
         (s-pair (sockaddr->addr&port-string server-sockaddr))
         (server-addr (car s-pair))
         (server-port (cdr s-pair))
         (remote-sockaddr (socket-address client-socket))
         (r-pair (sockaddr->addr&port-string remote-sockaddr))
         (remote-addr (car r-pair))
         (remote-port (cdr r-pair))
         (tcpcgi (spawn-tcpcgi self))
         (pid (sys-getpid))
         )
    (logging
      self
      "~a established ~a:~a ~a:~a"
      pid
      remote-addr
      remote-port
      server-addr
      server-port
      )
    (with-input-from-port
      incoming
      (lambda ()
        (with-output-to-port
          outgoing
            (lambda ()
              (setuidgid self)
              (tcpcgi-main
                tcpcgi
                server-addr ; SERVER_ADDR
                server-port ; SERVER_PORT
                (or         ; SERVER_NAME
                  (server-name-of self)
                  server-addr)
                remote-addr ; REMOTE_ADDR
                remote-port ; REMOTE_PORT
                #f          ; REMOTE_HOST
                #f ; HTTPS flag
                )))))
    (with-error-handler
      (lambda (e) #t)
      (lambda ()
        (flush outgoing)
        (socket-shutdown client-socket 2)))
    (socket-close client-socket)
    (logging
      self
      "~a shutdowned ~a:~a ~a:~a"
      pid
      remote-addr
      remote-port
      server-addr
      server-port
      )
    (sys-exit 0)))


(define (wait-connection self server-socket)
  (let1 max-clients (max-clients-of self)
    (define (collect-pid-from-pool pid-pool)
      (if (null? pid-pool)
        '()
        (let1 pid (sys-waitpid
                    -1
                    ;; ↓max-clientsに達している時は、減るまで待つ
                    :nohang (not
                              (and
                                max-clients
                                (<= max-clients (length pid-pool))
                                (begin ; ついでにロギングする
                                  (logging self "max-clients reached!")
                                  #t))))
          (if (= pid 0)
            pid-pool
            (begin
              (logging
                self
                "~d collected (~d/~d)"
                pid
                (- (length pid-pool) 1)
                max-clients)
              (socket-close (assv-ref pid-pool pid))
              (collect-pid-from-pool
                (alist-delete pid pid-pool)))))))

    (let loop ((pid-pool '()))
      (let* ((client-socket (socket-accept server-socket))
             (child-pid (sys-fork)))
        (when (= 0 child-pid)
          (exec-tcpcgi-main&quit! self client-socket))
        (logging
          self
          "~a raised (~d/~d)"
          child-pid
          (+ (length pid-pool) 1)
          max-clients)
        (loop
          (collect-pid-from-pool
            (acons
              child-pid
              client-socket
              pid-pool)))))))


(define (prepare-instance-slot! self)
  (update-vhost-dispatch-instance! self)
  (update-path-dispatch-instance! self)
  (update-none-dispatch-instance! self)
  #t)


(define *masked-signals*
  (let1 sigset (make <sys-sigset>)
    (sys-sigset-add!
      sigset
      SIGINT
      SIGTERM
      SIGHUP
      SIGPIPE)
    sigset))

(define-method tcpcgi-server-main ((self <tcpcgi-server>))
  (define (with-tcpcgi-server-signal-handler thunk)
    (with-signal-handlers
      ((*masked-signals* => (lambda (n)
                              (logging
                                self
                                "~d caught ~a"
                                (sys-getpid)
                                (sys-signal-name n))
                              (sys-exit 1))))
      thunk))

  (define (with-error-buffering thunk)
    (let* ((error-port (ref self 'cgi-error-port))
           (old-buffering-mode #f)
           )
      (dynamic-wind
        (lambda ()
          (set! old-buffering-mode (port-buffering error-port))
          (set! (port-buffering error-port) :line))
        thunk
        (lambda ()
          (set! (port-buffering error-port) old-buffering-mode)))))

  (prepare-instance-slot! self)

  (with-tcpcgi-server-signal-handler
    (lambda ()
      (with-error-buffering
        (lambda ()
          (and-let* ((server-socket
                       (with-error-handler
                         (lambda (e)
                           (logging
                             self
                             "cannot bind ~a:~a (~a)"
                             (server-addr-of self)
                             (server-port-of self)
                             (ref e 'message))
                           #f)
                         (lambda ()
                           (car
                             (make-server-sockets
                               (server-addr-of self)
                               (x->number (server-port-of self))
                               :reuse-addr? #t))))))
            (wait-connection self server-socket)))))))


(define-method tcpcgi-main ((self <tcpcgi-server>))
  (error "<tcpcgi-server> cannot execute tcpcgi-main (you can use tcpcgi-server-main)"))


(provide "tcpcgi-server")

