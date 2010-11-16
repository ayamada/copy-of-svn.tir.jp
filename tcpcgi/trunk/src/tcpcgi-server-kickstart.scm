#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; tcpcgi-server�μ¹ԥ���ץ롣

;;; usage :
;;; env - PATH="$PATH" ./tcpcgi-server-kickstart.scm

(add-load-path "lib")
(use tcpcgi-server)

(use gauche.process)
(use srfi-1)
(use text.tree)
(use text.html-lite)
(use www.cgi)
(use wiliki)


;;; ----------------------------------------------------------------
;;; �������鲼�ϡ�����ץ�cgi���

(define (debug-cgi)
  (cgi-main
    (lambda (params)
      (list
        (cgi-header :content-type "text/html")
        (html:html
          (html:body
            (html:h1 "cgi-metavariables")
            (html:dl
              (map (lambda (x)
                     (list
                       (html:dt (html-escape-string (car x)))
                       (html:dd (html-escape-string (cadr x)))))
                   (sort
                     (cgi-metavariables)
                     (lambda (x y)
                       (string<? (car x) (car y))))))
            (html:hr)
            (html:h1 "form-parameters")
            (html:dl
              (map (lambda (x)
                     (list
                       (html:dt (html-escape-string (car x)))
                       (map
                         (lambda (xx)
                           (html:dd (html-escape-string xx)))
                         (cdr x))))
                   params))
            (html:hr)
            (html:h1 "environment")
            (html:ul
              (map
                (lambda (x)
                  (html:li (html-escape-string x)))
                (process-output->string-list "/usr/bin/env")))
            ))))
    :on-error report-error
    ))


(define delayed-wiliki
  (delay (make <wiliki>
           :db-path "/tmp/wiliki-test"
           :log-file "wiliki-test.log"
           :top-page "wiliki-test"
           :title "wiliki-test"
           :language 'jp
           :charsets '((jp . euc-jp) (en . euc-jp))
           :debug-level 1
           :editable? #t
           )))

(define (wiliki-cgi)
  (wiliki-main (force delayed-wiliki)))


(define (menu-cgi)
  (write-tree
    (list
      (cgi-header)
      (html:html
        (html:body
          (html:h1 "tcpcgi-server sample")
          (html:ul
            (map
              (lambda (l)
                (html:li
                  (html:a
                    :href (car l)
                    (car l))))
              *path-dispatch*)))))))

(define (status-cgi)
  (write-tree
    (list
      (cgi-header
        :status "234 Hoge"
        )
      (html:html
        (html:body
          (html:p "Status: 234 Hoge"))))))

(define (simple-hello-cgi)
  (display "Content-Type: text/plain\n\n")
  (display "hello."))

(define (hello-cgi)
  (write-tree
    (list
      (cgi-header)
      (html:html
        (html:body
          (html:p "hello"))))))

(use file.util)
(define (touch-cgi)
  (touch-file "touched")
  (hello-cgi))

(define (sleep5-cgi)
  (for-each
    (lambda (x)
      (sys-sleep 1))
    (iota 5))
  (hello-cgi))

(define (sleep60-cgi)
  (for-each
    (lambda (x)
      (sys-sleep 1))
    (iota 60))
  (hello-cgi))

(define (script-error-cgi)
  (write-tree
    (list
      (cgi-header)
      (html:html
        (html:body
          (html:p "hoge-error start")
          (error "hoge-error-occured")
          (html:p "hoge-error end"))))))

(define (no-contents-cgi)
  #f)

(define (invalid-header-cgi)
  (print "hoge-invalid-string"))

(define (location1-cgi)
  (cgi-main
    (lambda (params)
      (list
        (cgi-header
          :location "http://google.com/"
          )))))
(define (location2-cgi)
  (cgi-main
    (lambda (params)
      (list
        (cgi-header
          :location "/env/aaa/bbb/ccc?ddd=eee&fff=ggg"
          )))))
(define (location3-cgi)
  (cgi-main
    (lambda (params)
      (list
        (cgi-header
          :location "hoge.cgi"
          )))))

(define (nph-cgi)
  (write-tree
    (list
      "HTTP/1.0 200 OK\r\n"
      "Content-Type: text/html\r\n"
      "Pragma: no-cache\r\n"
      "\r\n"
      (html:html
        (html:head
          (html:title "test of nph-script"))
        (html:body
          (html:h1 "this is test of nph-script.")
          (html:p "�����nph������ץȤ�ư��ƥ��ȤǤ���")
          (html:hr)
          (html:p
            (map
              (lambda (x)
                "�դ��դ�")
              (iota 32)))
          (html:hr)
          (html:p "������Ȥ��ޤ���")
          ))))
  (for-each
    (lambda (num)
      (flush)
      (sys-sleep 1)
      (display (x->string num))
      (print "<br />")
      (flush))
    (iota 4)))

(define (error404-cgi)
  (write-tree
    (list
      (cgi-header
        :status "404 Not Found"
        :pragma "no-cache"
        )
      (html:html
        (html:head
          (html:title "404 �Τä� �դ������")
          )
        (html:body
          (html:h1 "�Τä� �դ������")
          (html:p "�ߤĤ���ޤ���"))))))

(define (location-tcpcgi-cgi)
  (write-tree
    (cgi-header :location "http://d.tir.jp/pw?tcpcgi")))
(define (location-repos-cgi)
  (write-tree
    (cgi-header :location "http://svn.tir.jp/viewcvs.cgi/tcpcgi/")))

;;; ���������ϡ�����ץ�cgi���
;;; ----------------------------------------------------------------
;;; �������鲼�ϡ�tcpcgi������ʬ



;; alist�ǻ��ꤹ�롣
(define *path-dispatch*
  `(
    ;; �������
    ("/" ,menu-cgi)
    ("/tcpcgi" ,location-tcpcgi-cgi)
    ("/repos" ,location-repos-cgi)

    ;; cgiư���ǧ��
    ("/env" ,debug-cgi)
    ("/wiliki" ,wiliki-cgi)
    ("/simple-hello" ,simple-hello-cgi)
    ("/hello" ,hello-cgi)
    ("/touch" ,touch-cgi)
    ("/status" ,status-cgi)
    ("/nph-script" ,nph-cgi :nph #t) ; nphư��
    ("/location1" ,location1-cgi) ; full uri����
    ("/location2" ,location2-cgi) ; ����path����
    ("/location3" ,location3-cgi) ; ����path�����������
    ("/sleep5" ,sleep5-cgi)
    ("/sleep60" ,sleep60-cgi)

    ;; �ե�����ɽ��ư���ǧ��
    ("/cpuinfo" "/proc/cpuinfo") ; �ե�����ɽ��
    ("/qmail-doc" "/var/qmail/doc") ; �ǥ��쥯�ȥ�ɽ��
    ("/robots.txt" "/path/to/robots.txt") ; �ե�����ɽ��(404)
    ("/favicon.ico" #f) ; ����Ū��404���֤�
    ("/file/tcpcgi-kickstart.scm" "./tcpcgi-kickstart.scm") ; cwd��������л���
    ("/file/frame_black.css" "~nekoie/frame_black.css") ; mimeȽ��ƥ���

    ;; ���顼ư���ǧ��
    ("/no-contents" ,no-contents-cgi)
    ("/script-error" ,script-error-cgi)
    ("/invalid-header" ,invalid-header-cgi)
    ))


;; alist�ǻ��ꤹ�롣
(define *vhost-dispatch*
  `(
    ;; hoge.com
    ("hoge.com" ,*path-dispatch*)
    ;; *.hoge.com
    (".hoge.com"
     (("/" ,debug-cgi)
      ("/abc" "/proc/cpuinfo")))
    ;; mage.hoge.com (*.hoge.com����ͥ�褵���)
    ("mage.hoge.com" ,*path-dispatch*)
    ;; *.fuge.com (fuge.com�ϴޤޤ�ʤ�)
    (".fuge.com"
     (("/" ,debug-cgi)
      ("/def" "/proc/cpuinfo")))
    ))


(define *tcpcgi-server*
  (make <tcpcgi-server>
    :server-addr "0.0.0.0"
    :server-port 8888 ; 80����1024������port��Ȥ��ˤ�root���¤�ɬ�ס�
    ;:server-name "hoge.com"
    :max-clients 4
    ;:child-uid "nobody"
    ;:child-gid "nobody"
    ;; ��������Ĥ�root�Ǽ¹Ԥ��Ƥ�����Τ�ͭ����
    :connection-log-port (current-error-port)

    ;; �ʲ��Υѥ�᡼����<tcpcgi>��Ʊ��

    ;; �ʲ���:dispatch-*�ϡ����ν�˽�������롣
    :dispatch-vhost *vhost-dispatch* ; vhost��Ȥ�ʤ��ʤ���������
    ;; vhost�Τɤ�ˤ�ޥå����ʤ��ä����ϼ���dispatch-path���¹Ԥ���롣
    :dispatch-path *path-dispatch*
    ;; path�Τɤ�ˤ�ޥå����ʤ��ä����ϼ���dispatch-fallback���¹Ԥ���롣
    ;:dispatch-fallback debug-cgi
    ;:dispatch-fallback (list nph-cgi :nph #t) ; nph���ˤ��������Ϥ�������
    ;; dispatch-fallback��̵����ʤ顢404���֤���롣

    :errordoc-alist `((404 ,error404-cgi))

    ;; �����ॢ��������������
    :request-timeout 30 ; ���饤����Ȥ����HTTP�إå��ɤ߽Ф����Υ����ॢ����
    ;; ���ʹ֤������Ϥ򤹤�ʤ顢�礭���ͤˤ��롣�̾��5���餤��
    :response-timeout 60 ; cgi�¹Ի��Υ����ॢ����
    :keepalive-timeout 20 ; keep-alive�����ॢ����
    ;; ��reverse-proxy��Ȥ��ʤ顢�礭���ͤˤ��롣
    ;; ���ʹ֤������Ϥ򤹤�ʤ顢�礭���ͤˤ��롣�̾��5���餤��
    :use-server-header #t ; Server�إå������뤫�ݤ����ǥե���Ȥ�#f��
    ;:max-requests-per-child 100
    ))

;;; ----

(define (main args)
  (tcpcgi-server-main *tcpcgi-server*))



