#!/usr/local/gauche/bin/speedygosh
;#!/usr/local/gauche/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; S���Хȥ顼

;;; gauche.gong�˸����ơ��Ȥꤢ����ư���ޤǺ���

;;; TODO: ������(print ...)����¹Ԥ���ȡ����˻Ĥ�feature�����äƤ�褤
;;;       (say���ѻߤ��롩)

;;; TODO: ����ǥ��󥰤ϺǸ�����̥�å��������Ѱդ���
;;;       (���������Ƥ�ɽ������ʤ��褦�ˡ��̥ե�������ɤ߹���)


(use text.tree)
(use text.html-lite)
(use srfi-1)
(use util.list)
(use www.cgi)
(use rfc.822)
(use rfc.uri)
(use rfc.http)
(use util.match)
(use gauche.parameter)
(use gauche.charconv)
(use rfc.sha1)
(use util.digest)

(use gauche.process)

(add-load-path "lib")
(use sb-text)

;;; ----
;;; �����Х��ͤ����
(define-constant *gosh-path* "/usr/local/gauche/bin/gosh")
(define-constant *sbattle-path* "./sbattle.scm")
(define-constant *errorlog-path* "/home/nekoie/tmp/sb-errorlog.txt")

;;; ----

;; ����ǥ��������������
(define-condition-type <sbattle-timeout> <error>
  sbattle-timeout?
  )
(define-condition-type <sbattle-abnormal-exit> <error>
  sbattle-abnormal-exit?
  (status sbattle-abnormal-exit->status)
  )


;;; ----
;;; ���Ѽ�³����

(define (completion-uri uri server-name server-port https)
  (receive (uri-scheme
            uri-userinfo
            uri-hostname
            uri-port
            uri-path
            uri-query
            uri-fragment)
    (uri-parse uri)
    ;; uri-scheme��̵�����ˤ����䴰����
    ;; â����server-name��Ϳ�����Ƥ��ʤ������䴰�Ǥ��ʤ��Τǡ����⤷�ʤ�
    (if (or uri-scheme (not server-name))
      uri
      (let* ((scheme (if https "https" "http"))
             (default-port (if https 443 80))
             )
        (uri-compose
          :scheme scheme
          :userinfo uri-userinfo
          :host server-name
          :port (and
                  server-port
                  (not (eqv? default-port (x->number server-port)))
                  server-port)
          :path uri-path
          :query uri-query
          :fragment uri-fragment)))))

(define (path->url path)
  (completion-uri path
                  (cgi-get-metavariable "SERVER_NAME")
                  (cgi-get-metavariable "SERVER_PORT")
                  (cgi-get-metavariable "HTTPS")))

(define (symbol->path symbol . opt-params)
  ;; TODO: ���ݤʤΤǼ�³��Ū�˽񤤤Ƥ��ޤä������Ȥ�ľ����
  (let* ((params (get-optional opt-params #f))
         (str1 (symbol->string symbol)) ; ʸ����
         (str2 (regexp-replace #/^cgi\:/ str1 "")) ; ��Ƭ��cgi:�ʤ���
         (path1 (or (cgi-get-metavariable "SCRIPT_NAME") "/"))
         (path2 (string-append path1 "?c=" str2))
         (path3 (if (not params)
                  path2
                  (tree->string
                    (list path2
                          "&"
                          (intersperse
                            "&"
                            (apply
                              append
                              (map
                                (lambda (key+vals)
                                  (let1 k (delay (uri-encode-string
                                                   (car key+val)))
                                    (map
                                      (lambda (val)
                                        (list
                                          (force k)
                                          "="
                                          (uri-encode-string val)))
                                      (cdr key+vals))))
                                params)))))))
         )
    path3))



(define (location url)
  (cgi-header
    :location url
    :pragma "no-cache"))

(define-syntax when/null
  (syntax-rules ()
    ((_ pred body ...) (if pred
                         (begin body ...)
                         '()))))

(define (make-cgi-response . keywords)
  (let-keywords keywords ((encoding #f) ; TODO: ���Ȥǹͤ���
                          (extra-http-headers '())
                          ;; TODO: ɬ�פ˱����ơ����Ȥ��ɲä���
                          . #t)
    (list
      (apply
        cgi-header
        (list*
          :pragma "no-cache"
          extra-http-headers))
      (apply make-html-response keywords))))

(define (make-html-response . keywords)
  (let-keywords keywords ((encoding (gauche-character-encoding))
                          (base-url #f)
                          (css-url #f)
                          (css-body #f)
                          (js-url #f)
                          (js-body #f)
                          (js #f)
                          (robots #f)
                          (title #f)
                          (title-format #f)
                          (title-format-args '())
                          (extra-html-headers '())
                          (html-body #f)
                          (html-body-attrs '())
                          (html-body-header #f)
                          (html-body-footer #f)
                          (html-frame-body #f)
                          . #t)
    (let ()
      (list
        ;; xml header
        (if encoding
          #`"<?xml version=\"1.0\" encoding=\",|encoding|\"?>\n"
          "<?xml version=\"1.0\"?>\n")
        ;; html doctype
        (html-doctype
          :type (if html-frame-body
                  :xhtml-1.0-frameset
                  :xhtml-1.0-transitional))
        ;; html
        (html:html
          :lang "ja-JP"
          :xml:lang "ja-JP"
          :xmlns "http://www.w3.org/1999/xhtml"
          (html:head
            (when/null encoding
              (html:meta :http-equiv "Content-Type"
                         :content #`"text/html; charset=,|encoding|"))
            (when/null base-url
              (html:base :href base-url))
            (when/null (or css-url css-body)
              (html:meta :http-equiv "Content-Style-Type"
                         :content "text/css"))
            (when/null (or js-url js-body js)
              (html:meta :http-equiv "Content-Script-Type"
                         :content "text/javascript"))
            (when/null robots
              (html:meta :name "ROBOTS"
                         :content robots))
            ;; title��ͥ���̤ϡ�title����title-format������ͥ�褹��
            (or
              (and
                title-format
                (guard (e (else #f))
                  (html:title
                    (html-escape-string
                      (apply format #f title-format title-format-args)))))
              (when/null title
                (html:title (html-escape-string title))))
            (when/null css-url
              (html:link :rel "Stylesheet"
                         :type "text/css"
                         :href css-url))
            (when/null css-body
              (html:style :type "text/css"
                          "<!--\n"
                          css-body
                          "\n-->"
                          ))
            (when/null js-url
              (html:script :type "text/javascript"
                           :src js-url))
            (when/null js-body
              (html:script :type "text/javascript"
                           "<!--\n"
                           js-body
                           "\n-->"
                           ))
            (when/null extra-html-headers extra-html-headers)
            )
          (when/null html-body
            (apply html:body
                   (append
                     html-body-attrs
                     (when/null html-body-header html-body-header)
                     (list html-body)
                     (when/null html-body-footer html-body-footer)
                     )))
          (when/null html-frame-body html-frame-body))))))

(define (response-frame keywords)
  (error "not yet") ; TODO: ���Ȥ�
  )

(define (response-html html-body)
  (make-cgi-response
    ;; TODO: title��css����ꤹ��
    ;; TODO: ���줬�Ȥ��褦�ˡ�¾�Υ�����ɤ�­����(���Ȥ�)
    :title "S���Хȥ顼"
    :css-url "http://css.tir.jp/tir.css"
    :robots "NOINDEX,NOFOLLOW"
    :html-body html-body))

(define (params->path params)
  (tree->string
    (list
      (cgi-get-metavariable "SCRIPT_NAME")
      "?"
      (intersperse
        "&"
        (map
          (lambda (x)
            (list
              (uri-encode-string (x->string (car x)))
              "="
              (uri-encode-string (x->string (cadr x)))))
          params)))))


(define (get-name)
  (receive (status headers body) (http-get "d.tir.jp" "/namegen.cgi")
    (if (and
          (equal? status "200")
          (< (string-size body) 64))
      (car (string-split (ces-convert body "EUC-JP") "\n"))
      "������")))


;;; ----
;;; play����ȥ�Υ롼��:
;;; - ��play:�פȤ���prefix�ǻϤޤ륷��ܥ��«������롣
;;; - cgi:play-submit����ʲ��ΰ����ǸƤФ�롣
;;;   (play:hoge params name sexpr-src sexpr)
;;; - cgi-main���������������֤��ͤ��֤���

;(define (play:check params name sexpr)
;  ;; ���λ����ǡ������å����̤äƤ���Ȧ�ʤΤǡ����Τޤ��֤�����
;  (cgi:play params '() #t))



;; TODO: �޿;���ȴ���б��ˤ���ɬ�פ�����
(define (play:output-result textonly? won? logs)
  (if textonly?
    (list
      (cgi-header :content-type "text/plain")
      (intersperse
        "\n"
        (map
          (lambda (x)
            (write-to-string x))
          logs)))
    (response-html
      (list
        (html:pre
          (intersperse
            "\n"
            (map
              (lambda (x)
                ;; �ʹ֤��ɤߤ䤹���������Ѵ�����
                (let* ((cmd (cadr x))
                       (proc (hash-table-get *table:cmd->display-proc*
                                             cmd
                                             #f))
                       )
                  (if (not proc)
                    (html-escape-string
                      (write-to-string x))
                    (apply proc (car x) (cddr x)))))
              logs)))))))
(define *table:cmd->display-proc*
  (hash-table
    'eq?
    `(say    . ,(lambda (who message prior)
                  ;; TODO: prior�˱������Ѳ��������
                  (html-escape-string
                    (format
                      "��~a�٤϶��������~a��" who message))))
    `(ready  . ,(lambda (who action)
                  (let1 weapon (cond
                                 ((eq? action 'g) "G���å���")
                                 ((eq? action 'c) "C�֥졼��")
                                 ((eq? action 'p) "P�������")
                                 (else ; #f
                                   #f))
                    (if weapon
                      (html-escape-string
                        (format
                          "��~a�٤�~a�򹽤�����" who weapon))
                      (html-escape-string
                        (format
                          "��~a�٤Ϲ�����򤤤���" who))))))
    `(attack . ,(lambda (who action)
                  (let1 weapon (cond
                                 ((eq? action 'g) "G���å���")
                                 ((eq? action 'c) "C�֥졼��")
                                 ((eq? action 'p) "P�������")
                                 (else ; #f
                                   #f))
                    (if weapon
                      (html-escape-string
                        (format
                          "��~a�٤�~a�ǹ��⤷����" who weapon))
                      (html-escape-string
                        (format
                          "��~a�٤Ϸ��ǲ��꤫���ä���" who))))))
    `(parry  . ,(lambda (who)
                  (html-escape-string
                    (format
                      "��������~a�٤Ϲ�������ή������" who))))
    `(stun   . ,(lambda (who)
                  (html-escape-string
                    (format
                      "��~a�٤�����;�ä�ž��Ǥ��ޤä���" who))))
    `(damage . ,(lambda (who num)
                  (html-escape-string
                    (format
                      "��~a�٤�~d�Υ��᡼�����������" who num))))
    `(hp     . ,(lambda (who num)
                  (html-escape-string
                    (format
                      "��~a�٤�HP��~d�ˤʤä���" who num))))
    `(guard  . ,(lambda (who)
                  (html-escape-string
                    (format
                      "��~a�٤����ι�����ɤ�����" who))))
    `(won    . ,(lambda (who)
                (html-escape-string
                  (format
                    "��~a�٤����ä���" who))))
    `(fumble . ,(lambda (who)
                   (html-escape-string
                     (format
                       "��~a�٤ϥܡ��äȤ��Ƥ��롣" who))))
    `(elapse . ,(lambda (who total-steps)
                   (html-escape-string
                     (format
                       "��Ʈ���Ϥ��顢~d���ƥåפ��вᤷ����" total-steps))))
    `(draw   . ,(lambda (who)
                   (html-escape-string
                     (format
                       "���餬�Ĥ��ʤ��Τ�Ƚ�꤬�Ԥ�줿��"))))
    ))


;;; ----
;;; cgi����ȥ�Υ롼��:
;;; - ��cgi:�פȤ���prefix�ǻϤޤ륷��ܥ��«������롣
;;; - �������ļ��(cgi-params)��
;;; - cgi-main���������������֤��ͤ��֤���


(define (cgi:explain params)
  (let1 html-back (html:ul
                    (html:li
                      (html:a :href (symbol->path 'menu) "���")))
    (response-html
      (list
        html-back
        (html:hr)
        (html:h1 "���ȡ��꡼(��)")
        (html:pre
          (html-escape-string *text:story*))
        (html:h1 "����")
        (html:pre
          (html-escape-string *text:brief*))
        (html:hr)
        html-back))))





;; ���顼������ʤ餽��list�򡢤����Ǥʤ����#f���֤�
;; ���������Ǥϴ�ñ�ʥ����å������Ԥ��ʤ���
;; S���������Τʸ��ڤ�sbattle.scm�ǹԤ��������դ������
(define (check-freeplay-parameter params)
  (let/cc return
    (define (goto-error . msgs)
      (return msgs))

    (let (
          (p1name (or (cgi-get-parameter "p1name" params) ""))
          (p2name (or (cgi-get-parameter "p2name" params) ""))
          (p1sexpr (or (cgi-get-parameter "p1sexpr" params) ""))
          (p2sexpr (or (cgi-get-parameter "p2sexpr" params) ""))
          (confirm (cgi-get-parameter "confirm" params))
          )
      (when (equal? "" p1name)
        (goto-error "����饯��1��̾�������Ǥ�"))
      (when (equal? "" p2name)
        (goto-error "����饯��2��̾�������Ǥ�"))
      (when (< 64 (string-size p1name))
        (goto-error "����饯��1��̾����Ĺ�����ޤ�"))
      (when (< 64 (string-size p2name))
        (goto-error "����饯��2��̾����Ĺ�����ޤ�"))
      (when (equal? p1name p2name)
        (goto-error "����饯��1�ȥ���饯��2��̾����Ʊ���Ǥ�"))
      (when (equal? "" p1sexpr)
        (goto-error "����饯��1��S�������Ǥ�"))
      (when (equal? "" p2sexpr)
        (goto-error "����饯��2��S�������Ǥ�"))
      (when (< 2048 (string-size p1sexpr))
        (goto-error "����饯��1��S����Ĺ�����ޤ�"))
      (when (< 2048 (string-size p2sexpr))
        (goto-error "����饯��2��S����Ĺ�����ޤ�"))
      #f)))

(define (freeplay-output-result params winner-num logs)
  (if (cgi-get-parameter "textonly" params)
    (list
      (cgi-header :content-type "text/plain")
      (intersperse
        "\n"
        (map
          (lambda (x)
            (write-to-string x))
          logs)))
    (let (
          (p1name (or (cgi-get-parameter "p1name" params) ""))
          (p2name (or (cgi-get-parameter "p2name" params) ""))
          (p1sexpr (or (cgi-get-parameter "p1sexpr" params) ""))
          (p2sexpr (or (cgi-get-parameter "p2sexpr" params) ""))
          )
      (let (
            (p1name-re (string->regexp
                         (regexp-quote
                           (string-append "��" p1name "��"))))
            (p2name-re (string->regexp
                         (regexp-quote
                           (string-append "��" p2name "��"))))
            )
        (response-html
          (list
            ;; TODO: �����ץ����
            (html:pre
              (intersperse
                "\n"
                (map
                  (lambda (x)
                    ;; �ʹ֤��ɤߤ䤹���������Ѵ�����
                    (let* ((cmd (cadr x))
                           (proc (hash-table-get *table:cmd->display-proc*
                                                 cmd
                                                 #f))
                           )
                      (if (not proc)
                        (html-escape-string
                          (write-to-string x))
                        (let1 str (apply proc (car x) (cddr x))
                          (cond
                            ((p1name-re str)
                             (html:span :style "color: blue" str))
                            ((p2name-re str)
                             (html:span :style "color: red" str))
                            (else
                              (html:span :style "color: black" str)))))))
                  logs)))))))))


(define (cgi:freeplay-result params)
  ;; ����params�δ�ñ�ʥ����å��ϴ�λ���Ƥ���
  (receive (winner-num logs) (sbattle.scm:battle params)
    ;; ��̤���Ϥ���
    (freeplay-output-result params winner-num logs)))


(define (cgi:freeplay-submit params)
  (let* (
         (p1name (let1 n (or (cgi-get-parameter "p1name" params) "")
                   (if (equal? "" n) (get-name) (or n ""))))
         (p2name (let1 n (or (cgi-get-parameter "p2name" params) "")
                   (if (equal? "" n) (get-name) (or n ""))))
         (new-params (map
                       (lambda (key+vals)
                         (cond
                           ((equal? (car key+vals) "p1name")
                            (list "p1name" p1name))
                           ((equal? (car key+vals) "p2name")
                            (list "p2name" p2name))
                           (else key+vals)))
                       params))
         (errors (check-freeplay-parameter new-params))
         (confirm (cgi-get-parameter "confirm" new-params))
         )
    (if errors
      (cgi:freeplay new-params errors)
      (guard (e
               ((sbattle-timeout? e)
                (cgi:report new-params (ref e 'message)))
               ((sbattle-abnormal-exit? e)
                (cgi:report new-params (ref e 'message)))
               ((<process-abnormal-exit> e)
                (cgi:report new-params "�ץ������۾ｪλ���ޤ���"))
               (else
                 ;; ����¾�Υ��顼
                 (cgi:freeplay new-params (list (ref e 'message)) #f)))
        (if confirm
          (cgi:freeplay-result new-params) ; �ºݤξ���ذܹ�
          (begin
            ;; sbattle.scm:check�ˤ������å������Ԥä����
            (sbattle.scm:check new-params)
            (cgi:freeplay new-params '() #t)))))))


;; ���μ�³���ϡ�
;; check?��#t�ʤ�sbattle.scm��check�դ��ǸƤ֡��֤��ͤϰ�̣������ʤ���
;; check?��#f�ʤ顢�ºݤ���Ʈ���ߥ�졼�Ȥ�Ԥ������Ԥο��ͤ�logs��¿�ͤ��֤���
;; ���餫�����꤬ȯ���������ϡ����顼�㳰��Ȥä����Τ����
(define (%sbattle.scm check? params)
  (let (
        (p1name (cgi-get-parameter "p1name" params))
        (p2name (cgi-get-parameter "p2name" params))
        (p1sexpr (cgi-get-parameter "p1sexpr" params))
        (p2sexpr (cgi-get-parameter "p2sexpr" params))
        (p1hp 100)
        (p2hp 100)
        (args (if check? '("check") '()))
        )
    (let1 p (run-process `(,*gosh-path* ,*sbattle-path* ,@args)
                         :input :pipe
                         :output :pipe
                         ;:error :pipe
                         :wait #f
                         :fork #t
                         )
      (unwind-protect
        (let ((source-port (process-input p))
              (result-port (process-output p)))
          ;; ���ǡ�������������
          (write (list :p1name p1name
                       :p2name p2name
                       :p1sexpr p1sexpr
                       :p2sexpr p2sexpr
                       :p1hp p1hp
                       :p2hp p2hp)
                 source-port)
          (flush source-port)
          ;; ��̤��������
          ;; ��̤ϡ��ʲ��Υѥ�����ͭ������Τǡ����줾����к���ɬ�פˤʤ�
          ;; - result-port��`(ok)���֤�
          ;; - result-port��`(done ,winner ,logs)���֤�
          ;; - result-port��`(error ,errors)���֤�
          ;; - ��λ���ơ�����0�ʳ��ǽ�λ(�������顼)��
          ;; - ���������˽�λ���ʤ��ä�(�����ॢ����)��
          ;; - result-port��������S�����֤�(�Х�)
          ;; �����Ĥ�����ΤˤĤ��Ƥϡ��ü�ʽ�����Ԥ�ɬ�פ����뤬��
          ;;   (��ݡ��Ȳ��̤�Ф�)
          ;;   �ɤ�����Ф褤��
          ;;
          ;; ����r�ˡ���̤�����롣�ʲ��Τɤ줫�ˤʤ�Ȧ��
          ;; - r��ʸ����ʤ顢done�ޤ���error
          (let1 r (call-with-output-string
                    (lambda (buf)
                      (let retry ((times 20))
                        (if (<= times 0)
                          #t
                          (begin
                            (when (byte-ready? result-port)
                              (copy-port result-port buf :unit 'byte))
                            (process-wait p :nohang #t)
                            (when (process-alive? p)
                              (sys-sleep 1)
                              (retry (- times 1))))))
                      ;; �ץ�������λ�������ޤ���retry����ޤ�ã����
                      (process-wait p :nohang #t)
                      (when (process-alive? p)
                        ;; �����ॢ����
                        (error <sbattle-timeout> "�����������ॢ���Ȥ��ޤ���"))
                      ;; race condition�к��ˡ��Ǹ�ˤ⤦����copy���ߤ�
                      (when (byte-ready? result-port)
                        (copy-port result-port buf :unit 'byte))
                      #t))
            (unless (zero? (process-exit-status p))
              ;; �������顼
              (error <sbattle-abnormal-exit>
                     :status (process-exit-status p)
                     "�������ͽ�����̥��顼��ȯ�����ޤ���"))
            (let1 res (read-from-string r)
              (when (eof-object? res)
                (error <sbattle-abnormal-exit>
                       :status (process-exit-status p)
                       "�������ͽ�����̥��顼��ȯ�����ޤ���"))
              (case (car res)
                ((ok) #t)
                ((done) (apply values (cdr res)))
                ((error) (error (apply string-append (cadr res))))
                (else
                  (error "assertion"))))))
        (begin
          (guard (e (else e))
            (process-kill p)) ; ������λ(�ե������������̵��Ȧ�ʤΤ�-9���ɤ�)
          (guard (e (else e))
            (process-wait p :nohang #f)))))))



;; ���μ�³���ϡ���Ʈ���ߥ�졼����¹Ԥ����������꤬����Х��顼�㳰���ꤲ��
;; ����˴�λ�����ʤ顢���Ԥο��ͤ�logs��¿�ͤ��֤�
(define (sbattle.scm:battle params)
  (%sbattle.scm #f params))

;; ���μ�³���ϡ�params������å��������꤬����Х��顼�㳰���ꤲ��
;; �֤��ͤϰ�̣������ʤ�
(define (sbattle.scm:check params)
  (%sbattle.scm #t params))

(define (cgi:freeplay params . opt-errors+ok)
  (let ((errors (guard (e (else '()))
                  (car opt-errors+ok)))
        (check-ok (guard (e (else #f))
                    (cadr opt-errors+ok)))
        ;(mode (cgi-get-parameter "mode" params))
        (textonly (cgi-get-parameter "textonly" params))
        (p1name (let1 n (cgi-get-parameter "p1name" params)
                  (if (equal? "" n)
                    (get-name)
                    (or n ""))))
        (p1sexpr (or (cgi-get-parameter "p1sexpr" params) ""))
        (p2name (let1 n (cgi-get-parameter "p2name" params)
                  (if (equal? "" n)
                    (get-name)
                    (or n ""))))
        (p2sexpr (or (cgi-get-parameter "p2sexpr" params) ""))
        )
    (response-html
      (list
        (html:h1 "��������")
        (if (null? errors)
          '()
          (html:div
            :style "border:solid 1px black"
            (html:strong :style "color: red" "error")
            (html:ul
              (map
                (lambda (e)
                  (html:li
                    (regexp-replace-all
                      #/\n/
                      (html-escape-string e)
                      (tree->string (html:br)))))
                errors))))
        (if (not check-ok)
          '()
          (html:div
            (html:p
              (html:em :style "color: blue"
                       "��Ĥ�S����valid�Ǥ��ꡢ"
                       "�����ǽ�ʻ�����ǧ����ޤ�����"
                       "���Ρ���Ʈ���ϡץܥ���򲡤��Ƥ���������"
                       ))))
        (html:form
          :action (cgi-get-metavariable "SCRIPT_NAME")
          :target "_self"
          :method "post"
          (html:input :type "hidden" :name "c" :value "freeplay-submit")
          (html:table
            (html:tr
              (html:td
                (html:div
                  (html:div "����饯��1��̾�������Ϥ��Ʋ�����"
                            (html:br)
                            "(̤���Ϥξ��ϥ��������������ޤ�)")
                  (html:div
                    "̾��: " (html:input :type "text" :name "p1name" :value p1name)))
                (html:div
                  (html:div "����饯��1��S�������Ϥ��Ʋ�����")
                  (html:textarea :name "p1sexpr" :rows "24" :cols "64" p1sexpr)))
              (html:td
                (html:div
                  (html:div "����饯��2��̾�������Ϥ��Ʋ�����"
                            (html:br)
                            "(̤���Ϥξ��ϥ��������������ޤ�)")
                  (html:div
                    "̾��: " (html:input :type "text" :name "p2name" :value p2name)))
                (html:div
                  (html:div "����饯��2��S�������Ϥ��Ʋ�����")
                  (html:textarea :name "p2sexpr" :rows "24" :cols "64" p2sexpr)))
              ))
          (html:div
            (html:div
              (html:input :type "checkbox"
                          :name "textonly"
                          :value "#t"
                          :checked textonly)
              "��Ʈ��̤�����������ɽ������")
            )
          (html:div
            (html:input :type "submit" :value "S��������å�����")
            "(���ä��餤������ޤ�)"
            (html:br)
            (html:input :type "submit" :name "confirm" :value "��Ʈ����")
            "(�̾�Ͽ��á���Ĺ��10�ä��餤������ޤ�)"
            )
          )
        (html:hr)
        (html:div
          (html:h2
            (html:a :name "history" "S���Хȥ顼�ι�������"))
          (html:p "(���٤ʽ����ϥ��å���Ԥ����礬����ޤ�)")
          *html:history*)
        (html:hr)
        (html:div
          (html:h2
            (html:a :name "explain" "S�����Ϥ�����"))
          (html:p "���Υڡ����ΰ��ֲ��Υ�󥯤��顢����ץ��S���򸫤�����"
                  "�ᤤ�����Τ�ޤ���")
          (html:ul
            (html:li (html:code "(define (battle-main) ...)")
                     (html:br)
                     "�Τ褦�ˡ�battle-main��³����������Ƥ���������"
                     (html:br)
                     "����battle-main��³������λ����ȥڥʥ�ƥ���Ϳ����줿"
                     "��ǡ�battle-main��³�����Ƶ�ư����ޤ���"
                     "ɬ�פʤ顢"
                     (html:br)
                     (html:code "(define (battle-main) ... (battle-main))")
                     (html:br)
                     "�Τ褦�ˡ������Ƶ����Ƥ���������"
                     )
            (html:li "�ޥ��������ȥե�������ɷϤ������R5RS��³���ȡ�"
                     "������Gauche��ĥ��³�������Ѳ�ǽ�Ǥ���")
            (html:li
              "��Ʈ�ΰ٤ˡ��ʲ���«�����󶡤���ޤ���"
              *html:explain*)
            ))
        ;(html:hr)
        ;(html:div
        ;  (html:h2
        ;    (html:a :name "rule" "�ܺ٥롼��"))
        ;  (html:p "�ɤޤʤ��Ƥⲿ�Ȥ��ʤ�ޤ���")
        ;  *html:rule*
        ;  )
        (html:hr)
        (html:ul
          (html:li
            (html:a :href (symbol->path 'freeplay-sample)
                    :target "_blank"
                    "����ץ�S���򸫤�"))
          (html:li
            (html:a :href (symbol->path 'menu)
                    "�������λ���ƥ�˥塼�����"))
          )
        ))))



;;; ----

(define (cgi:report-done params)
  (response-html
    (html:div
      :class "wireframe01"
      (html:p "���顼��ȯ������S������ե�����˵�Ͽ���ޤ�����")
      (html:p "���Ȥ��б����ޤ���")
      (html:hr)
      (html:ul
        (html:li
          (html:a
            :href (symbol->path 'menu)
            "�ȥåפ����"))))))

(define (cgi:report-submit params)
  (define (logging msg)
    (with-output-to-file
      *errorlog-path*
      (lambda ()
        (write msg)
        (newline))
      :if-exists :append))

  (let ((p1sexpr (or (cgi-get-parameter "p1sexpr" params) ""))
        (p2sexpr (or (cgi-get-parameter "p2sexpr" params) ""))
        )
    (logging '----)
    (logging (sys-ctime (sys-time)))
    (logging p1sexpr)
    (logging p2sexpr)
    (location
      (path->url
        (symbol->path 'report-done)))))

(define (cgi:report params . opt-message)
  (let ((message (get-optional opt-message "(������)"))
        (p1sexpr (or (cgi-get-parameter "p1sexpr" params) ""))
        (p2sexpr (or (cgi-get-parameter "p2sexpr" params) ""))
        )
    (response-html
      (html:div
        :class "wireframe01"
        (html:h1 "��̿Ū���顼")
        (html:p "�ʲ�����̿Ū���顼��ȯ�����ޤ�����")
        (html:div (html:strong (html-escape-string message)))
        (html:p "���Υ��顼�θ����Ϥ����餯��"
                "eval/sv��̤������ʬ/�Զ����Ȼפ��ޤ���"
                (html:br)
                "(�ޤ��ϡ����ޤ��ޥ����Ф��۾�˹���٤��ä���)"
                (html:br)
                "eval/sv�δ����ΰ٤ˡ�"
                "����S���ξ����Ͽ���Ƥ������Ǥ��礦����"
                (html:br)
                "�������С����Υܥ���򲡤��ȡ������S����"
                "�����ФΥ��ե�����˵�Ͽ����ޤ���"
                )
        (html:form
          :action (cgi-get-metavariable "SCRIPT_NAME")
          :target "_self"
          :method "post"
          (html:input :type "hidden" :name "c" :value "report-submit")
          (html:input :type "hidden" :name "p1sexpr" :value p1sexpr)
          (html:input :type "hidden" :name "p2sexpr" :value p2sexpr)
          (html:input :type "submit" :value "��Ͽ����"))
        (html:div
          (html:p "�ץ쥤�䡼1��S��")
          (html:pre
            (html-escape-string p1sexpr)))
        (html:div
          (html:p "�ץ쥤�䡼2��S��")
          (html:pre
            (html-escape-string p2sexpr)))
        ))))

(define (cgi:freeplay-sample params)
  (response-html
    (list
      (html:h1 "����ץ륭��饯����")
      (html:p "���ԥڤ��Ƥ��Τޤ޻Ȥ��ޤ���")
      *html:freeplay-sample*
      )))

(define (cgi:menu params)
  (response-html
    (list
      (html:h1 "S���Хȥ顼")
      (html:ul
        (html:li
          (html:a :href (symbol->path 'explain) "�Ϥ����"))
        (html:li
          (html:a ;:href (symbol->path 'play)
                  "��ͥץ쥤(̤����)"))
        (html:li
          (html:a :href (symbol->path 'freeplay) "�ե꡼����"))
        ))))

;;; ----

;;; cgi�ѥ�᡼���ϡ�
(define (main argv)
  (cgi-main
    (lambda (params)
      (let1 c (x->string (or (cgi-get-parameter "c" params) "menu")) ; default
        (let1 cgi-entry-symbol (string->symbol (string-append "cgi:" c))
          (let1 proc (guard (e (else cgi:menu)) ; fallback
                       (eval cgi-entry-symbol (current-module)))
            (proc params)))))
    ;:on-error report-error
    ))



