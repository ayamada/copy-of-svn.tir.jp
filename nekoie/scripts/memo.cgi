#!/usr/local/gauche/bin/speedygosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(use dbm)
(use dbm.fsdbm)
(use gauche.parameter)
(use gauche.charconv)
(use srfi-1)
(use www.cgi)
(use text.html-lite)
(use text.tree)
(use util.list)
(use file.util)
(use rfc.822)
(use srfi-13)
(use srfi-19)

(add-load-path "/home/nekoie/copy-of-svn.tir.jp/Gauche-tir/trunk/lib")
(add-load-path "/home/nekoie/copy-of-svn.tir.jp/Gauche-tir/branches/Gauche-tir01/trunk")
(use www.cgi.dispatch-tir)
(use tir04.cgi.util)
(use tir.lock)

(define *dbm-file* "/home/nekoie/data/memo/memo.dbm")
(define *lock-file* "/home/nekoie/data/memo/memo.lock")
(define *log-file* "/home/nekoie/data/memo/memo.log")
(define *encoding* (ces-guess-from-string "" "*JP"))
(define *css-url* "http://css.tir.jp/tir.css")

(define *textarea-rows* "20")
(define *textarea-cols* "40")
(define *textarea-style* "width:90%")

;;; ----
;;; misc
(define (string-empty-or . strs)
  (cond
    ((null? strs) "")
    ((or
       (not (string? (car strs))) ; ʸ����Ǥʤ���Τ�ѥ�����
       (string=? "" (car strs))) (apply string-empty-or (cdr strs)))
    (else (car strs))))

(define (format-date date)
  (date->string
    date
    "~Y/~m/~d ~H:~M:~S (~a)"))

;;; content�β��Ԥ�CRLF�Ȥ���
;;; �������٤ˡ������Ѥμ�³�����Ѱդ���
(define (convert-crlf str)
  (regexp-replace-all #/(\r\n|\r|\n)/ str "\r\n"))

(define (logging sexpr)
  (with-output-to-file
    *log-file*
    (lambda ()
      (write sexpr)
      (newline))
    :if-exists :append
    :buffering :full))


(define (log-filter-map-reverse proc)
  (or
    (with-input-from-file
      *log-file*
      (lambda ()
        (let next ((result '()))
          (let1 line (read-line)
            (if (eof-object? line)
              result
              (let1 mail (guard (e (else #f))
                           (read-from-string line))
                ;; TODO: mail������ʹ�¤�ˤʤäƤ��뤫������å����������ɤ�
                (if (not mail)
                  (begin
                    ;; ���顼�ʤ饹���åפ���
                    ;; TODO: ɬ�פʤ�ɤ�����logging���Ƥ�褤
                    (next result))
                  (let1 one (proc mail) ; filter-mapư���Ԥ�(â���ս�)
                    (next (if one
                            (cons one result)
                            result)))))))))
      :if-does-not-exist #f)
    '()))


(define (output-html . bodies)
  (cgi-tree-make
    :encoding *encoding*
    :css-url *css-url*
    :robots "NOINDEX,NOFOLLOW,NOARCHIVE"
    :title "���"
    :body bodies
    ))



;;; ----
;;; parameters

(define p:params (make-parameter '()))
(define p:path (make-parameter #f))
(define p:dbm (make-parameter #f))
(define (cgp name . opt-args)
  (apply cgi-get-parameter name (p:params) opt-args))


;;; ----
;;; mail accessor
;;; mail�ϡ�car������ƥ�����Ρ�cdr��headers��pair�Ȥ���

;;; base
(define (mail->content mail)
  (car mail))
(define (mail->headers mail)
  (cdr mail))
(define (make-mail content headers)
  (cons content headers))

;;; converter
;;; ������ĤϺ��ΤȤ���valid�Ǥʤ�rfc822�����򰷤äƤ���١�
;;; ������ѹ�������ǽ��ͭ��
;;; NB: ���ˡ����Τޤޤη�������¸������ˤʤä�
#|
(define (dbmval->mail str)
  ;; str�Ͼ��valid��rfc822������ʸ����(valid�Ǥʤ������㳰���ꤲ��)
  (call-with-input-string
    str
    (lambda (p)
      (let1 headers (rfc822-read-headers p :strict? #f)
        (make-mail (get-remaining-input-string p) headers)))))
(define (mail->dbmval mail)
  (with-output-to-string
    (lambda ()
      ;; write headers
      (rfc822-write-headers (mail->headers mail))
      ;; write content
      (display (mail->content mail))
      #t)))
|#

;;; ref/update
(define (mail-header-ref mail field-name . opt-default)
  (apply rfc822-header-ref (mail->headers mail) field-name opt-default))
(define (mail-header-update-one mail field-name new-value)
  ;; ���󥹥��󥹤򹹿�����ΤǤϤʤ������������󥹥��󥹤��äƤ��ä����֤�
  (let1 new-headers (if (mail-header-ref mail field-name)
                      ;; ����
                      (map
                        (lambda (old-header)
                          (let ((old-name (car old-header))
                                (old-value (cadr old-header)))
                            (if (string=?
                                  (string-downcase old-name)
                                  (string-downcase field-name))
                              (list field-name new-value)
                              old-header)))
                        (mail->headers mail))
                      ;; �ɲ�
                      (cons
                        (list field-name new-value) ; TODO: ���ΤޤޤǤ�������
                        (mail->headers mail)))
    (make-mail (mail->content mail) new-headers)))

(define (mail-header-update mail update-headers)
  ;; update-headers�ϡ�headers�����Ȥ���
  (fold
    (lambda (name&value old-mail)
      (apply mail-header-update-one old-mail name&value))
    mail
    update-headers))

(define (mail-update mail content update-headers)
  (make-mail
    content
    (mail->headers
      (mail-header-update mail update-headers))))

(define (mail->url mail)
  (string-append (self-url) "/" (mail-header-ref mail "path")))



(define (get-current-mail)
  (and
    (p:path)
    (dbm-get (p:dbm) (p:path) #f)))


;;; ----

(define-cgi-entry (memo:purge-log:done) '(("c" "p") ("cc" "d"))
  (output-html
    (html:h1 "�Խ�������Ҹ����괰λ")
    (html:p "�Խ�����������򤷤ޤ�����")
    (html:ul
      (html:li
        (html:a :href (self-path) (het "���������"))))))
(define-cgi-entry (memo:purge-log:submit) '(("c" "p") ("cc" "s"))
  #f)

(define-cgi-entry (memo:purge-log) '(("c" "p"))
  (output-html
    (html:h1 (het "�Խ�������Ҹ�����"))
    (html:ul
      (html:li
        (html:a :href (self-path) (het "���������"))))
    (html:hr)
    (html:p
      (html:strong
        ""
        "(����������ʸ���)"
        "(���Ȥ�)"
        ""
        ))
    (html:hr)
    (cgi-entry->html:form
      'memo:purge-log:submit
      :method "post"
      :target "_self"
      :submit-label "�������Խ���������Ҹˤ�����")
    ))




(define-cgi-entry (memo:history:desc) '(("c" "h") ("cc" "d"))
  (define (get-backup-mail)
    (let ((path (p:path))
          (serial (cgp "serial")))
      (let1 r (log-filter-map-reverse
                (lambda (mail)
                  (and
                    (equal? path (mail-header-ref mail "path"))
                    (equal? serial (mail-header-ref mail "serial"))
                    mail)))
        (if (null? r)
          #f
          (car r)))))

  (let1 mail (get-backup-mail)
    (if (not mail)
      (output-html (html:h1 "��������ǡ��������դ���ޤ���"))
      (output-html
        (html:p
          (html:strong
            (het "����ϡ����Υڡ����β��ΥС������Ǥ�")))
        (html:ul
          (html:li
            (cgi-entry->html:a 'memo:history :label "������������"))
          (html:li
            (html:a :href (self-path/path-info)
                    (het "�ǿ��Υڡ��������")))
          )
        (html:div "�Ѹ�̾: " (html:tt (het (mail-header-ref mail "path"))))
        (html:div "�����ֹ�: " (html:tt (het (cgp "serial"))))
        (html:div "��������: "
                  (html:tt
                    (het
                      (format-date
                        (time-utc->date (mail->time-utc mail))))))
        ;; �����ȥ�
        (html:h1 (het 
                   (string-empty-or
                     (mail-header-ref mail "subject")
                     (mail-header-ref mail "path"))))
        ;; ��ʸ��ɽ��
        (render mail)
        ))))

(define-cgi-entry (memo:history:list) '(("c" "h") ("cc" "l"))
  (let1 mail (get-current-mail)
    (if (not mail)
      (output-html (html:h1 "�����ʥ��������Ǥ�"))
      (output-html
        (html:h1 "�����������")
        (html:ul
          (html:li
            (html:a :href (self-path/path-info) "���")))
        (html:hr)
        (html:table
          (html:tr
            (html:th "�����ֹ�")
            (html:th "�����ȥ�")
            (html:th "��������")
            (html:th "����")
            )
          (let1 path (mail-header-ref mail "path")
            (log-filter-map-reverse
              (lambda (old-mail)
                (and
                  (equal? path (mail-header-ref old-mail "path"))
                  (let1 serial (mail-header-ref old-mail "serial")
                    (list
                      (html:tr
                        (html:th :style "text-align: right"
                                 serial)
                        (html:td
                          (cgi-entry->html:a
                            'memo:history:desc
                            :base-url (self-path/path-info)
                            :params `(("serial" ,serial))
                            :label (het
                                     (string-empty-or
                                       (mail-header-ref old-mail "subject")
                                       (mail-header-ref old-mail "path")))))
                        (html:td
                          (html:tt
                            (het
                              (format-date
                                (time-utc->date (mail->time-utc old-mail))))))
                        (html:td :style "text-align: center"
                                 "-")
                        ))))))))
        (html:hr)
        (html:h2 "����")
        (html:ul
          (html:li "���Υڡ����β��ξ��֤򸫤��������ޤ���")
          (html:li "���ɵ��פ����ݤ�����ϻĤ�ޤ���"
                   "(����ϡ�������Ƥ��ޤä�ʸ�Ϥ�ڡ�����"
                   "���Ȥǳ�ǧ����٤ε�ǽ�ʤΤ�)"
                   "��")
          (html:li "�ֺ���פ����ڡ���������򸫤������ϡ���ö��"
                   "����url��Ʊ���ڡ����򿷵��������Ƥ�������"
                   "��")
          (html:li "��������Ҹ�����ˤʤ�ȡ�"
                   "�Ť���Τ����˸���ʤ��ʤ�ޤ�"
                   "(�������ί�ޤ�ȽŤ��ʤ�١������Ҹ����ꤹ��ͽ��Ǥ�)"
                   "��")
          )
        ))))

(define-cgi-entry (memo:history) '(("c" "h"))
  (memo:history:list))

(define (append/unshift filter-proc)
  (let ((mail (get-current-mail))
        (line (cgp "line")))
    (cond
      ((not mail)
       (output-html (html:h1 "�����ʥ��������Ǥ�")))
      ((equal? "" line)
       (location (self-url/path-info)))
      (else
        (let* ((timestamp? (cgp "timestamp"))
               ;; line�������ˤϾ�ˡ�"\r\n"����Ϳ����
               (line-true (if timestamp?
                            (format "~a: ~a\r\n"
                                    (format-date (current-date))
                                    line)
                            (format "~a\r\n" line)))
               (new-content (filter-proc (mail->content mail) line-true))
               (new-date (x->string (time->seconds (current-time))))
               (new-serial (x->string
                             (+ 1
                                (x->number (mail-header-ref mail "serial")))))
               (new-mail (mail-update
                           mail
                           (convert-crlf new-content)
                           `(("date" ,new-date)
                             ("serial" ,new-serial)
                             )))
               )
          ;; ��¸����
          (dbm-put! (p:dbm) (p:path) new-mail)
          ;; ���Υڡ��������Ф�
          (location (self-url/path-info))
          )))))
  
(define-cgi-entry (memo:unshift) '(("c" "u"))
  ;; unshift(��Ƭ���ɵ�)
  (append/unshift
    (lambda (content line)
      ;; line�������ϴ��˲��Ԥ��Ĥ��Ƥ���Τǡ����Τޤ޷�礹�������ok
      (string-append line content))))

(define-cgi-entry (memo:append) '(("c" "a"))
  ;; append(�������ɵ�)
  (append/unshift
    (lambda (content line)
      ;; content�����������Խ����Ǥʤ����ϡ�"\r\n"����Ϳ����ɬ�פ�����
      ;; â����content�������˶�ʸ����λ��ˤϡ�"\r\n"����Ϳ���ƤϤ����ʤ�
      (if (string=? "" content)
        line
        (string-append
          content
          (if (#/\n$/ content) "" "\r\n")
          line)))))

(define-cgi-entry (memo:delete:done) '(("c" "d") ("cc" "d"))
  (output-html
    (html:h1 "�ڡ��������λ")
    (html:p "�ڡ������������ޤ�����")
    (html:ul
      (html:li
        (html:a :href (self-path) (het "���������"))))))

(define-cgi-entry (memo:delete:submit) '(("c" "d") ("cc" "s"))
  (let1 mail (get-current-mail)
    (if (not (equal? (cgp "serial")
                     (mail-header-ref mail "serial")))
      (output-html
        (html:h1 "���顼")
        (html:p "���Υڡ����ϡ����ʤ��γ�ǧ��ˡ�"
                "¾��ï���ˤ�ä��ѹ�����ޤ�����"
                (html:br)
                "�⤦���١����Υڡ������ǧ�������侩���ޤ���"
                )
        (html:hr)
        (html:ul
          (html:li
            (html:a :href (self-path/path-info)
                    "���Υڡ������ǧ����"))))
      (begin
        ;; �������
        (guard (e (else e))
          (dbm-delete! (p:dbm) (p:path)))
        ;; �Ť�mail����˻Ĥ�
        (guard (e (else e))
          (logging mail))
        ;; done�˥�����쥯��
        (location
          (cgi-entry->url 'memo:delete:done
                          :params `(("path" ,(p:path)))
                          :base-url (self-url)))))))

(define-cgi-entry (memo:delete:confirm) '(("c" "d") ("cc" "c"))
  (let1 mail (get-current-mail)
    (if (not mail)
      (output-html (html:h1 "�����ʥ��������Ǥ�"))
      (output-html
        (html:h1 (het "�ڡ������"))
        (html:p
          (html:strong
            "�ڡ�����"
            (het (mail-header-ref mail "subject"))
            "��( "
            (het (mail-header-ref mail "path"))
            " )�������ޤ���"
            ))
        (cgi-entry->html:form
          'memo:delete:submit
          :method "post"
          :target "_self"
          :params `(
                    ("serial" ,(mail-header-ref mail "serial"))
                    )
          :submit-label "�����˺��")
        (html:hr)
        (html:ul
          (html:li
            (html:a :href (self-path/path-info) (het "���Υڡ����򸫤�")))
          (html:li
            (html:a :href (self-path) (het "���������"))))
        ))))

(define-cgi-entry (memo:delete) '(("c" "d"))
  ;; delete(���/��ǧ�դ�)
  (memo:delete:confirm))

(define-cgi-entry (memo:edit:done) '(("c" "e") ("cc" "d"))
  (output-html
    (html:h1 "�ڡ���������λ")
    (html:p "�ڡ�������������ޤ�����")
    (html:ul
      (html:li
        (let1 url (self-url/path-info)
          (html:a :href url (het url))))
      (html:li
        (html:a :href (self-path) (het "���������"))))))

(define-cgi-entry (memo:edit:submit) '(("c" "e") ("cc" "s"))
  (let1 mail (get-current-mail)
    ;; �ޤ������å���Ԥ�
    (or
      (and-let* ((errors (memo:edit:check mail)))
        (output-html
          (html:h1 "���顼")
          (map
            (lambda (x)
              (html:div (html:strong (het x))))
            errors)))
      (memo:edit:check-serial mail)
      (let* (
             (date (if (equal? "#t" (cgp "sage"))
                     (mail-header-ref mail "date")
                     (x->string (time->seconds (current-time)))))
             (serial (x->string
                       (+ 1
                          (x->number (mail-header-ref mail "serial")))))
             (new-mail (mail-update
                         mail
                         (convert-crlf (cgp "content"))
                         `(("subject" ,(cgp "subject"))
                           ("date" ,date)
                           ("serial" ,serial)
                           ("status" "ok")
                           )))
             )
        ;; ��¸����
        (dbm-put! (p:dbm) (p:path) new-mail)
        ;; �Ť�mail����˻Ĥ�
        (guard (e (else e))
          (logging mail))
        ;; done�˥�����쥯��
        (location
          (cgi-entry->url 'memo:edit:done
                          :base-url (self-url/path-info)))))))

(define (memo:edit:check mail)
  ;; �ѥ�᡼�������Ƥ����꤬�ʤ���С�#f���֤�
  ;; �����Ǥʤ���С����顼���Ƥ�ʸ�Ϥ�list���֤�
  (define (check-subject)
    (let1 subject (cgp "subject")
      (cond
        ((not subject) "�����ȥ뤬����ޤ���")
        ;((zero? (string-size subject)) "�����ȥ뤬���Ǥ�")
        ((< 64 (string-size subject)) "�����ȥ뤬Ĺ�����ޤ�")
        (else #f))))
  (define (check-content)
    (let1 content (cgp "content")
      (cond
        ((not content) "��ʸ������ޤ���")
        ;((zero? (string-size content)) "��ʸ�����Ǥ�")
        ((< 65535 (string-size content)) "��ʸ��Ĺ�����ޤ�")
        (else #f))))
  (define (check-all)
    (filter-map
      identity
      (list
        (check-subject)
        (check-content))))
  (if (not mail)
    '("���Υڡ����Ϻ�������Ƥ��ޤ���")
    (let1 errors (check-all)
      (if (null? errors)
        #f
        errors))))

(define (memo:edit:check-serial mail)
  (and
    (not (equal? (cgp "serial")
                 (mail-header-ref mail "serial")))
    (output-html
      (html:h1 "���顼")
      (html:p "���Υڡ����ϡ����ʤ����Խ���ˡ�"
              "¾��ï���ˤ�ä��ѹ�����ޤ�����"
              (html:br)
              "�⤦���١����Υڡ��������Խ���ľ���Ƥ���������"
              )
      (html:hr)
      (html:p "���ߤΤ��Υڡ����ȡ����ʤ����Խ��������Ƥκ�ʬ�ϰʲ����̤�Ǥ���")
      (html:p "(TODO: ���ȤǤĤ���)")
      (html:hr)
      (html:p "���ʤ����Խ�������ʸ�ϰʲ����̤�Ǥ���")
      (html:textarea :name "content"
                     :rows *textarea-rows*
                     :cols *textarea-cols*
                     :style *textarea-style*
                     (cgp "content"))
      (html:hr)
      (html:ul
        (html:li
          (html:a :href (self-path/path-info)
                  :target "_blank"
                  "���Υڡ������̲��̤ǳ���"))
        (html:li
          (cgi-entry->html:a
            'memo:edit
            :base-url (self-path/path-info)
            :target "_blank"
            :label "���Υڡ������Խ����̤��̲��̤ǳ���"))))))

(define-cgi-entry (memo:edit:preview) '(("c" "e") ("cc" "p") ("just" :*))
  (memo:edit:submit))

(define-cgi-entry (memo:edit:preview) '(("c" "e") ("cc" "p"))
  (let1 mail (get-current-mail)
    ;; �ޤ������å���Ԥ�
    (or
      (and-let* ((errors (memo:edit:check mail)))
        (output-html
          (html:h1 "���顼")
          (map
            (lambda (x)
              (html:div (html:strong (het x))))
            errors)))
      (memo:edit:check-serial mail)
      ;; �ץ�ӥ塼����ɽ��
      (output-html
        (html:h1 (het "�ץ�ӥ塼"))
        (html:p
          (html:strong
            "����ϳ�ǧ���̤Ǥ���"
            (html:br)
            "���ֲ��Ρ���¸�ץܥ���򲡤��Ƥ���������"
            ))
        (cgi-entry->html:form
          'memo:edit:submit
          :method "post"
          :target "_self"
          :params `(
                    ("subject" ,(cgp "subject"))
                    ("content" ,(cgp "content"))
                    ("serial" ,(cgp "serial"))
                    ("sage" ,(cgp "sage"))
                    )
          :internal-html (edit-form-preview
                           :subject (cgp "subject")
                           :content (cgp "content")
                           ))
        ))))
(define-cgi-entry (memo:edit:form) '(("c" "e") ("cc" "f"))
  ;; �ե�����
  (let1 mail (get-current-mail)
    (if (not mail)
      (output-html
        (html:h1 "���顼")
        (html:p "���Υڡ����Ϻ�������Ƥ��ޤ���"))
      (output-html
        (html:h1 (het "�Խ�"))
        (html:div
          (html:a :href (self-path/path-info) "���"))
        (html:hr)
        (cgi-entry->html:form
          'memo:edit:preview
          :base-url (self-path/path-info)
          :params `(("serial" ,(mail-header-ref mail "serial")))
          :internal-html (edit-form
                           :subject (mail-header-ref mail "subject")
                           :content (mail->content mail)
                           ))
        (html:hr)
        (html:h2 "����")
        (html:ul
          (html:li "���������Ƥ�񤤤Ƥ���������")
          (html:li "html�����ϻȤ��ޤ���")
          (html:li "url�Ͼ���˥�󥯤ˤʤ�ޤ���")
          (html:li "�ڡ������ư�����������ϡ��������ڡ������ä�Ʊ�����Ƥ�񤭹���Ǥ��顢�Ť��ڡ����������Ƥ���������")
          )
        ))))

(define-cgi-entry (memo:edit) '(("c" "e"))
  ;; edit(�Խ�/�ץ�ӥ塼�դ�/¾���ѹ������å�ͭ��/sage��ǽͭ��)
  (memo:edit:form))

(define (render mail)
  (html:div
    :style "border:1px black solid; padding: 4px; margin: 4px; font-family: monospace"
    (text->inline-html
      (string-empty-or (mail->content mail) "(��ʸ������ޤ���)")
      :url->href:keywords '(:target "_blank"))))

(define-cgi-entry (memo:view) '(("c" "v"))
  ;; view(ɽ��)
  (let1 mail (get-current-mail)
    (cond
      ((not mail)
       (output-html
         (html:h1 "���顼")
         (html:p "�ڡ��������դ���ޤ���")))
      (else
        (output-html
          ;; �إå�
          ;; ���ΤȤ���ϥإå�̵��
          ;; �����ȥ�
          (html:h1 (het 
                     (string-empty-or
                       (mail-header-ref mail "subject")
                       (mail-header-ref mail "path"))))
          ;; ��Ƭ���ɲä���ե�����
          (cgi-entry->html:form
            'memo:unshift
            :base-url (self-path/path-info)
            :internal-html (list
                             (html:input :type "checkbox"
                                         :name "timestamp"
                                         :value "#t"
                                         :title "�����դ�")
                             (html:input :type "text"
                                         :name "line"
                                         :size "40"
                                         :value "")
                             (html:input :type "submit" :value "��Ƭ���ɵ�")))
          ;; ��ʸ��ɽ��
          (render mail)
          ;; �������ɲä���ե�����
          (cgi-entry->html:form
            'memo:append
            :base-url (self-path/path-info)
            :internal-html (list
                             (html:input :type "checkbox"
                                         :name "timestamp"
                                         :value "#t"
                                         :title "�����դ�")
                             (html:input :type "text"
                                         :name "line"
                                         :size "40"
                                         :value "")
                             (html:input :type "submit" :value "�������ɵ�")))
          (html:hr)
          ;; ɬ�פ��ղþ��󤬤���ʤ顢�������ɲä���
          ;; TODO: ����������ɽ��
          ;; �եå�
          (html:div
            (html:tt
              ;; ����
              (html:a :href (self-path) (het "���������"))
              " | "
              ;; �Խ�
              (cgi-entry->html:a 'memo:edit
                                 :base-url (self-path/path-info)
                                 :label "�ڡ����Խ�")
              " | "
              ;; ����
              (cgi-entry->html:a 'memo:history
                                 :base-url (self-path/path-info)
                                 :label "��������")
              " | "
              ;; ���
              (cgi-entry->html:a 'memo:delete
                                 :base-url (self-path/path-info)
                                 :label "�ڡ������")
              ))
          )))))



(define-cgi-entry (memo:new:done) '(("c" "n") ("cc" "d"))
  (let1 mail (get-current-mail)
    (if (not mail)
      (output-html "���������������ǤϤ���ޤ���")
      (output-html
        (html:h1 "�ڡ���������λ")
        (html:p "�������ڡ�������������ޤ�����")
        (html:ul
          (html:li
            (cgi-entry->html:a 'memo:edit
                               :base-url (self-path/path-info)
                               :label "����³�����ڡ������Խ���Ԥ�"))
          (html:li
            (html:a :href (self-path) (het "���������"))))))))

(define (memo:new:check)
  ;; �ѥ�᡼�������Ƥ����꤬�ʤ���С�#f���֤�
  ;; �����Ǥʤ���С����顼���Ƥ�ʸ�Ϥ�list���֤�
  (define (check-path)
    (let1 path (cgp "path")
      (cond
        ((not path) "�Ѹ�̾������ޤ���")
        ((zero? (string-size path)) "�Ѹ�̾�����Ǥ�")
        ((not (#/^[\w\-]+$/ path)) "�Ѹ�̾��Ⱦ�ѱѿ��ȡ�_�ס�-�פΤ߻��Ѳ�ǽ�Ǥ�")
        ((< 32 (string-size path)) "�Ѹ�̾��Ĺ�����ޤ�")
        ((dbm-get (p:dbm) path #f) "���αѸ�̾�ϴ���¸�ߤ��ޤ�")
        (else #f))))
  (define (check-all)
    (filter-map
      identity
      (list
        (check-path)
        )))
  (let1 errors (check-all)
    (if (null? errors)
      #f
      errors)))

(define-cgi-entry (memo:new:submit) '(("c" "n") ("cc" "s"))
  ;; �ޤ������å���Ԥ�
  (or
    (and-let* ((errors (memo:new:check)))
      (output-html
        (html:h1 "���顼")
        (map
          (lambda (x)
            (html:div (html:strong (het x))))
          errors)))
    (let ((path (cgp "path"))
          (serial -1) ; �����������Ѥ�Ȥäƹ�������
          )
      (log-filter-map-reverse
        (lambda (mail)
          (when (equal? path (mail-header-ref mail "path"))
            (let1 old-serial (x->number (mail-header-ref mail "serial"))
              (when (< serial old-serial)
                (set! serial old-serial))))
          #f)) ; �֤��ͤϻȤ�ʤ��ΤǾ��#f���֤�
      ;; ��¸����
      (dbm-put!
        (p:dbm)
        path
        (make-mail
          ""
          `(("content-type" "text/memo")
            ("path" ,path)
            ("subject" "")
            ("date" ,(x->string (time->seconds (current-time))))
            ("serial" ,(x->string (+ 1 serial)))
            ("status" "ok")
            )))
      ;; done�˥�����쥯��
      (location
        (cgi-entry->url 'memo:new:done
                        :base-url (string-append (self-url) "/" path))))))

(define (edit-form-preview . keywords)
  (let-keywords keywords (
                          (subject "")
                          (content "")
                          )
    (list
      (html:div
        (html:span (het "�ڡ����Υ����ȥ�: "))
        (html:em (het subject)))
      (html:div
        (html:span (het "��ʸ: "))
        (html:br)
        (html:div
          :style "border:1px black solid; padding: 4px; margin: 4px; font-family: monospace"
          (text->inline-html
            content
            :url->href:keywords '(:target "_blank"))))
      (html:input :type "submit" :value "��¸")
      )))

(define (edit-form . keywords)
  (let-keywords keywords (
                          (subject "")
                          (content "")
                          )
    (list
      (html:div
        (html:table
          (html:tr
            (html:td "��������")
            (html:th
              :style "border:1px black solid; padding: 4px; margin: 4px"
              :title "�ȥ�ץ륯��å�������Ǥ��ޤ�"
              (het (format-date (current-date))))
            (html:td "")
            )))
      (html:div
        (html:em (het "�ڡ����Υ����ȥ�: "))
        (html:input :type "text"
                    :name "subject"
                    :size "40"
                    :value subject)
        ;(html:br)
        ;(het "(���ܸ��ok)")
        )
      (html:div
        (html:em (het "��ʸ: "))
        (html:br)
        (html:textarea :name "content"
                       :rows *textarea-rows*
                       :cols *textarea-cols*
                       :style *textarea-style*
                       content))
      (html:input :type "submit" :value "��ǧ")
      " "
      (html:input :type "checkbox" :name "sage" :value "#t" :id "sage")
      (html:label :for "sage"
        (het "�����������ѹ����ʤ�(sage��ǽ)"))
      (html:div
        :style "display: none"
        (html:br)
        (html:input :type "submit" :name "just" :value "¨ȿ��")
        "(��)"
        )
      )))

(define-cgi-entry (memo:new:form) '(("c" "n") ("cc" "f"))
  (output-html
    (html:h1 (het "�������ڡ��������"))
    (html:div
      (html:a :href (self-path/path-info) "���"))
    (html:hr)
    (cgi-entry->html:form
      'memo:new:submit
      :method "post"
      :target "_self"
      :internal-html (list
                       (html:div
                         (html:em (het "�ڡ����αѸ�̾: "))
                         (html:input :type "text"
                                     :name "path"
                                     :size "24"
                                     :value "")
                         (html:br)
                         (het "(url�˻Ȥ��ޤ� / �ѿ���ʸ���ǡ�����ϡ�_�ס�-�פΤ߻��Ѳ�ǽ / ����ϰ��ٷ�᤿���ѹ��Ǥ��ޤ���)"))
                       (html:input :type "submit" :value "�ڡ�������")))
    ))

(define-cgi-entry (memo:new) '(("c" "n"))
  ;; new(��������)
  (memo:new:form))

(define-cgi-entry (memo:fallback) '()
  ;; �ѥ�᡼������̵������fallback
  ;; ���λ��ϡ�(p:path)�򸫤�Ƚ�Ǥ���
  (if (p:path)
    (memo:view)
    (memo:list)))

(define (mail-date->time-utc mail-date)
  (or
    ;; mail-date��rfc822������Date�񼰤��ä����
    (and-let* ((date (rfc822-date->date mail-date)))
      (date->time-utc date))
    ;; mail-date��time-utc�����ξ������ä����
    (and (number? mail-date) (seconds->time mail-date))
    ;; mail-date��time-utc�����ξ�����ʸ������ä����
    (and-let* ((num (string->number mail-date)))
      (seconds->time num))
    ;; �ɤ�ˤ�ޥå����ʤ��ä����
    #f))
(define (mail->time-utc mail)
  (and-let* ((mail-date-string (mail-header-ref mail "date")))
    (mail-date->time-utc (string->number mail-date-string))))
(define *time-zero*
  (seconds->time 0))

(define-cgi-entry (memo:list) '(("c" "l"))
  ;; list(����ɽ��/���������߽�/�ڡ�����̵��)
  (let1 data (sort
               (filter
                 identity
                 (dbm-map
                   (p:dbm)
                   (lambda (key val)
                     (let1 mail val
                       ;; �������Ƥ���ʤ�#f���֤�(ɽ�����ʤ�)
                       ;; NB: ���κ�������å������פˤʤä�������
                       ;;     �����Ʊ�ͤ�����Ԥ���ǽ���Ϥ���Τǡ�
                       ;;     �Ĥ��Ƥ���
                       (and
                         ;(not (equal? (mail-header-ref mail "status")
                         ;             "deleted"))
                         mail)))))
               (lambda (mail-a mail-b)
                 ;; �����Ƚ�ϡ��ʲ�����Ӥ���
                 ;; - �ǽ���������(Date)���Ƕ�Τ�Τۤɾ�
                 ;; - Date��¸�ߤ��ʤ���Τ�ͥ��Ū�˲�
                 (< (time->seconds (or (mail->time-utc mail-b) *time-zero*))
                    (time->seconds (or (mail->time-utc mail-a) *time-zero*)))))
    ;; data(mail��list)�򡢾夫��������ɽ������Τ�
    (output-html
      (html:h1 (het "������"))
      (html:div
        (cgi-entry->html:a 'memo:new :label "�������ڡ������������"))
      (html:hr)
      (if (null? data)
        (html:div (html:strong (het "��Ĥ�ڡ���������ޤ���")))
        (html:table
          ;(html:tr
          ;  (html:th "�����ȥ�")
          ;  (html:th "��������")
          ;  (html:th "�Խ�")
          ;  (html:th "���")
          ;  )
          (map
            (lambda (mail)
              (html:tr
                (html:td
                  (html:a :href (mail->url mail)
                          (het
                            (string-empty-or
                              (mail-header-ref mail "subject")
                              (mail-header-ref mail "path")))))
                (html:td
                  (html:tt
                    (het
                      (format-date
                        (time-utc->date (mail->time-utc mail))))))
                (html:td
                  (cgi-entry->html:form 'memo:edit
                                        :base-url (mail->url mail)
                                        :submit-label "�Խ�"))
                (html:td
                  (cgi-entry->html:form 'memo:delete
                                        :base-url (mail->url mail)
                                        :submit-label "���"))
                ))
              data)))
      (html:hr)
      ;(html:div
      ;  (cgi-entry->html:a 'memo:backup :label "�Хå����å׳�ǧ(̤����)"))
      ;(html:hr)
      ;(html:div
      ;  (cgi-entry->html:a 'memo:purge-log :label "�Խ���������Ҹˤ�����"))
      ;(html:hr)
      (html:div (het "�ʾ�Ǥ�"))
      )))

(define (main args)
  (cgi-main
    (lambda (params)
      (with-write-locks
        (lambda ()
          ;; dbm���Ѱդ���
          (let1 dbm #f
            (dynamic-wind
              (lambda ()
                (set! dbm (dbm-open <fsdbm>
                                    :path *dbm-file*
                                    :rw-mode :write
                                    :key-convert #f
                                    :value-convert #t
                                    ))
                #t)
              (lambda ()
                (parameterize ((p:params params)
                               (p:path (and-let* ((pl (get-path-info-keylist)))
                                         (string-join pl "/")))
                               (p:dbm dbm))
                  ;; �ǥ����ѥå�
                  ((cgi-params-dispatch params))))
              (lambda ()
                (guard (e (else #f))
                  (dbm-close dbm))
                (set! dbm #f)
                #t))))
        *lock-file*))
    :on-error cgi-on-error/stack-trace))







