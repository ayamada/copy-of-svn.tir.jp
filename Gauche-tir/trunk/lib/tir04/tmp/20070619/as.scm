;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; account scheduler

;;; ����:
;;; ���������դ���Ģ���ץꡣ

;;; TODO: �ɤ����齤��/�������뤫�ͤ��롣
;;; - �ǡ��������ʬ����Ω�ץ��������롩
;;; -- �Ȥꤢ�������饤�֥��ʬ�䤰�餤�Ϥ��٤���
;;; --- �⥸�塼��̾�ϡ�
;;; ---- ���ˡ�as.storage�Ȥ���
;;; -- �ץ�ȥ����SETP
;;; - �������ϡ��ե�������������θ�Ψ��������
;;; -- �����Ф�ޤ����ʤ��Ȥ���������ɤ��ʤ顢
;;;    �ե�����ѥ��Τߤ�ž��������Ǥ�ok������
;;;    �ե��������������������Τ��礭�ʥ������Υ��������������/����/����
;;;    �ʤɤλ���¿���١���������Τ򥸥㡼�ʥ�󥰥��˻Ĥ���̣��������
;;;    ����ʤ�ž��������ȿ�Ǥ�����ˡ�Ǥ�褤�Ȼפ���

;;; TODO: url�������ѹ����롣
;;; - /yyyy/mm/dd �ǤϤʤ���
;;;   /groupname/yyyy/mm/dd �Ȥ��롣
;;;   groupname�˵���ͤ�region�����졢������
;;;   .metadata�ե�����ˡ����롼�����������롣
;;; - �ƥ�������ȤϺǽ顢��Ĥ����Υ��롼�פ�°����
;;;   �ץ饤�١��Ȥʾ���Ϥ����˽񤭹���Ȥ������ͤˤ��롣

;;; TODO: ���̾�����Ͽ���ˡ�
;;;       �ǽ����Ͽ������������Ȥ�
;;;       ����Ū�ˡ�root(""����'()����#f����)�ν�ͭ�ԤȤ���褦��
;;;       �������
;;;       �����ơ���̥ǥ��쥯�ȥ�/���롼�פν�ͭ�Ԥϡ�
;;;       ���̥ǥ��쥯�ȥ�/���롼�פؤθ��¤���ġ��Ȥ������ͤȤ��롣
;;;       ����ˤ�äơ����¤�����Ǥ���褦�ˤ��롣


;;; TODO: MIME type����롣
;;; - Content-Type: application/as; data-type=moge
;;; -- application/as�ʳ��ξ��ϡ����Τޤ޽��Ϥ��롣


;;; TODO:
;;; - http://as.tir.jp/one �����֤��롣
;;; - �ץ饰���󥷥��ƥ������������
;;; - ���㡼�ʥ�󥰥���ɬ�ס�
;;; -- �����syslogž���Τ褦�ʡ����Ѥλ��Ȥߤˤ��٤�����
;;; - �ǡ����Ź沽������Τ����ޤ�������ɬ�ܤǤϤʤ�
;;; - PATH_INFO�ǤϤʤ���dns��AAAA�쥳���ɤ�Ȥ�����
;;; -- wiki�ʤ餽��Ǥ⤤����������ϥ���������ǽ�����ˤʤ�١�
;;;    �ս�ˤʤ�Τ����ޤ����ʤ��Τǡ�PATH_INFO���ѤȤ��롣

;;; TODO: ����ϡ�httpd�������������������Τ�ʤ�
;;;       (��������httpd���ˤϻ��֤�������١���������ˤ��Ƥ⡢
;;;        �ޤ������̤�ư����Τ��������Ƥ���ˤ������)

;;; url�ˤĤ���:
;;; - PATH_INFO��wikiname�Ȥ��ƻȤ���
;;; - wikiname���Τϴ���Ū��\w��ʸ���ΤߤȤ��롣
;;; -- %nn���󥳡��ɤ�ɬ�פˤʤ�̾�����Բġ�
;;; -- �������������Թ�塢.���Բġ�
;;; -- �ְ��ʤ��褦�ˡ���ʸ���ΤߤǤι����Ȥ��롣��ʸ���Բġ�
;;; -- ��������С���ok��
;;; - ���ڤ�ʸ����/��
;;; -- root�Τߤ���PATH_INFO��������/�ǽ����������ǽ�Ȥ��롣
;;; -- root�ʳ��ϡ�PATH_INFO��������/�ǽ����ʤ��褦�ˤ��롣
;;; --- �Ȥϸ���������Ū�ˤϡ�path-info-keylist��ȤäƤ���Τǡ�
;;;     �ɤ���ʤΤ��϶��̤Ǥ��ʤ�����
;;;     ���������Υ����ƥफ��PATH_INFO�դ�url��Ф��ݤˤϡ�
;;;     root�ʳ���������/�ǽ����ʤ��褦�ˤ��롢�Ȥ�����«�Ȥ��롣

;;; note:
;;; - ����������wiki�˶ᤤ���֤Ȥ��롣
;;; -- �ǡ����ϡ�PATH_INFO���̤�Υǥ��쥯�ȥ���ꡢ
;;;    ���Υǥ��쥯�ȥ����.content�Ȥ����ե�������ꡢ
;;;    �������rfc822�����ǥ᥿�ǡ������ߤ���¸���롣
;;;    (�Ĥޤꡢ�ɵ�ok���ĥإå��˥᥿�ǡ����������������ǽ�ˤ���)

;;; TODO: ���̤괰�������顢�����Ѳ�ǽ�ʤ褦�ˡ�����ʬ��⥸�塼���ʬ�򤹤롣
;;; - content-tree�����/������󥰤�����ʬ
;;; -- rfc822���ե��������桼�ƥ���ƥ�
;;; - �Ƽ�ץ饰���󥷥��ƥ�
;;; - ��������ȥޥ͡�����
;;; - ����¾

;;; TODO: �ǡ����Υ��ꥭ��å���μ���


;;; TODO: Ʊ�쥢������Ȥ��̡�����³������Ʊ���˥�����Ǥ��ʤ����Ȥߤ�
;;;       ��������ɬ�פ�����


(define-module tir04.tmp.20070619.as
  (use gauche.parameter)

  (use rfc.822)
  (use file.util)
  (use util.list)
  (use dbm)
  (use text.html-lite)
  (use text.tree)
  (use www.cgi)
  (use tir04.cgi.util)
  (use tir04.cgi.ident)
  (use tir04.cgi.session.ident)
  (use tir04.dbm.util)

  (use srfi-19)
  (use gauche.sequence)
  (use srfi-1)

  ;(use wc-frame)
  (use tir04.util.calendar)


  (use tir04.tmp.20070619.as.storage)

  (export
    <cgi-as>
    cgi-as-execute
    ))
(select-module tir04.tmp.20070619.as)


(define-class <cgi-as> ()
  (
   ;; TODO: �����������åȤ��Ȥꤢ����ɬ��
   ;;       (�Ȥꤢ��������̩�ˤ��٤������ͤϥ���åȤǻ��������)
   (information-keywords
     :accessor information-keywords-of
     :init-keyword :information-keywords
     :init-value '())
   (setting-keywords
     :accessor setting-keywords-of
     :init-keyword :setting-keywords
     :init-value '())
   (cgi-ident
     :accessor cgi-ident-of
     :init-keyword :cgi-ident
     :init-value #f)

   ;; private slot
   (csa
     :accessor csa-of
     :init-value #f)
   ))


(define-method informations-of ((self <cgi-as>) keyword . opt-fallback)
  (apply get-keyword keyword (information-keywords-of self) opt-fallback))

(define-method settings-of ((self <cgi-as>) keyword . opt-fallback)
  (apply get-keyword keyword (setting-keywords-of self) opt-fallback))

(define-method initialize ((self <cgi-as>) initargs)
  (next-method)
  (unless (file-exists? (settings-of self :storage-dir))
    (make-directory* (settings-of self :storage-dir)))
  (set!
    (csa-of self)
    (make
      <cgi-session-ident>
      :dbm-type (settings-of self :dbm-type)
      :dbm-path (string-append
                  (settings-of self :storage-dir)
                  "/"
                  "csa")
      :expire-second (* 1 24 60 60)
      :cgi-ident (cgi-ident-of self)))
  #t)



;;; ----




(define (make-response:not-logined self params)
  (make-page
    self
    (list
      (html:h1 (het (informations-of self :title)))
      (html:ul
        (html:li
          (html:login/logout self 'login "������")))
      )))

(define (html:login/logout self type label)
  (make-cgi-session-ident-form
    (csa-of self)
    '()
    type
    (html:input
      :type "submit"
      :value label)))


(define (path-info-keylist->date path-info-keylist)
  (define (str-is-number-only? str)
    (and
      str
      (not
        (#/\D/ str))))
  (define (path-info-keylist->year+month+day+rest)
    ;; �ޤ����ǽ�����Ǥ�����å�
    (let1 year? (car path-info-keylist)
      (if (not
            (and
              (str-is-number-only? year?)
              (= 4 (string-size year?))))
        (values #f #f #f path-info-keylist)
        (let1 month? (and
                       (not (null? (cdr path-info-keylist)))
                       (cadr path-info-keylist))
          (if (not
                (and
                  (str-is-number-only? month?)
                  (= 2 (string-size month?))))
            (values (x->integer year?)
                    1
                    1
                    (cdr path-info-keylist))
            (let1 day? (and
                         (not (null? (cddr path-info-keylist)))
                         (caddr path-info-keylist))
              (if (not
                    (and
                      (str-is-number-only? day?)
                      (= 2 (string-size day?))))
                (values (x->integer year?)
                        (x->integer month?)
                        1
                        (cddr path-info-keylist))
                (values
                  (x->integer year?)
                  (x->integer month?)
                  (x->integer day?)
                  (cdddr path-info-keylist)))))))))
  (cond
    ((not path-info-keylist) (current-date))
    ((null? path-info-keylist) (current-date))
    (else
      ;; �ޤ���path-info-keylist���顢ǯ�����Ǥ����ǽ���Τ������Ǥ���Ф�
      (receive (year month day rest) (path-info-keylist->year+month+day+rest)
        (values
          (if (not year)
            (current-date)
            (make-date 0 0 0 0 day month year (date-zone-offset
                                                (current-date))))
          rest)))))
          
(define (with-dbm self dbm-name rw-mode proc)
  (with-dbm-open
    (settings-of self :dbm-type)
    :path (string-append
            (settings-of self :storage-dir)
            "/"
            (x->string dbm-name))
    :rw-mode rw-mode
    :key-convert #t
    :value-convert #t
    proc))

(define (get-user-info self ident-info)
  ;; dbm����user-info���ɤ�
  (guard (e (else #f)) ; TODO: ���顼����
    (with-dbm
      self
      "user-info"
      :read
      (lambda (dbm)
        (dbm-get dbm (get-keyword :uid ident-info) #f)))))

(define (set-user-info! self ident-info user-info)
  (guard (e (else #f)) ; TODO: ���顼����
    (with-dbm
      self
      "user-info"
      :write
      (lambda (dbm)
        (dbm-put! dbm (get-keyword :uid ident-info) user-info)))))

(define (make-response:logined:user-info
          self params ident-info path-info-keylist user-info)
  (let1 cc (cgi-get-parameter "cc" params)
    ;; �ޤ��ǥ����ѥå�����
    (cond
      ((equal? "confirm" cc)
       (make-response:logined:user-info:confirm
         self params ident-info user-info))
      ((equal? "submit" cc)
       (make-response:logined:user-info:submit
         self params ident-info user-info))
      ((equal? "done" cc)
       (make-response:logined:user-info:done
         self params ident-info user-info))
      (else ; or "edit"
        (make-response:logined:user-info:edit
          self params ident-info user-info)))))

(define (make-response:logined:user-info:confirm
          self params ident-info user-info)
  ;; TODO: �������ƤΥ����å���ɬ��
  (let ((name (cgi-get-parameter "name" params)))
    (make-page
      self
      (make-form
        (self-path/path-info)
        `(
          ("c" "ui")
          ("cc" "submit")
          ("name" ,name)
          )
        (list
          (html:p "�ʲ������Ƥ������Ԥ��ޤ���")
          (html:div
            "̾��: "
            (html:tt
              (het name)))
          (html:input
            :type "submit"
            :value "���������ꤹ��")
          )))))
(define (make-response:logined:user-info:submit
          self params ident-info user-info)
  ;; TODO: �������ƤΥ����å���ɬ��
  (let ((name (cgi-get-parameter "name" params)))
    (set-user-info! self ident-info `(:name ,name))
    (location
      (append-params-to-url
        (self-path/path-info)
        '(("c" "ui")
          ("cc" "done"))))))
(define (make-response:logined:user-info:done
          self params ident-info user-info)
  (make-page
    self
    (list
      (html:p "���꤬��λ���ޤ�����")
      (html:ul
        (html:li
          (html:a
            :href (self-path/path-info)
            "���"))))))
(define (make-response:logined:user-info:edit
          self params ident-info user-info)
  ;; ������ܤ��Խ����̤�Ф�
  ;; TODO: �ޤ��������ǽ���ܤ����ɬ�פ����롣
  ;; - ̾��
  ;; - ¾�ˤϡ�����
  (define (read-from-user-info keyword . opt-fallback)
    (if (not user-info)
      (car opt-fallback)
      (apply get-keyword keyword user-info opt-fallback)))

  (make-page
    self
    (make-form
      (self-path/path-info)
      '(
        ("c" "ui")
        ("cc" "confirm")
        )
      (list
        (if (not user-info)
          (html:p "���̾������ꤵ��Ƥ��ޤ��������ԤäƤ���������")
          (html:p "���̾�����Խ��Ǥ��ޤ���"))
        (html:div
          "̾��: "
          (html:input
            :type "text"
            :name "name"
            :value (read-from-user-info :name "")))
        (html:input
          :type "submit"
          :value "���ꤹ��")
        (html:p
          "(���ΤȤ���̾����������Ǥ��ޤ���"
          "������ܤϺ��塢������ͽ��Ǥ�)")
        (if (not user-info)
          '()
          (list
            (html:hr)
            (html:ul
              (html:li
                (html:a :href (self-path/path-info)
                        "(���)")))))))))

(define (do-redirect self src-path params)
  ;; �ޤ���path�򸡾ڤ��롣
  ;; /�Ϥޤ�ǡ��ʹߤ�\w�ޤ���/�Τߤ�ޤ�ʸ����Ȥ��롣
  ;; â����path�����ʤ顢/�����ꤵ�줿��ΤȤ��ư�����
  (let1 path (if (string=? "" src-path)
               "/"
               src-path)
    (or
      (and-let* ((m (#/^\/[\/\w]*$/ path)))
        (location
          (append-params-to-url
            (string-append (self-path) path)
            (remove
              (lambda (x)
                (equal? (car x) "redirect_path"))
              params))))
      (make-page
        self
        (list
          (html:h1 "error")
          (html:div
            "������path�����ꤵ��ޤ���"))))))

(define (make-response:logined
          self params path-info-keylist ident-info session-parameter)
  ;; �ޤ����桼���������Ф�
  ;; �桼������¸�ߤ��ʤ����ϡ�����Ū��������̤ؤ����ܤ�����
  (let1 user-info (get-user-info self ident-info)
    (if (not user-info)
      (make-response:logined:user-info
        self params ident-info (current-date) user-info)
      ;; CGI�����β���Ԥ�
      (let (
            (redirect-path (cgi-get-parameter "redirect_path" params))
            (c (cgi-get-parameter "c" params))
            )
        (if redirect-path
          (do-redirect self redirect-path params)
          (cond
            ((equal? "ui" c) (make-response:logined:user-info
                               self params ident-info path-info-keylist user-info))
            ((equal? "edit" c) (make-response:logined:edit
                                 self params ident-info path-info-keylist user-info))
            ((equal? "submit" c) (make-response:logined:submit
                                   self params ident-info path-info-keylist user-info))
            (else
              ;; fallback or "display"
              (make-response:logined:display
                self params ident-info path-info-keylist user-info))))))))

(define (make-month m y)
  (make-date 0 0 0 0 1 m y (date-zone-offset (current-date))))

(define (date->prev-month date)
  (if (= (date-month date) 1)
    (make-month 12 (- (date-year date) 1))
    (make-month (- (date-month date) 1) (date-year date))))

(define (date->next-month date)
  (if (= (date-month date) 12)
    (make-month 1 (+ (date-year date) 1))
    (make-month (+ (date-month date) 1) (date-year date))))

(define (url-chop url)
  ;; url�����������Ǥ���Ȥ���ʸ������֤���
  (let1 m (#/\/[^\/]*\/?$/ url)
    (if m
      (m 'before)
      url)))

(define (calendar-html path-info-keylist edit-mode?)
  (let1 date (path-info-keylist->date path-info-keylist)
    ;; TODO: �����դ˥�󥯤�Ĥ���
    ;;       ���/����ޤ��
    ;;       ���/���ؤΥ�󥯤�Ĥ���(nnk�ߴ�)
    ;; �ʲ��Τ褦�ʥե����ޥåȤȤ��롣
    ;; ��    2007/07     �� 
    ;; �� �� �� �� �� �� �� 
    ;; 01 02 03 04 05 06 07 
    ;; 08 09 10 11 12 13 14 
    ;; 15 16 17 18 19 20 21 
    ;; 22 23 24 25 26 27 28 
    ;; 29 30 31 01 02 03 04
    ;; TODO: input text���������ϥܥå�����Ĥ���٤���
    ;;       ���ϤĤ��ʤ����Ĥ���Ȥ��Ƥ��ǡ�
    (define (date->url-month date)
      (format "~a/~4,'0d/~2,'0d"
              (self-path)
              (date-year date)
              (date-month date)))
    (define (date->url-day date)
      (format "~a/~4,'0d/~2,'0d/~2,'0d"
              (self-path)
              (date-year date)
              (date-month date)
              (date-day date)))
    (define (url-filter url)
      (if edit-mode?
        (append-params-to-url
          url
          '(("c" "edit")))
        url))

    (let (
          (prev-month (date->prev-month date))
          (next-month (date->next-month date))
          )
      (html:table
        :summary "calendar"
        (html:thead
          (html:tr
            (html:th :colspan "1" '())
            (html:th
              :colspan "1"
              (if (null? path-info-keylist)
                "��"
                (html:a :href (url-filter
                                (let1 new-url (url-chop (self-path/path-info))
                                  (if (< (string-length (self-path))
                                         (string-length new-url))
                                    new-url
                                    (self-path/slash))))
                        "��")))
            (html:th
              :colspan "3"
              (html:a :href (url-filter (date->url-day (current-date)))
                      "��"))
            (html:th
              :colspan "1"
              (if (null? path-info-keylist)
                "��"
                (html:a :href (url-filter (self-path/slash))
                        "��")))
            (html:th :colspan "1" '())
            )
          (html:tr
            (html:th (html:a :href (url-filter (date->url-month prev-month))
                             "��"))
            (html:th
              :colspan "5"
              (html:a :href (url-filter (date->url-month date))
                      (het
                        (date->string date "~Y/~m"))))
            (html:th (html:a :href (url-filter (date->url-month next-month))
                             "��"))
            )
          (html:tr
            (map
              (lambda (x)
                (html:th
                  (het x)))
              '("��" "��" "��" "��" "��" "��" "��"))))
        (html:tbody
          (map
            (lambda (row)
              (html:tr
                (map
                  (lambda (col)
                    (html:td
                      col))
                  row)))
              (datetime->calendar-list
                (date->datetime date)
                (lambda (datetime target-day? current-month?)
                  (let1 str (format "~2,'0d" (date-day
                                               (datetime->date datetime)))
                    (define (filter-current-month str)
                      (if current-month?
                        str
                        (html:span
                          :style "font-size:0.5em"
                          str)))
                    (let1 link-filter (if edit-mode?
                                        (cut
                                          html:a
                                          :href (url-filter
                                                  (date->url-day
                                                    (datetime->date datetime)))
                                          <>)
                                        ;; TODO: edit-mode���ʳ��ˤ⡢
                                        ;;       �������դ��б����륳��ƥ�Ĥ�
                                        ;;       ����ʤ顢
                                        ;;       ��󥯲����ʤ��Ȥ����ʤ���
                                        ;;       ���ȤǼ����������
                                        identity)
                      (link-filter
                        (if (and
                              (= (date-year (datetime->date datetime))
                                 (date-year (current-date)))
                              (= (date-month (datetime->date datetime))
                                 (date-month (current-date)))
                              (= (date-day (datetime->date datetime))
                                 (date-day (current-date))))
                          ;; TODO: ���ΤޤޤǤ��طʿ����ַϤ��Ȥޤ����ʤ롣
                          ;;       ���餫��portable����ˡ��Ƴ���������
                          (html:span :style "color:red"
                                     (filter-current-month
                                       (het str)))
                          (filter-current-month
                            (het str))))))))))
        (html:tfoot
          (html:tr
            (html:th
              :colspan "7"
              (if edit-mode?
                (html:a :href (self-path/path-info)
                        "(view mode)")
                (html:a :href (append-params-to-url
                                (self-path/path-info)
                                '(("c" "edit")))
                        "(edit mode)")
                ))))))))


(define (html:page-selector-form self path-info-keylist edit-mode?)
  (make-form
    (self-path/path-info)
    (if edit-mode?
      '(("c" "edit"))
      '())
    (html:div
      "�ڡ���ľ�ܰ�ư: "
      (html:input
        :type "text"
        :name "redirect_path"
        :value (let1 p (string-join path-info-keylist "/" 'prefix)
                 (if (string=? p "")
                   "/"
                   p)))
      (html:input
        :type "submit"
        :value "��ư")
      )))

(define (make-response:logined:edit:form
          self params ident-info path-info-keylist user-info)
  (make-page
    self
    (list
      (html:page-selector-form self path-info-keylist #t)
      (html:hr)
      (html:as-edit-form self params ident-info path-info-keylist)
      (html:hr)
      (html:div
        :style "float:right"
        (html:login/logout self 'logout "��������"))
      (calendar-html path-info-keylist #t)
      )))

(define (make-response:logined:edit
          self params ident-info path-info-keylist user-info)
  (let1 cc (cgi-get-parameter "cc" params)
    ;; �ޤ��ǥ����ѥå�����
    (cond
      ((equal? "preview" cc)
       (make-response:logined:edit:preview
         self params ident-info path-info-keylist user-info))
      ((equal? "submit" cc)
       (make-response:logined:edit:submit
         self params ident-info path-info-keylist user-info))
      ((equal? "done" cc)
       (make-response:logined:edit:done
         self params ident-info path-info-keylist user-info))
      (else ; or "form"
        (make-response:logined:edit:form
          self params ident-info path-info-keylist user-info)))))

(define (make-response:logined:display
          self params ident-info path-info-keylist user-info)
  (make-page
    self
    (list
      (html:page-selector-form self path-info-keylist #f)
      (html:hr)
      (html:display self params ident-info path-info-keylist)
      (html:hr)
      ;(html:h1 (het (informations-of self :title)))
      (html:div
        :style "float:right; text-align:right"
        (html:login/logout self 'logout "��������")
        ;; TODO: ̾��ɽ���򤹤٤�������̯
        (html:div
          "̾��: " (html:tt
                       (het (get-keyword :name user-info))))
        (html:div
          (html:a :href (append-params-to-url
                          (self-path/path-info)
                          '(("c" "ui")))
                  "(���̾����Խ�)"))
        )
      (html:div
        :style "float:left"
        (calendar-html path-info-keylist #f))
      )))

(define (html:as-edit-form self params ident-info path-info-keylist)
  (receive (path
            path-dir
            content-lock-file
            path-content-file
            path-dir-config-file) (get-content-parameter self
                                                         path-info-keylist)
    ;; �ޤ������ߤ����Ƥ���Ф�
    ;;;;; TODO: ���Ȥ�
    (let1 pair #f
      (html:div
        (make-form
          (self-path/path-info)
          '(("c" "edit")
            ("cc" "preview")
            )
          (list
            ;; TODO: Content-Type�ѹ��ΰ٤Υ�󥯤��ɲ�
            (html:input
              :type "text"
              :name "text"
              :value "")
            (html:input
              :type "submit"
              :value "����")
            ))))))

(define (get-content-parameter self path-info-keylist)
  (let* (
         (path (string-join
                 path-info-keylist
                 "/"
                 'prefix))
         (path-dir (string-append
                     (settings-of self :storage-dir)
                     "/"
                     "content-tree"
                     path))
         (content-lock-file (string-append
                             (settings-of self :storage-dir)
                             "/"
                             "content-tree.lock"))
         (path-content-file (string-append path-dir "/.content"))
         (path-dir-config-file (string-append path-dir "/.config"))
         )
    (values path
            path-dir
            content-lock-file
            path-content-file
            path-dir-config-file)))


(define (html:display self params ident-info path-info-keylist)
  ;; path-info-keylist���顢�����ǡ�������Ф���Ŭ�ڤ�ɽ������
  ;; TODO: �ޤ��ϡ���������ǥ��쥯�ȥ�ڤ�.content�ե������path�����Ȥ���ޤǺ���
  (receive (path
            path-dir
            content-lock-file
            path-content-file
            path-dir-config-file) (get-content-parameter self
                                                         path-info-keylist)
    ;; ɽ�����٤�����򡢰ʲ��μ��ǵ���
    ;; - �ޤ���path-content��¸�ߤ���ʤ顢������������(rfc822����)
    ;; - ��������content�ޤ���#f���Ф����ʲ�����Ŭ�Ѥ���
    ;; -- path-dir-config�ˤ�롢���餫�����꤬¸�ߤ���ʤ顢�����Ŭ�Ѥ���
    ;;    (��̤�rfc822�����ޤ���#f)
    ;; -- path���б����롢�ե��륿�ץ饰����¸�ߤ���ʤ顢�����Ŭ�Ѥ���
    ;;    (��̤�rfc822�����ޤ���#f)
    ;; -- �Ǹ�ˡ����η�̤�Content-Type�˱��������ϥץ饰������Ѥ��ơ�
    ;;    html����(�ޤ��Ϥ���¾�η���)�Ȥ��ƽ��Ϥ��롣
    ;;    �Ǹ�ޤ�#f�Τޤޤ��ä����ϡ�404�ޤ��Ϥ�����ह��ڡ�����Ф���
    ;; TODO: �ե�����˰۾郎�����硢
    ;;       (�ե������¸�ߤ��뤬�����������¤�̵�����ɤ�ʤ���)
    ;;       Ŭ�ڤ˥��顼���֤��褦�ˤ�����
    ;; â����.content���ɵ����ե�����Ȥ��ơ�����ʥǡ�����
    ;; ί����ޤ�Ƥ����ǽ���⤢��١�
    ;; rfc822������body��ʬ��ľ�ܼ����ޤ��ˡ�
    ;; �ǽ��port���֤Ȥ����ݻ����Ƥ�����ΤȤ��롣
    ;; (port���֤�������¾�ξ��֤��ϡ�������ɤǼ�����ΤȤ���)
    (define (path-dir-config-filter pair)
      ;; TODO: ���Ȥ�
      pair)
    (define (path-plugin-filter pair)
      ;; TODO: ���Ȥ�
      pair)
    (define (rfc822-output-filter pair)
      (if (not pair)
        (list
          (html:h1 "content not found")
          (html:div
            "����url�ˤϥǡ�����¸�ߤ��ޤ���"
            (html:br)
            "�����餯��url�ְ㤤����������줿���Τɤ줫�Ǥ���"
            (html:br)
            "��"
            (html:a
              :href (append-params-to-url
                      (self-path/path-info)
                      '(("c" "edit")))
              "edit mode")
            "�פΤȤ����顢���Ƥ򿷤��˺�����������ǽ�Ǥ���"
            ))
        ;; TODO: ���Ȥ�
        (html:div "�ǡ���������ޤ������Ȥ�ɽ����ʬ��������ޤ���")))

    (let1 path-content-port #f
      (dynamic-wind
        (lambda ()
          ;; TODO: Ʊ���˥�å�����������������
          ;;       (�������ܺ�̤��)
          (set!
            path-content-port
            (open-input-file path-content-file :if-does-not-exist #f)))
        (lambda ()
          (let* ((path-content-headers (if path-content-port
                                         (rfc822-header->list
                                           path-content-port)
                                         #f))
                 (path-content-pair0 (if path-content-port
                                       (cons path-content-headers
                                             path-content-port)
                                       #f))
                 ;; �ޤ��������path-dir-config�����ꤵ�줿�ե��륿�ˤ�����
                 (path-content-pair1 (path-dir-config-filter
                                       path-content-pair0))
                 ;; ���ˡ������path���б�����ץ饰����ե��륿�ˤ�����
                 (path-content-pair2 (path-plugin-filter
                                       path-content-pair1))
                 )
            ;; �Ǹ�ˡ���̤�Content-Type�˱�����Ŭ�ڤ˥ե����ޥåȤ����֤�
            ;; �Ǹ�ޤ�#f���ä����ϡ�404�ޤ��Ϥ�����ह���̤��֤���
            (rfc822-output-filter path-content-pair2)))
        (lambda ()
          ;; TODO: dynamic-wind�������˥�å����Ƥ���ʤ顢
          ;;       �����ǥ����å������
          (when path-content-port
            (close-input-port path-content-port))
          (set! path-content-port #f))))))


(define (make-response:logined:submit
          self params ident-info path-info-keylist user-info)
  ;; TODO: ���Ȥ�
  (let1 new-msg (or (cgi-get-parameter "text" params) "")
    #f)
  (location
    (self-path)))

(define (make-page self . body)
  (cgi-tree-make
    :http-header '(:pragma "no-cache")
    :encoding (x->string (gauche-character-encoding))
    :css-url (informations-of self :css-url)
    :robots "NOINDEX,NOFOLLOW"
    :js #f
    :title (informations-of self :title)
    :body-header #f
    :body-footer (list
                   (html:hr)
                   (html:address
                     (html:a
                       :name "bottom"
                       (informations-of self :title))))
    :body body))







(define-method cgi-as-execute ((self <cgi-as>))
  (cgi-main
    (lambda (orig-params)
      (with-cgi-session-ident
        (csa-of self)
        orig-params
        (lambda (true-params ident-info session-parameter)
          (with-path-info-keylist
            (lambda (path-info-keylist)
              (if ident-info
                (make-response:logined
                  self
                  true-params
                  path-info-keylist
                  ident-info
                  session-parameter)
                (make-response:not-logined
                  self
                  true-params)))))))
    :on-error cgi-on-error/stack-trace
    )
  0)





;;; --------


(provide "tir04/tmp/20070619/as")

