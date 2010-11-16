;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; account scheduler

;;; ����: ����osŪ��cms��

;;; �����λ��ͤ����ѹ������ս�:
;;; - �¥ե����륷���ƥ��mount���ǽ�ˤ���(�ʲ������ˡ�mountdir�ȸƤ�)
;;;   (mountdir�����۲��ϡ��̾��httpdƱ�ͤε�ư�ˤʤ�)
;;; -- �����ǡ�%nn�β�᤬ɬ�פˤʤꡢ�ޤ�������˱ƶ�����롢
;;;    ���ݤʥ������ƥ�������꤬ȯ�����������դ���ɬ�פ����롣
;;; - �ü�ե�����̾���ѹ�
;;; -- .content �� .as_content
;;; -- .metadata �� .as_metadata
;;; - anonymous(�������)�桼���κ���
;;; -- �ǡ����ĥ꡼���Τ�anonymous�桼�������ԲĻ�ˤ���н����̤�ε�ư�ˤʤ�
;;; - ���롼�״������ѹ�
;;; -- �ܺ�̤��
;;; - ������ǽ��ץ饰����ΰ�ĤȤ���
;;; -- url�� /path/to/as.cgi/login �Τ褦�ˤʤ�ͽ��
;;; - directory index��.as_content��Content-Type�ȥץ饰���󥷥��ƥ�Ǽ¸�����
;;; - �ץ饰���󥷥��ƥ�ξܺٵͤ�
;;; -- �ץ饰�����ʣ���μ��ब�����ΤȤ��롣
;;; --- ���������ץ饰����
;;;     ľ��path�����֤��졢�¹Ԥ�����Ρ�
;;;     .as_content�ΰ쵡ǽ�Ȥ��Ƽ�������롣
;;;     ���Τ�cgi�γƵ�ǽ��delegate���Ƥ��������
;;; --- �饤�֥��ץ饰����
;;;     ���������ץ饰���󤫤�饤�֥��Ū�˻��Ȥ����ץ饰����
;;;     mountdir����Ρ�mountlib�ˤ�äƼ�������롣
;;;     ���Τ�gauche�Υ⥸�塼�륷���ƥऽ�Τ�Ρ�
;;; ---- �����ͤǤ������������ꡢ
;;;      �֤������饤�֥��ץ饰����פ��Ȥ�������
;;;      as���Τ����Τ���ɬ�פ����롣
;;;      (���դ���ʣ���Υ饤�֥��ץ饰������ɲä����褦�ʾ�������)
;;;      �ɤ�������ɤ���
;;; --- �����ƥ�ץ饰����
;;;     os�Υ⥸�塼�뵡ǽ�Τ褦�ˡ�
;;;     �����ƥऽ�Τ�Τ˵�ǽ���ɲä���٤Υץ饰����
;;;     ɬ�ܡ�
;;;     ����Ū�ʵ�ǽ�⡢������Ѥ��Ƽ������롣
;;;     ���������ͤξܺ٤Ϥޤ�̤�ꡣ
;;; -- �ץ饰����ǥ��쥯�ȥ꼫�Τ⡢���Υ����ƥ�Ǵ�������
;;; --- �����ƥ�������ȥ����ƥ���˲����Ƥ��ޤ���ǽ�����Ф뤬��
;;;     ������̾��os��Ʊ�����Ȥ������ǡġ�
;;; --- ���������Ū�ʥץ饰����ϡ��ǡ����ĥ꡼�γ����֤���
;;;     ���ä�������ʤ餽�ä�������ͥ�褵���롢�Ȥ������ˤˤ��롣
;;; - �ǡ����Ź沽��ǽ���Τ�ɬ�ܤˤ�����ˤ�����
;;;   (�Ĥޤꡢ�Ź沽key��ɬ�פȤ���)
;;;   �������������ϸ�󤷡�
;;;   �ޤ����ǽ�Ū�ˡ��ǥե���Ȥ�ͭ���ˤ��뤫�ɤ�����̤�ꡣ
;;;   �Ź沽��blowfish����Ѥ���ͽ�ꡣ
;;;   (�����ʣ���ΰŹ沽����������Ǥ���褦�ˤ��٤�����)


;;; TODO: ���ܥ��åȤȤʤ롢�ǡ����ĥ꡼�����桼�ƥ���ƥ����ޥ�ɤ�
;;;       ����ʪ�˴ޤ��ɬ�פ����롩
;;;       (���ܵ�ǽ�ΰ������ץ饰���󲽤���Ƥ���١�
;;;        �ץ饰����Ȥ��ƥǡ����ĥ꡼�۲��˴ޤ��ɬ�פ�����)


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

;;; TODO: ���̤괰�������顢�����Ѳ�ǽ�ʤ褦�ˡ�����ʬ��⥸�塼���ʬ�򤹤롣
;;; - content-tree�����/������󥰤�����ʬ
;;; -- rfc822���ե��������桼�ƥ���ƥ�
;;; - �Ƽ�ץ饰���󥷥��ƥ�
;;; - ��������ȥޥ͡�����
;;; - ����¾

;;; TODO: �ǡ����Υ��ꥭ��å���μ���

;;; TODO: Ʊ�쥢������Ȥ��̡�����³������Ʊ���˥�����Ǥ��ʤ����Ȥߤ�
;;;       ��������ɬ�פ�����


;;; �����������ɤΥ⥸�塼�빽¤:
;;; as.cgi : cgi�ѥ��󥿡��ե�����
;;;          (httpd���󥿡��ե����������뤫�⤷��ʤ��Τ�ʬ���Ȥ�)
;;; as.storage : �ǡ����ĥ꡼�˥�������������ʬ�򥫥ץ��벽
;;; ¾�Ͻ�����


(define-module tir04.tmp.20070919.as.cgi
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
  ;(use tir04.util.calendar)


  (use tir04.tmp.20070919.as.storage)

  (export
    <as-cgi>
    as-cgi-execute
    ))
(select-module tir04.tmp.20070919.as.cgi)


(define-class <as-cgi> ()
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
   (as-storage
     :accessor as-storage-of
     :init-value #f)
   ))


(define-method information-of ((self <as-cgi>) keyword . opt-fallback)
  (apply get-keyword keyword (information-keywords-of self) opt-fallback))

(define-method setting-of ((self <as-cgi>) keyword . opt-fallback)
  (apply get-keyword keyword (setting-keywords-of self) opt-fallback))

(define-method initialize ((self <as-cgi>) initargs)
  (next-method)
  (unless (file-exists? (setting-of self :storage-dir))
    (make-directory* (setting-of self :storage-dir)))
  (set!
    (csa-of self)
    (make
      <cgi-session-ident>
      :dbm-type (setting-of self :dbm-type)
      :dbm-path (string-append
                  (setting-of self :storage-dir)
                  "/"
                  "csa")
      :expire-second (* 1 24 60 60)
      :cgi-ident (cgi-ident-of self)))
  (set!
    (as-storage-of self)
    (make
      <as-storage>
      :dbm-type (setting-of self :dbm-type)
      :storage-dir (setting-of self :storage-dir)
      ;; TODO: �����֥ǥ��쥯�ȥ귡��٤���
      ;; TODO: ¾�ˤɤ�ʰ�����ɬ�ס�
      ))
  #t)




;;; ----



;; note: ɬ�פʥѥ�᡼���ϰʲ����̤ꡣ
;; - ���������Ԥμ��̾���(ident)
;; - �׵ᤵ�줿���������о�(path)
;; - �оݤ��Ф��륳�ޥ�ɵڤӤ��ξܺ�(params)
;; �ʲ��ϡ����Ѥ��٤����¤��ѥ�᡼����
;; - ���Ū���������֤��ݻ�����٤Υ��å����Хåե�(session)
;;   (�������Ѥ���ȡ�����Ū�ʾ������ܤ������)
;; �����ϤޤȤ�ư�Ĥˤ��Ƥ�����
(define (make-req-info ident path params)
  (list ident path params))
(define (req-info->ident req-info)
  (car req-info))
(define (req-info->path req-info)
  (cadr req-info))
(define (req-info->params req-info)
  (caddr req-info))







(define (as:do-cmd self req-info c)
  ;; TODO: c�ǤΥǥ����ѥå��˼��Ԥ������ϡ�as:display-path�˰�ư�����롩
  #f)

(define (as:display-path self req-info)
  ;; TODO: as.storage�⥸�塼���Ȥ���
  ;;       �ǡ����ĥ꡼���顢
  ;;       ident���¤ˤ�äơ�
  ;;       path���б�����ǡ�������Ф�
  ;;       (���¤�̵������#f���������֤롩)
  ;; TODO: �֤��ͤȤ��ơ��ʲ��Τ褦�ʾ���ɬ�פˤʤ롣
  ;; - path���б����벿����¸�ߤ��ʤ��ä�
  ;;   (¸�ߤ��ʤ��ʤ顢���饤����Ȥ�404���֤�ɬ�פ�����)
  ;; - path���б����벿����¸�ߤ��뤬���������������ʤ��ä�
  ;;   (403���֤�ɬ�פ�����)
  ;; - path�ϼ¥ե����뤫�ݤ�
  ;;   (�¥ե�����ʤ顢httpd����ư�����)
  ;; - path�ϥǥ��쥯�ȥ�Ǥ��뤬��.content��¸�ߤ��ʤ��ä�
  ;;   (�������ɤ�������ư���ɤ���)
  ;; - path�ϥǥ��쥯�ȥ�Ǥ��ꡢ.content��¸�ߤ���
  ;;   (���˾ܺ٤ʥѥ������ʬ������)
  ;; - .content�μ��Τ�rfc822�����Υե�����Ǥ����ΤȤ���
  ;;   ���Υإå���ʬ��alist�ȡ�body��ʬ�����ä�����port���֤����٤���
  ;; �Ǹ�ޤ���ã�������ϡ��إå���ʬ�򸫤�ư��Ԥ��롣
  ;; ����Ū�ˤϡ����Τޤ�cgi���ϤȤ����֤��Ƥ�����Τʤ���礬¿����
  ;; - Location: �إå�������ʤ顢������쥯�Ȥ�Ԥ���
  ;; - Content-Type: �إå�������ʤ顢������б�����ư���Ԥ���
  ;;   â����application/as�������ꤵ��Ƥ������ϡ�
  ;;   ���Τޤ޽��ϤϤ����ˡ�����Υե��륿���̤��Ƥ����̤���Ϥ����롣
  ;;   (�ե��륿�ˤ�äƤϡ����Ϥ�����̵�뤷�ơ��ȼ��ν��Ϸ�̤��֤��Τ⥢��)
  (let1 content-obj (path->entity
                      (as-storage-of self)
                      (req-info->ident req-info)
                      (req-info->path req-info))
    #f))


(define (req:cgi-get-parameter self req-info key . opt-args)
  (apply
    cgi-get-parameter
    (string-append
      (setting-of self :param-name-prefix "")
      "_"
      key)
    (req-info->params req-info)
    opt-args))

(define (as-cgi-execute:main self req-info)
  (let1 c (req:cgi-get-parameter self req-info "c")
    ;; �ղå��ޥ�ɤ�̵ͭ�ˤ�äơ���ư���Ѳ�������
    (if c
      (as:do-cmd self req-info c)
      (as:display-path self req-info))))


(define-method as-cgi-execute ((self <as-cgi>))
  (cgi-main
    (lambda (orig-params)
      (with-cgi-session-ident
        (csa-of self)
        orig-params
        (lambda (true-params ident-info session-parameter)
          (with-path-info-keylist
            (lambda (path-info-keylist)
              (let1 req-info (make-req-info
                               ident-info
                               path-info-keylist
                               true-params)
                (as-cgi-execute:main self req-info)))))))
    :on-error cgi-on-error/stack-trace ; for debug
    )
  0)


;;; --------


(provide "tir04/tmp/20070919/as/cgi")

