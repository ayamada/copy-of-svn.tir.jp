;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; cgi�ѤΥ桼���μ��̤�Ԥ��٤Υ��饹�Υ١������饹

;;; ����:
;;; ���Υ⥸�塼��ϡ�cgi�ǥ桼���μ��̤�Ԥ��٤Υ��饹���������٤�
;;; ���Ȥߤ��󶡤��롣
;;; �ºݤˤ����������饹���ꤿ�����ϡ�
;;; ���Υ��饹��١������饹�˻��ꤹ����ˤʤ롣
;;;
;;; ���Υ��饹��Ѿ������ƥ��饹�ϡ��桼���μ��̵�ǽ�Τߤ��󶡤���١����ӡ�
;;; ���å���󥯥饹���Ѱդ���ɬ�פ�����(tir04.cgi.session)��
;;; �ޤ��ϡ����å����Ȥ��̤ˡ�
;;; ���餫�ν��פ������Ѥ���Ĺ�ư(�ǡ����κ����ʪ�ʤι�����)��
;;; �桼�����Ԥ��ݤ�ñ�γ�ǧ���ӤȤ��Ƥ����Ѳ�ǽ��

;;; NB: ident-info��������ݤ������Ѥ�����������١�
;;;     with-cgi-ident���Ϥ���proc��ident-info�������ä������ߥ󥰤Ǥϡ�
;;;     ���Τޤ�html����Ϥ����ˡ���ö������쥯�Ȥ�Ԥ��褦�ˤ��������ɤ���
;;;     (�������ʤ��ȡ��֥饦����back������Ǻƥ������������ä���������ˤʤ�)

;;; TODO: �嵭�������ʬ����ˤ����Τǡ����С������Ǥϡ�
;;;       ��ident-info�������Ǥ�������proc�פ����ӻ��ꤹ��褦�ˤ���
;;;       ����proc�ϥ�����쥯�Ȥ��٤�url���֤����ͤȤ��롢
;;;       �ߤ����ʴ����λ��ͤˤ��롣
;;;       (â�������λ��ͤ��Τ�ΤǤϤʤ����⤦�������ͤ�����ɬ�פ����롣)

#|
(define *cgi-ident*
  (make <cgi-ident-hoge>
        :internal-key-prefix "cgi-ident"
        :description-html ...
        :description-html-keywords '(...)
        :error-html-proc ...
        :error-html-keywords '(...)
        ...))

(cgi-main
  (lambda (orig-params)
    (with-cgi-ident
      *cgi-ident*
      orig-params ; �����ͤˤ�äơ�with-cgi-ident�������򲣼�ꤹ�뤫����ޤ�
      (lambda (true-params ident-info)
        ;; true-params�ˤϡ�:internal-key-prefix�ǻϤޤ�key������줿
        ;; params�����롣
        ;; ǧ�ڤ��Ԥ�졢���줬�����������Τߡ�ident-info���ͤ����롣
        ;; ident-info��#f�ʳ����ä���硢�桼����������ܥ���򲡤���
        ;; with-cgi-ident������ꤷ���������ǧ�ڽ������Ԥ�졢������ǧ�ڤ��줿
        ;; �Ȥ������ʤΤǡ�ident-info���������params���Υ����å��������
        ;; �������ν����ؤȥǥ����ѥå����ʤ��ƤϤʤ�ʤ���
        ;; �ޤ�������ǧ�ڤξ�硢ident-info�μ����������Ѥ���Ĳ�ǽ��������١�
        ;; ident-info���������줿���ľ�ܥ���ƥ�Ĥ�ɽ������ΤǤϤʤ���
        ;; ��ö������쥯�Ȥ������˾�ޤ���(�֥饦����history back�к��Ȥ���)��
        ;; �����⤷ǧ�ڤ˼��Ԥ������ϡ�with-cgi-ident������ꤷ���������
        ;; ���顼��ɽ�������Ȧ�ʤΤǡ������Ǥϵ��ˤ��ʤ����ɤ���
        ;; ident-info�ϡ��ʲ��Τ褦��keywords�Ǥ��롣
        ;; '(
        ;;   :ident-type ???
        ;;   ;; 'form��'hatena��'typekey����ǧ�ڥ����פ����ꤹ�륷��ܥ롣
        ;;   ;; ����Ū�ˤϡ����饹̾�����Τޤ޻Ȥ��롣
        ;;   :ident-path "..."
        ;;   ;; dbm��path����:ident-type������ʣ������������ľ��ˡ�
        ;;   ;; ���ξܺ٤����ꤵ��롣
        ;;   ;; ����ǧ������ʣ��������褬¸�ߤ��ʤ����϶�ʸ����
        ;;   :ident-id #*"..."
        ;;   ;; :ident-id�ϡ�ǧ�ڤη�������롢unique��idʸ����
        ;;   ;; ǧ�ڥ����פˤ�äƤ��Դ���ʸ�����������ǽ��������١�
        ;;   ;; �����ˤ���դ�ɬ�ס�
        ;;   :uid "..."
        ;;   ;; :ident-type��:ident-path��:ident-id��ʸ����Ȥ��Ʒ�礷��
        ;;   ;; ����sha1 digest���ä���̤�hexify����ʸ����
        ;;   ;; ʣ����ǧ�ڤ�ޤ����Ǥ�unique��ʸ����������١�
        ;;   ;; ʣ����ǧ�ڥ����פ���Ĥ�����ˤ�unique���ͤ������롣
        ;;   ;; �̾�ϡ������ͤ�key�Ȥ������Ѥ��٤���
        ;;   ;; (ident-id�򤽤Τޤ�key�Ȥ������Ѥ���ΤϿ��������꤬����)
        ;;   ;; ����unique�ʻ��ˤĤ��Ƥϡ�sha1�ξ��ͥ�٥�ǰ������ݾڤ����
        ;;   ;; �����ΤΡ�digest�μ�����ˤĤ��Ƥ���̩���Τ褦�ʤ�Τ�
        ;;   ;; �ä˻ȤäƤ��ʤ��١�����򤽤Τޤޥ桼���˸�����Τ�
        ;;   ;; ���Ƥ����������ɤ�(brute force�Ǹ���ident-id����Ƚ���Ǥ���)��
        ;;   :x-ident-info '(...)
        ;;   ;; ����¾�Ρ�ǧ�ڥ�������˸��̤������롢�Ƽ�ξ���
        ;;   ;; rfc.822������alist���󶡤���롣
        ;;   ;; key��val��(�Դ��������Τ�ʤ�)ʸ����
        ;;   )
        ;; ǧ�ڥڡ����ؤ�url��������
        (make-ident-url *cgi-ident* misc-params)
        ;; ǧ�ڥܥ�������
        (make-ident-form *cgi-ident* misc-params html-tree . keywords)
        ... ; �͡��ʽ�����Ԥ�
        ))))
|#


(define-module tir04.cgi.ident
  (use srfi-1)
  (use srfi-13)
  (use text.html-lite)
  (use www.cgi)
  (use tir04.cgi.util)

  (export
    <cgi-ident>
    with-cgi-ident
    make-ident-url
    make-ident-form
    ))
(select-module tir04.cgi.ident)


(define (default-error-html-proc error-message)
  (html:p
    (text->inline-html error-message)))
(define *default-error-html-keywords*
  `(:encoding ,(x->string (gauche-character-encoding))
    :http-header '(:pragma "no-cache" :cache-control "private")
    :css-url "http://css.tir.jp/tir.css"
    :robots "NOINDEX,NOFOLLOW"
    :title "error"
    ))

(define-class <cgi-ident> ()
  (
   (internal-key-prefix
     ;; with-cgi-ident�ǽ����򲣼�ꤹ��٤�cgi�ѥ�᡼����keyʸ�����prefix��
     ;; �ºݤν����ǻȤ�key�����ʤ��褦�ˤ������
     :accessor internal-key-prefix-of
     :init-keyword :internal-key-prefix
     :init-value "tir04-cgi-ident")
   (description-html
     ;; ����ǧ�ڤ�����ʸ��
     ;; â����ǧ�ڥ����פˤ�äƤ������Ȥ��ʤ������Τ�ʤ���
     :accessor description-html-of
     :init-keyword :description-html
     :init-value #f)
   (description-html-keywords
     ;; ���Ҥ�����ʸ��ɽ������ݤ˻Ȥ��륭����ɷ���
     ;; ������ɤˤĤ��Ƥϡ�html-tree-make��Ʊ����
     ;; description-html��Ʊ�ͤˡ�ǧ�ڥ����פˤ�äƤ������Ȥ��ʤ���
     :accessor description-html-keywords-of
     :init-keyword :description-html-keywords
     :init-value #f)
   (error-html-proc
     ;; ���顼����html�Ҥ���������proc����ꤹ�롣
     ;; �����Ȥ��ƥ��顼���Ƥ�����ʸ��plain text�Ȥ����Ϥ���롣
     ;; (ɬ�פʤ饨�������פ�������Ԥ�ɬ�פ�����������)
     :accessor error-html-proc-of
     :init-keyword :error-html-proc
     :init-value default-error-html-proc)
   (error-html-keywords
     ;; description-html-keywords-of�Υ��顼�ǡ�
     ;; cgi-tree-make���Ϥ���롣
     :accessor error-html-keywords-of
     :init-keyword :error-html-keywords
     :init-value *default-error-html-keywords*)
   ))


(define-method initialize ((self <cgi-ident>) initargs)
  (next-method)
  )


(define (internal-key self suffix)
  (string-append (internal-key-prefix-of self) "-" suffix))

(define (dispatch-key-of self)
  (internal-key self "cmd"))

;; �����ؤ��󶡤���methods
;; �����ϻҥ��饹�Ͼ�񤭤���ɬ�פ�̵����
(define-method with-cgi-ident ((self <cgi-ident>) cgi-params proc)
  ;; �ޤ���cgi-params��(internal-key-prefix-of self)���ޤޤ�Ƥ��뤫��ǧ����
  (let ((dispatch-val (cgi-get-parameter (dispatch-key-of self)
                                         cgi-params))
        (true-params (remove
                       (lambda (key+vals)
                         (string-prefix? (internal-key-prefix-of self)
                                         (car key+vals)))
                       cgi-params)))
    (if (not dispatch-val)
      (proc true-params #f)
      (receive r (let/cc ident-info-cont
                   (values
                     #f
                     (dispatch-or-ident-info self
                                            dispatch-val
                                            cgi-params
                                            true-params
                                            ident-info-cont)))
        ;; ident-info�μ������������Ƥ����顢
        ;; (get-remove-params-key-list self)��key�򹹤˾ä��Ƥ���proc���Ϥ�
        (if (car r)
          (proc
            (fold
              (lambda (keyname prev)
                (alist-delete keyname prev equal?))
              true-params
              (get-remove-params-key-list self))
            (car r))
          (cadr r)))))) ; ident-info�μ����Ϥޤ��ʤΤǡ��������줿html���֤�

(define-method make-ident-url ((self <cgi-ident>) misc-params)
  (append-params-to-url
    (self-url/path-info)
    (list*
      `(,(dispatch-key-of self) "login")
      misc-params)))

(define-method make-ident-form ((self <cgi-ident>)
                               misc-params
                               html-tree . keywords)
  (apply
    make-form
    (self-url/path-info)
    (list*
      `(,(dispatch-key-of self) "login")
      misc-params)
    (or html-tree (html:input
                    :type "submit"
                    :value "login"))
    keywords))


;; �ҥ��饹���������٤�methods
(define-method dispatch-or-ident-info ((self <cgi-ident>) dispatch-val
                                                        cgi-params
                                                        fixed-params
                                                        ident-info-cont)
  ;; ���μ�³���ϡ�dispatch-val�ˤ�ä��͡��ʵ�ư��Ԥ���
  ;; �֤��ͤȤ���cgi-main���Ϥ�������html-tree���֤���
  ;; â������Ū�Ǥ���ident-info�������������������ϡ�
  ;; html�Ҥ��֤����ˡ�(ident-info-cont ident-info)��ƤӽФ������Τޤ�
  ;; ��³��é�äƽ�λ����(��³�ʤΤǡ��֤��ͤ��֤�ɬ�פ�̵��)��
  ;;
  ;; cgi-params�ˤϡ�make-ident-url��make-ident-form���˻��ꤷ��
  ;; misc-params���ɲä��Ϥ���롣
  ;; �����ˤϡ�dispatch-val�ϴ��˺������Ƥ���١�����
  ;; misc-params���ϤäƤ��Ƥ����ΤȤ��ư��ä��ɤ���
  ;; (â�����桼���������˥ѥ�᡼������¤���Ƥ����ǽ���Ͼ�ˤ���)
  ;; �����ϥ�����Хå������Ϥ��������Ȥ�����¸�������˾�ޤ�������
  ;; ¾�����ӤǻȤäƤ��ɤ���������ñ�˼ΤƤƤ��ɤ���
  ;;
  ;; dispatch-val�ϡ��ǽ�˥�����url/������ܥ��󤬲����줿�ʳ��Ǥϡ�
  ;; "login"�Ȥʤ�褦�ˤʤäƤ��롣
  ;; ����ʳ��Υǥ����ѥå��ΰ٤�ʸ�����Ǥ�դ˷��ƹ���ʤ���
  ;; �����ǥ����ѥå�Ƚ��˼��Ԥ������ϡ�"login"��Ʊ�ͤν�����Ԥ��褦��
  ;; ���Ƥ����Τ�˾�ޤ�����
  ;;
  ;; �������줿html�ڡ�����href��form��hidden�ѥ�᡼���ˡ�
  ;; (dispatch-key-of self)��key���ޤޤ�Ƥ���¤�ϡ�����
  ;; dispatch-or-ident-info��³�����ƤӽФ����Τǡ�
  ;; �������줿html�ڡ�����html��form��hidden�ˤ�ɬ��
  ;; (dispatch-key-of self)��key��ޤ��褦�ˤ������˺��ʤ��褦�ˡ�
  (error "not implemented"))

(define-method get-remove-params-key-list ((self <cgi-ident>))
  ;; ���μ�³���ϡ���ǧ�ڥ⥸�塼�����������Ѥ���ɬ�פΤ��롢
  ;; �ɲåѥ�᡼��̾��list���֤���
  ;;
  ;; �㤨�С�����ǧ�����ǡ�������̾��ѥ���ɾ����
  ;; cgi-params�˴ޤ�Ǥ����硢�����Υѥ�᡼���Ϻ�����Ƥ���������
  ;; �̾(internal-key-prefix-of self)�ǻϤޤ�key�ϰ��ۤ���˺������뤬��
  ;; ����ǧ�ڤξ�硢����ǧ�ڤΥ����ƥ�Ǹ����key��Ϳ�������礬���롣
  ;; �����������ˡ����Υ�����̾��ѥ���ɾ����key̾��
  ;; ����list�Ȥ����ɲä�����ɤ���
  ;; ��������ȡ�cgi-params�ˡ�(dispatch-key-of self)���ޤޤ졢
  ;; ǧ�ڤ���λ�����ʳ��ǤΤߡ�����list�˴ޤޤ�Ƥ���ѥ�᡼����
  ;; ������줿���֤�true-params�Ȥ����󶡤���롣
  ;; (�����ʳ��ʳ��ǤϺ������ʤ�������ա�)
  ;;
  ;; �ä˲���̵������'()���֤����ɤ���
  '())


;; �ҥ��饹�ΰ٤Ρ��桼�ƥ���ƥ���³��
(define-method get-error-html ((self <cgi-ident>) error-message)
  (apply
    cgi-tree-make
    :body ((error-html-proc-of self) error-message)
    (error-html-keywords-of self)))




(provide "tir04/cgi/ident")
