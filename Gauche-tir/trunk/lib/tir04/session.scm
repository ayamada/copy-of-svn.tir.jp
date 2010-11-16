;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ����:
;;; ���Υ⥸�塼��ϡ����å�����ݻ���Ԥ��٤Υ��饹���������٤�
;;; ���Ȥߤ��󶡤��롣
;;; �ºݤˤ����������饹���ꤿ�����ϡ�
;;; ���Υ��饹��١������饹�˻��ꤹ����ˤʤ롣

;;; - ���å����ơ��֥�Υ���ȥ��key��sid�ȸƤ֡�
;;;   ����Ū�ˤϡ�sha1�������������ͤ�16��ʸ���󲽤��줿32ʸ����ʸ����ˤʤ롣
;;; - ���å����ơ��֥�Υ���ȥ��value�ˤϡ�����ξ�硢������
;;;   read/write invariance�����������֥������Ȥ���¸�����������롣
;;; -- �⥸�塼��ˤ�äƤϡ�����ʳ��Υ��֥������Ȥ���¸�Ǥ�����⤢�롣

;;; ���Υ⥸�塼�뤬�󶡤��륻�å����γ���:
;;; - ���å����Ȥϡ������ॢ���Ȥˤ��GC��ɤ����դ�����
;;;   (��³)�ơ��֥�Τ褦�ʤ�ΤȤ��롣
;;; -- �Ĥޤꡢsid�ˤ�ä�value����¸����GC����ʤ��¤�ϼ��Ф����ΤȤ��롣
;;; -- sid��create-session!�ˤ�äƤΤ������Ǥ��롣
;;;    �����sha1�����������Ȥ�16��ʸ���󲽤���32ʸ����ʸ����ˤʤ롣
;;;    �������ƥ���δ�����ꡢǤ�դ�ʸ�����key�˻Ȥ����ʤ�̵����ΤȤ��롣
;;; -- GC�˲�����줿sid/value�ϡ��ǽ餫��¸�ߤ��ʤ��ä���ΤȤ��ư����롣
;;; --- �פ���ˡ������ॢ���ȤʤΤ��ǽ餫��¸�ߤ��ʤ��ä��Τ���
;;;     ���̤�����ʤ�̵���Ȥ�������

;;; �ºݤ˥��å�����Ȥ����ξ����ˤĤ���:
;;; - ���å������̾����������Ԥ����桼����ǧ�ڤ�Ԥä���ˡ�
;;;   ����ǧ�ھ��֤��ݻ����Ƥ���Ȥ����ڤγ���Ȥ������Ѥ���롣
;;;   ��äơ�(������value���б�����)sid�����=ǧ�ڤ��Ԥ�줿���֡��Ȥʤ�١�
;;;   �����ʼ��ʤˤ�äƥ��å�����ȯ�ԤǤ��ʤ��褦�ˤ���ɬ�פ����롣
;;;   ����Ū�ˤϡ�������λ���ʳ��˿������å���������Ǥ��ƤϤ����ʤ���
;;; - �ޤ������Υ��å��������ʾ塢�����ѥ���������Ρ��ʲ����̤�ˤʤ롣
;;; -- sid������ʤ���anonymous�桼���Ȥ��ƽ�����Ԥ���
;;;    (ɬ�׺Ǿ��¤θ��¤���Ϳ���ʤ����ޤ��ϡ�������ʳ��θ��¤�����Ϳ���ʤ�)
;;; -- �������³����Ԥ���sid������������anonymous�桼����
;;; -- sid����äƤ��롣������ѥ桼���Ȥ��ƽ�����Ԥ���
;;;    (ɬ�פ˱����Ƹ��¤�Ϳ����)
;;; - ��äơ��礭��ʬ���ơ�(������value���б�����)sid�����ȡ�����ʳ��ξ���
;;;   �������礭���Ѥ��١��ʲ��Τ褦�������Ǵ�������Τ��ɤ��ȹͤ����롣

#|
(with-session
  session-obj
  sid ; ������sid�ޤ���#f
  (lambda (session-data) ; sid���������ޤ��ϡ����å����¸�ߤ��ʤ�����#f
    ;; �����˥��å������������
    ;; (session-data��#f�λ��Τ߼¹Բ�ǽ������ʳ��ξ��ϥ��顼�㳰���ꤲ��)
    (let1 new-sid (create-session! new-session-data)
      ;; ���å����˥ǡ�������¸��������sid��������
      ...)

    ;; ���å�������¸�����ǡ����򹹿�����
    (update-session! new-session-data)

    ;; ���å�����������(�������Ƚ������ǻȤ�)
    (remove-session!)

    ;; ��������proc����λ����ݡ�
    ;; ������update-session!���Ԥ��Ƥ��ʤ����Ǥ��äƤ⡢
    ;; ���å����ؤΥ���������ͭ���Ǥ�����򼨤��٤ˡ�
    ;; ��ưŪ�˥��å����Υ饤�ե����ब��������롣
    ...) ; ����proc���֤��ͤϡ�with-session���֤��ͤˤʤ�
|#

;;; ����¾�λ���:
;;; - ���å�����#f����¸���褦�Ȥ���ȡ����å����Ϻ������롣
;;; -- �����������å����¸�ߤ��ʤ�����with-session��proc�ˤ�
;;;    #f���Ϥ����Τǡ���ưŪ�ˤ������̵��Ȧ��
;;; - �����̤η����°����ʣ���Υ��å����ޥ͡������Ʊ�������Ѳ�ǽ������
;;;   (â�������ξ���method��Ƥֺݤˤ�����Ū��
;;;    ���å����ޥ͡��������ꤹ��ɬ�פ�����)
;;;   ��ĤΥ��å����ޥ͡�����˴ޤޤ��ʣ����sid�Ȥ��μ¥ǡ�����
;;;   Ʊ�������Ѥ�����ϤǤ��ʤ���ΤȤ��롣
;;; -- ʣ��Ʊ�������ѤǤ�����ϡ����ѤȺ��𤷤��⤿�餵�ʤ��褦�˻פ���١�
;;; -- �������������sid���ѹ����뵡ǽ(���å������Υǡ�����Ʊ��)�ϡ�
;;;    ���äƤ⤤�������Τ�ʤ���
;;; --- �Ȥϸ��������ˤ˻Ȥ���ǽ�ǤϤʤ��Τǡ����ϼ������ʤ����ˤ��롣


(define-module tir04.session
  (use gauche.parameter)
  (use rfc.sha1)
  (use util.digest)
  (use math.mt-random)

  (export
    <session>
    with-session
    create-session!
    update-session!
    remove-session!
    ))
(select-module tir04.session)

;; with-session���ΰ�����ά��ǽ�ѥѥ�᡼��
(define session (make-parameter #f))


;; �ҥ��饹�ϡ����Υ��饹��Ѿ��������

(define-class <session> ()
  (
   ;; session�μ�̿
   (expire-second
     :accessor expire-second-of
     :init-keyword :expire-second
     :init-value (* 1 24 60 60)) ; ���˰����Ȥ��Ƥ���
   ;; gc�ΰ٤Υ�����
   (gc-counter
     :accessor gc-counter-of
     :init-value 0)
   ;; ����ǡ��������륹��å�
   (now-sid
     :accessor now-sid-of
     :init-form (make-parameter #f))
   (now-status
     ;; with-session��ǡ����å����Υǡ��������줿�ݤ��ѹ������ե饰��
     ;; �ʲ����ͤ��롣
     ;; - 'not-modified
     ;; - 'created
     ;; - 'updated
     ;; - 'removed
     :accessor now-status-of
     :init-form (make-parameter #f))
   ))

(define-method initialize ((self <session>) initargs)
  (next-method)
  ;; ���Υ��饹���Τˤϡ��������ɬ�פʽ�����̵����
  ;; �ҥ��饹�ϡ��⤷ɬ�פʤ顢���Ӽ�ʬ���Ѱդ������
  #t)

;; �ҥ��饹�ϡ��ʲ���method���Ѱդ��ʤ��ƤϤʤ�ʤ���

(define-method inner:with-session-prehandler ((self <session>) sid)
  ;; ����method�ϡ�with-session�¹����˸ƤФ�롣
  ;; ����������ɬ�פʤ��ɲä��Ƥ褤��
  #f)

(define-method inner:with-session-posthandler ((self <session>) sid)
  ;; ����method�ϡ�with-session�¹Ը�˸ƤФ�롣
  ;; ����������ɬ�פʤ��ɲä��Ƥ褤��
  ;; ����dynamic-wind�ˤ�äơ����顼�㳰���ꤲ��줿�ݤˤ�ƤФ�롣
  ;; �ޤ���sid�ϡ�inner:with-session-prehandler���ƤФ줿����Ʊ���Ȥϸ¤�ʤ���
  ;; (create-session!��remove-session!���������������ǽ���������)
  #f)

(define-method inner:gc-session! ((self <session>))
  ;; ����method�ϡ�with-session�¹Ի��˻����¹Ԥ���롣
  ;; ����method���ƤФ줿�顢���å����ޥ͡����㤬�������Ƥ��륻�å�����
  ;; ���̤�����å����������ॢ���Ⱦ��֤ˤʤäƤ����Τ�������˴��������
  ;; �����������������å����ʤ��Ƥ⾡���gc�����褦�ʻ��Ȥߤ�
  ;; ���å�����������Ƥ�����ϡ����μ�³���ϲ���Ԥ�ʤ��Ƥ⹽��ʤ���
  #f)

(define-method inner:get-session ((self <session>) sid fallback)
  ;; ����method�ϡ����å����Υǡ������ȥ졼�����顢sid���б�����ǡ�����
  ;; ���Ф����֤�����
  ;; ��������sid��¸�ߤ��ʤ����䡢���Ф���褦�Ȥ���sid�Υ饤�ե������
  ;; ͭ�����¤��ڤ�Ƥ������ϡ��֤��ͤȤ���fallback�Ȥ����Ϥ��줿�ͤ��֤�����
  ;; ����method�Ǥϡ��ǡ������ȥ졼������ǡ�������Ф��ݤ�
  ;; �饤�ե�����ι����򤹤�ɬ�פ�̵����
  ;; ���������饤�ե�����ι�����Ʊ���˹ԤäƤϤ����ʤ����ǤϤʤ���
  (error "not implemented"))

(define-method inner:touch-session! ((self <session>) sid)
  ;; ����method�ϡ����ꤵ�줿sid�Υ饤�ե�����ι�����Ԥ�����
  ;; �饤�ե�����μ����ϡ�����ñ�˹������줿�����򲿤餫����ˡ��
  ;; �ݻ�����Τ��Ǥ��ñ���Ȼפ��롣
  ;; (ͭ�������ڤ줫�ɤ�����Ƚ�ꤹ��ݤˤϡ�(expire-second-of self)���Ȥ���)
  ;; ����method���ƤӽФ��줿�ʳ��ǥ饤�ե����ब�ڤ�Ƥ������ϡ�
  ;; �ޤ��饤�ե����बͭ���Ǥ��ä���ΤȤ��ơ�������Ԥ��Τ�˾�ޤ�����
  ;; �⤷�����줬̵���ʾ��ϡ�inner:get-session���ˤ�饤�ե�����ι�����
  ;; �Ԥ��褦�˼�������ɬ�פ����롣
  (error "not implemented"))

(define-method inner:put-session! ((self <session>) sid session-data)
  ;; ����method�ϡ����ꤵ�줿sid��session-data��key��value�Ȥ��ơ�
  ;; �饤�ե�����򹹿����Ĥġ��ǡ������ȥ졼���˵����������
  ;; ���ꤵ�줿sid���ޤ�¸�ߤ��ʤ����ϡ������ʥ���ȥ���������Ƶ����������
  (error "not implemented"))

(define-method inner:delete-session! ((self <session>) sid)
  ;; ����method�ϡ����ꤵ�줿sid���б����륨��ȥ�����������
  ;; ���ꤵ�줿����ȥ꤬¸�ߤ��ʤ����ϡ�����Ԥ�ʤ��Ƥ褤��
  (error "not implemented"))







;;; �ҥ��饹��ɽ���󶡤��٤�method��
;;; �����ϡ�inner:*�γ�method��ƤӽФ��١��Ҥ��Ѱդ���ɬ�פ�̵����
;;; â�����⥸�塼�뤬use���줿�顢������method��export�������

(define-method with-session ((self <session>) sid proc)
  (parameterize ((session self)
                 ((now-sid-of self) sid)
                 ((now-status-of self) 'not-modified))
    (dynamic-wind
      (lambda ()
        (inner:with-session-prehandler self ((now-sid-of self))))
      (lambda ()
        (let1 session-data (and sid (inner:get-session self sid #f))
          (receive result (proc session-data)
            (cond
              ((eq? ((now-status-of self)) 'not-modified)
               ;; ���å����¸�ߤ��Ƥ�����Τߥ饤�ե�����򹹿�
               (when session-data
                 (inner:touch-session! self sid)))
              ((eq? ((now-status-of self)) 'created)
               #f) ; ���ΤȤ���������Ԥ�ɬ������̵������
              ((eq? ((now-status-of self)) 'updated)
               #f) ; ���ΤȤ���������Ԥ�ɬ������̵������
              ((eq? ((now-status-of self)) 'removed)
               #f) ; ���ΤȤ���������Ԥ�ɬ������̵������
              (else
                (error "assertion")))
            (when (= 0 (mt-random-integer *mt* 2))
              (let1 gc-counter (+ (gc-counter-of self) 1)
                (if (< 16 gc-counter) ; ����32��˰�󤰤餤gc���ƤФ��
                  (begin
                    (inner:gc-session! self)
                    (set! (gc-counter-of self) 0))
                  (set! (gc-counter-of self) gc-counter))))
            (values result))))
      (lambda ()
        (inner:with-session-posthandler self ((now-sid-of self)))))))

(define-method create-session! ((self <session>) session-data)
  ;;(when ((now-sid-of self))
  ;;  (error "session is already exists"))
  ;; �����å�����̵ͭ�ϥ����å������ˡ���˿����Υ��å��������ΤȤ���
  ;; (��ͳ�ϡ����å��������ԤäƤ⡢����ϥ��å������ǡ����κ����
  ;;  �Ԥ������ʤΤǡ������Ȥ��Ƴ����Ǥ�sid���ݻ����Ƥ��ꡢ
  ;;  ���å������������å�����������å��������ȹԤä��ݤˡ�
  ;;  ����ܤΥ��å�����������ʬ��sid����˻��äƤ���٤ˡ�
  ;;  ���ä����äƤ��ޤ���)
  (let1 sid (make-sid self)
    (inner:put-session! self sid session-data)
    ((now-sid-of self) sid)
    ((now-status-of self) 'created)
    sid))
(define-method create-session! (session-data)
  (unless (session)
    (error "cannot found session-manager object"))
  (create-session! (session) session-data))

(define-method update-session! ((self <session>) session-data)
  (unless ((now-sid-of self))
    (error "have not sid"))
  (if session-data
    (begin
      (inner:put-session! self ((now-sid-of self)) session-data)
      ((now-status-of self) 'updated)
      #t)
    (remove-session! self))) ; #f����¸����褦�Ȥ����ʤ�������
(define-method update-session! (session-data)
  (unless (session)
    (error "cannot found session-manager object"))
  (update-session! (session) session-data))

(define-method remove-session! ((self <session>))
  (unless ((now-sid-of self))
    (error "have not sid"))
  (inner:delete-session! self ((now-sid-of self)))
  ((now-sid-of self) #f)
  ((now-status-of self) 'removed)
  #t)
(define-method remove-session! ()
  (unless (session)
    (error "cannot found session-manager object"))
  (remove-session! (session)))






;; sid�����Ѽ�³����
(define *mt*
  (make <mersenne-twister>
        :seed (receive (epoch micro) (sys-gettimeofday)
                (receive (q r) (quotient&remainder epoch (+ (sys-getpid) 1))
                  (+ q r micro)))))

(define-method make-sid ((self <session>))
  (let1 new-sid (digest-hexify
                  (sha1-digest-string
                    (x->string
                      (mt-random-real0 *mt*))))
    ;; ���ߤΥ��å����ơ��֥�˴��ˤ���sid��̵�������ǧ����ɬ�פ�����
    ;; ��å����Ƥ������ǤϤʤ��Τǡ�race condition��ȯ�������ǽ���Ϥ��뤬��
    ;; ���⤽��sha1 digest���äƤ���Τǡ����β�ǽ���Ͻ�ʬ�㤤�Τǡ�����롣
    ;; �⤷��inner:get-session�Υ����Ȥ��빽�礭���褦�ʤ顢
    ;; ���γ�ǧ��Ȥ��ά���ƹ���ʤ������Τ�ʤ���
    ;(if (inner:get-session self new-sid #f)
    ;  (make-sid self)
    ;  new-sid)
    ;; �Ȥꤢ������ά������ˤ���
    new-sid))





;;; --------


(provide "tir04/session")
