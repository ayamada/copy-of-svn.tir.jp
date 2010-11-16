;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; data storager of account scheduler

;;; TODO: ����ϥץ���ʬΥ���ơ�socket�ˤ�ä�
;;;       �ǡ����������ϤǤ���褦�ˤ���ɬ�פ�����
;;;       (�������Ϥ��ʤ�)

;;; TODO: �ե�����Υ���å��󥰤�ɬ�ס��ʲ��λ��ͤȤ��롣
;;; - �ե���������Ƥ򡢥ե�����Υ����ॹ�����(stat�Ǽ����Ǥ�����)��
;;;   ��Ϣ�դ��Ƶ������롣
;;; - â�������̤˾嵭�Τ褦�ˤ��Ƥ��ޤ��ȡ����ð����ʣ���󹹿����줿�ݤ�
;;;   �ºݤΥե�����ȥ���å������Ƥ˰㤤���ФƤ��ޤ���ǽ��������١�
;;;   �ե�����˽񤭹���ݤˡ����Υե�����Υ����ॹ����פ����˸��ߤ�
;;;   epoch��Ʊ����������̤����ä����ϡ������塢touch��Ȥäơ�
;;;   ���Υե�����Υ����ॹ����פ���1�ä����ʤ᤿�ÿ��Ǥι������ä��褦��
;;;   �������������
;;; - �Ť��ե������GC�Ϥ��ȤǼ��������������GC̵�����ɤ���
;;;   (�����������Ȥ�GC�ɲä��䤹���褦�˼��������)

;;; ����:
;;; - :storage-dir��ˡ�����񼰤Υե����뷲����ᡢ������������⥸�塼��
;;; - ����Ū�ˡ��ǥ��쥯�ȥ�Τߤǹ�������롣
;;; -- �ǥ��쥯�ȥ�̾�����Ѳ�ǽ��ʸ���ϡ�A-Za-z0-9_�ΤߤȤ���
;;; - �ǥ��쥯�ȥ���ˤϡ��ʲ��Υɥåȥե����뤬���֤�����ǽ�������롣
;;; -- .content������path�Υǡ������Τ�rfc822��������¸����롣
;;; -- .metadata������path�ξܺ������͵ڤӥ��롼�׾��󡣽�̤�ꡣ
;;;    ����ͤ�.config��.group��.priv�����礷����Ρ�
;;;    �����ǥ��쥯�ȥ곬�ؤ��������ϡ��Ƶ�Ū�˸���/Ŭ�Ѥ���롣
;;;    �ޤ����ʲ��ξ������Ǥ���Ĥ�ΤȤ��롣
;;; --- owner�������ǥ��쥯�ȥ�ν�ͭ�ԡ�
;;;     �ä˻��ꤵ��Ƥ��ʤ����ϡ���̥ǥ��쥯�ȥ�ν�ͭ�Ԥ��Ѿ�����롣
;;; --- group��
;;;     unix��group��Ʊ����
;;; --- permission��
;;;     unix��chmod��Ʊ����owner, group, other���Ф��ơ�
;;;     acl�����ꤹ�롣
;;;     �����ǽ�ʹ��ܤϡ�r, w, a, t, S�Ȥ��롣x�Ϻ��ΤȤ���̵����
;;;     (���̥ǥ��쥯�ȥ�ؤλ��Ȥβ��ԲĤˤ�r��Ȥ�)
;;;     a��append��w���ɵ��Τ��ǡ�
;;;     t��sticky bit��w�θ��¤��⡢��ʬ���Ȥ����Ҥ�����ΤΤ��Խ���ǽ�Ȥ��롣
;;;     (owner��sticky bit�αƶ����������������Խ���ǽ�Ȥ��롣
;;;      �ޤ���t����header���Խ���owner�ΤߤȤ��롩����)
;;; ---- content��header��body�ǡ�acl��ʬ����٤���
;;;      body�Ϥ����餻��������header�Ϥ����餻�����ʤ����Ȥ�����礬���ꤽ����
;;; -- .metadata�ϡ������Ǥϰʲ��Τ褦��list�Ȥ����ݻ����롣
;;;    '(; ����̤��
;;;      )
;;; -- .content�ڤ�.metadata�Υե�����񼰤˰۾郎������ϡ�
;;;    �ɤ���ä����Τ��롩����
;;; --- �Ȥꤢ�������ˡ�stderr�˥��顼���Ƥ�ή����
;;;     �֤������ϲ�ǽ�ʸ¤ꤽ��äݤ���¤����
;;; - ����Ū�ˤϡ�path��node���Ф��ơ��Ƽ�����ޥ�ɤ�¹Ԥ�����ˤʤ롣
;;;   �ʲ��Υ��ޥ�ɤ��Ѱդ���Ƥ���в��Ȥ��ʤ�Ȼפ��롣
;;; -- ��������
;;; -- ��������
;;; -- ��������
;;; -- ��������
;;; - content�����Ƥϡ�Content-Type�ˤ�äƤ���������ɬ�פ����뤬��
;;;   ����Ϥ��Υ⥸�塼��ǤϤʤ����̤Υ⥸�塼�뤬���ݤ򸫤��ΤȤ��롣
;;; - path��node���Ф������ϡ����¤ˤ�äƵ�ư��ػߤ��٤���礬���뤬��
;;;   ���ζ�ʬ��ɤ��ǻ��Ĥ٤�����
;;; -- �ǽ�Ū�ˤϡ��ץ�����ʬΥ����socket�ˤ�ä�������ˤʤ�١�
;;;    ���λ����Ǵ��˸��´������Ƥ褤�Ȼפ��롣
;;;    �Ĥޤꡢ�Ƽ�����ޥ�ɤϡ������Ȥ���
;;;    ��ï�Υ�������Ȥθ��¤Ǽ¹Ԥ��뤫�פ�ɬ�פȤ����ΤȤ��롣


;;; TODO: ���ΤȤ������¤�read, write, append�λ��ĤΤߤ�����
;;;       ����Ϥ��ʣ���ʸ��¤��ɲä����ǽ�����⤤�Τǡ�
;;;       �ɲò�ǽ�ʹ�¤�ˤ��Ƥ�����


(define-module tir04.tmp.20070619.as.storage
  (use srfi-1)
  (use rfc.822)
  (use file.util)
  (use dbm)
  (use tir04.dbm.util)

  (export
    <as-storage>
    as-storage:open
    as-storage:close
    as-storage:fetch
    as-storage:store
    ))
(select-module tir04.tmp.20070619.as.storage)


(define-class <as-storage> ()
  (
   (storage-dir
     :accessor storage-dir-of
     :init-keyword :storage-dir
     :init-value #f)
   (dbm-type
     :accessor dbm-type-of
     :init-keyword :dbm-type
     :init-value #f)

   ;; internal slots
   (status
     :accessor status-of
     :init-value 'made)
   ))

(define-method initialize ((self <as-storage>) initargs)
  (next-method)
  (unless (file-exists? (storage-dir-of self))
    (make-directory* (storage-dir-of self)))
  (unless (dbm-type-of self)
    (errorf "~s must be need to :dbm-type" self))
  #t)



;;; ----


;; TODO: ��å����٤����ͤ������٤��ʤ���������
(define-method as-storage:open ((self <as-storage>) . opt-args)
  (when (eq? (status-of self) 'booted)
    (error "already booted" self))
  ;; TODO: ���Ȥǿ������ɲä���
  (set! (status-of self) 'booted)
  #t)

(define-method as-storage:close ((self <as-storage>) . opt-args)
  (unless (eq? (status-of self) 'booted)
    (error "not booted" self))
  ;; TODO: ���Ȥǿ������ɲä���
  (set! (status-of self) 'shutdowned)
  #t)


;; �����餯��HTTP�˶ᤤ¸�ߤˤʤ�ΤǤϤʤ����Ȼפ���Τǡ�
;; ��������HTTP�򻲹ͤˤ��ơ������ϥ��󥿡��ե�������ͤ��롣
;; - .metadata����������list���֤�ɬ�פ�����
;; - .content�β������֤�ɬ�פ�����
;; -- .content��path���Τ��
;; -- .content�����Τ����ä�����port
;; -- .content�Υإå���ʬ+�Ĥ꤬���ä�����port

;; TODO: ����method���֤����֤��ͤη��Υѥ�����ϡ�
;; - ������ϡ���褵�줿�ǡ�����¿�ͤ��֤�
;; - path�����椫��(�ޤ��Ϻǽ餫��)¸�ߤ��ʤ��ʤä����ϡ�����
;; - .metadata�ǻ��ꤵ��륰�롼��°���ˤ�äơ��������������ʤ��ä�
;; - ����¾�Υ��顼���ϡ�����
;; ����method�ϡ����餫������򡢥��顼�㳰�ˤ�ä����Τ���١�
;; Ŭ�ڤ�guard���Ƥ�������
;; �֤��ͤȤ��ơ��ʲ����ͤ�¿�ͤ��֤���롣
;; - �ޡ������줿.metadata����������list
;; - .content�Υإå�
;; - .content��body�����ä�����port
(define-method as-storage:fetch ((self <as-storage>) perm paths . opt-args)
  ;; TODO: ����å��󥰤λ��Ȥߤ�ɬ��
  ;; �Ȥꤢ���������դ����äƤ�����
  ;; �ޤ������ΤȤ���perm�ϸ����ˡ�paths�����򻲾Ȥ��Ƥɤ��ˤ�����褦��
  ;; �������Ƥߤ�
  ;; �ޤ���ʣ����/�ϰ�ĤˤޤȤ��(fs����)
  (let1 true-paths (delete "" paths equal?)
    ;; ���ˡ�true-paths��������ʸ�����ޤޤ�Ƥ��ʤ������ǧ����
    ;; �ġĤȻפä�������äѤꤳ��ϺƵ�Ū�˰�Ĥ��ĸ��ڤ��Ƥ������ˤ���
    (let next ((rest true-paths)
               ;; ���߰��֤�ʸ�����
               (current-path (storage-dir-of self))
               (merged-metadata '())
               )
      ;; �ޤ������ߤΥǥ��쥯�ȥ��.metadata��Ĵ�١��ޡ�������
      ;; TODO: ������ʬ��.metadata�Υ���å��󥰤�ɬ�פˤʤ�
      ;;       (���ˤ褯�������������)
      (let1 new-merged-metadata '() ; TODO: ����̤�ꡣ���Ȥ�
        ;; TODO: ���¤�̵�������֤��ͤϤɤ���ä��֤���
        ;; TODO: .content�ϥ���å��󥰤�ɬ��/��ǽ�����׸�Ƥ��
        (if (null? rest)
          (let1 content-path (string-append current-path "/.content")
            ;; TODO: ���˽�����³�������Ȥ�
            ...)
          (begin
            ;; �ޤ��������٤�path���ĤäƤ��롣
            ;; TODO: .metadata�ʳ��ˡ������ǤؤηѾ���ɬ�פʤ�Τ�����ʤ顢
            ;;       �����ǹԤ���
            ;; TODO: (car rest)�������Ǥʤ���������å�����ɬ�פ�����
            (next (cdr rest)
                  (string-append current-path "/" (car rest))
                  new-merged-metadata)))))))

(define-method as-storage:store ((self <as-storage>) perm paths . opt-args)
  ;; TODO: ����å��󥰤λ��Ȥߤ�ɬ��
  #f)


;;; ----


(provide "tir04/tmp/20070619/as/storage")

