;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ���Υ⥸�塼��ϡ�����ʸ�ʤɤ�Ĺ���ƥ����ȵڤ�html�Ҥ�������롣

(define-module sb-text
  (use text.html-lite)
  (use text.tree)
  (use srfi-1)
  (use util.list)

  (export-all))
(select-module sb-text)


;;; ----


(define *text:story*
  "
  ������S���������ࡣ
  �겼��S����Ʈ�Τ򥳥������������ߡ�
  ������������S�������Ȥ��μ겼���ݤ��ޤ��礦��
  ")


(define *text:brief*
  "
  ��Ʈ���르�ꥺ���ʬ�ǽ񤤤ơ�
  �����Ũ����碌�륲����Ǥ���
  ��Ʈ���르�ꥺ���Scheme�ǵ��Ҥ���ɬ�פ�����ޤ���
  ��Ʈ�ϥ���󥱥�١����Ǥ���
  ������Ʈ�ϥꥢ�륿����˿ʹԤ��Ƥ����ޤ���
  ���Ƚ����Ǥ���֤��ʹԤ��Ƥ����Τǡ�
  ���르�ꥺ�बʣ�����ȡ�
  �����٤������ˤʤ�ޤ���
  �ޤ�����Ʈ�ˤϥ��������Ǥϴޤޤ�ʤ��Τǡ�
  Ʊ�������ǲ������路�Ƥ⡢���Ʊ����̤ˤʤ�ޤ���

  ��ͥץ쥤�Ǥϡ��������Ѱդ��줿���޿ͤ����襤�ޤ���
  �ե꡼����Ǥϡ����Ϥ�����Ĥ�S������碌�ޤ���
  ")


(define *html:rule*
  (html:ul
    (html:li "�������ͤ������ˤ�äơ�"
             "�ɤ����ɾ������˳��Ϥ���뤫�����ꤷ�ޤ���"
             (html:br)
             "���θ塢����å�Ū��ξ�Ԥ�ɾ�����ʤߤޤ���"
             )
    (html:li "���HP��100�Ǥ���")
    (html:li
      "�ʲ��λ��Ĥ����Τɤ줫������ǡ����򹶷⤷�ޤ���"
      (html:ul
        (html:li "G���å���"
                 (html:br)
                 "���פ��礭������Ǥ���"
                 (html:br)
                 "�˲��Ϥ����⤤�Ǥ���"
                 (html:br)
                 "C�֥졼�ɤι����Ϥ����ĤĹ���Ǥ��ޤ�����"
                 "P������ɤˤϼ���ή����Ƥ��ޤ��ޤ���"
                 (html:br)
                 "G���å���Ʊ�ΤǤ����̤˥��᡼����Ϳ���ޤ���"
                 )
        (html:li "C�֥졼��"
                 (html:br)
                 "�Ԥ��Ϥ��ؾ���������ġ�����˾����ʷ�ζ�������"
                 "�����ο�ʪ�Ǥ���"
                 (html:br)
                 "�����������ȤǻȤ��ޤ���"
                 (html:br)
                 "����®�٤����Ū���Ǥ���"
                 (html:br)
                 "G���å����ˤϼ���ή����Ƥ��ޤ��ޤ�����"
                 "P������ɤˤϽ�β�����Ϥ�����ƹ���Ǥ��ޤ���"
                 (html:br)
                 "C�֥졼��Ʊ�ΤǤ����̤˥��᡼����Ϳ���ޤ���"
                 ;"C�֥졼��Ʊ�Τ��׶���礤�ˤʤäƥ��᡼����Ϳ�����ޤ���"
                 )
        (html:li "P�������"
                 (html:br)
                 "����ή�����ò��������礭���������ν�Ǥ���"
                 (html:br)
                 "G���å�����;͵�Ǽ���ή���ޤ�����"
                 "C�֥졼�ɤ���򤹤���ϤǤ��ޤ���"
                 (html:br)
                 "P������ɤ�¾�����Ȱ㤤��������Τ˻��֤�������ޤ������������ޤ޹ͤ����������ޤ���"
                 (html:br)
                 "P������ɤ��������硢���˥��᡼����Ϳ�������"
                 "�Ǥ��ޤ��󤬡�G���å����ι�������ή������硢"
                 "���˷�(wait)���Ǥ���Τǡ�"
                 "��������¾�����ǹ��⤷�ޤ��礦��"
                 )
        ))
    (html:li "�㤦���򹽤�����ˤϡ������䤬����ޤ���Ʊ������Ϣ³���⤹����ˤϷ�Ϥ���ޤ���"
             (html:br)
             "���η�ϥ�������Ǥ�ñ�ʤ�wait�Ȥ���ɽ������Ƥ��ޤ���")
    (html:li "���Ҥι����ѹ��η�Ȥ��̤ˡ���ﹶ������򤷤Ƥ���ºݤ˹��⤹��ޤǤ˰�����֤�����ޤ��������waitɽ���Ǥ���")
    (html:li "���ˡ��ºݤι���夫�鼡�˹�ư��ǽ�ˤʤ�ޤǤˤ������֤�����ޤ��������waitɽ���Ǥ���")
    (html:li "battle-main��³�������顼�㳰���֤��ȡ��ڥʥ�ƥ��Ȥ���̵�������֤ˤʤꡢ���Ф餯��ľ������ˡ�battle-main��³�������ٸƤФ�ޤ���")
    (html:li "���ꥹ�ƥå׷в��˾��餬�դ��Ƥ��ʤ���硢ξ�ԤλĤ�HP�Ǿ����Ƚ�ꤷ�ޤ��������Ʊ�����ä����ϸ并�ξ����Ȥ��ޤ���")
    ))


(define *html:explain*
  (html:ul
    (html:li (html:code "(action-g)")
             (html:br)
             "G���å����򹽤���Ũ�򲥤�ޤ���"
             "P������ɤ򲥤�ȼ���ή���졢�������̵�����ˤʤ�ޤ���"
             "��פ���100���ƥå��񤵤�ޤ���")
    (html:li (html:code "(action-c)")
             (html:br)
             "C�֥졼�ɤ򹽤���Ũ��ɤ��ޤ���"
             "G���å����򹽤������ˤ����Ѥ��ޤ���"
             "��פ���70���ƥå��񤵤�ޤ���")
    (html:li (html:code "(action-p)")
             (html:br)
             "P������ɤ򹽤��ޤ�������Ϥ��ޤ���"
             "C�֥졼�ɤι�����ɤ��ޤ���"
             "������ޤǤ���30���ƥå��񤵤�ޤ���")
    (html:li (html:code "(action-off . opt-wait-step)")
             (html:br)
             "��ʬ����̵�������֤ˤʤ�ޤ���"
             "���ƥå׿�����ꤹ��ȡ�̵�������֤ˤʤä��塢���Υ��ƥå׿�����"
             "���Τޤ��Ե����ޤ���"
             "�̾�ϻȤ��ޤ���")
    (html:li (html:code "(wait step)")
             (html:br)
             "���ߤι����Τޤޡ����ꥹ�ƥå׿������Ե����ޤ���"
             "��ˡ�P������ɤ�ݻ�����Τ˻Ȥ��ޤ���")
    (html:li ;(html:code "(say \"...\" . opt-prior)")
             (html:code "(say \"text ...\")")
             (html:br)
             "�ƥ����Ȥ򶫤Ӥޤ���"
             "����ϡ������������夲��٤Υե������㡼�Ǥ���"
             (html:br)
             "�ޤ���S���ΥǥХå��ˤ�Ȥ��ޤ���"
             ;(html:br)
             ;"opt-prior�ˤ�0-7�ο��ͤ���ꤷ�ޤ���"
             ;"���ͤ��礭���ۤ������Ƕ��Ӥޤ���"
             ;"�ǥե���Ȥ�4�Ǥ���"
             ;"0�ξ������ǤϤʤ����������̤ˤʤ�ޤ���"
             )
    (html:li (html:code "(self-info)")
             (html:br)
             "�������Ǥμ�ʬ���Ȥξ����������ޤ���"
             "����ϡ��ʲ��Τ褦��list�Ȥ��������ޤ���"
             (html:br)
             (html:code "'(hp action wait bonus-flag)")
             (html:br)
             "hp�ϸ��ߤ�����(����100)��"
             "action�ϸ��ߤι���('g 'c 'p #f�Τ����줫)��"
             "wait�Ͼ��0��"
             "bonus-flag�ϸ��ߤϾ��#f�Ǥ���"
             )
    (html:li (html:code "(enemy-info)")
             (html:br)
             "�������Ǥ�Ũ�ξ����������ޤ���"
             "����η����ϡ�����Ū�ˤ�(self-info)��Ʊ���Ǥ���"
             (html:br)
             "wait�ϡ�����Ũ����ư��ǽ�ˤʤ�ޤǤΥ��ƥå׿��򼨤��ޤ���"
             "����(action-g)���ι�ư�ϡ��ֹ����ե������סֹ�ư���⡼�����ե������סֹ�ư��⡼�����ե������פ�ʬ����Ƥ��ꡢwait�������륹�ƥå׿��ϳƥե�����ñ�Τλ��֤ˤʤ�ޤ����ƥե������ι���ͤǤϤʤ�������դ��Ƥ���������"
             )
    (html:li (html:code "(info->hp (enemy-info))")
             (html:br)
             (html:code "(info->action (enemy-info))")
             (html:br)
             (html:code "(info->wait (enemy-info))")
             (html:br)
             (html:code "(info->bonus-flag (enemy-info))")
             (html:br)
             "ñ�ʤ�桼�ƥ���ƥ���³���Ǥ���"
             "���Τ�car cadr caddr cadddr���Τ�ΤǤ���"
             )
    ))


(define *html:freeplay-sample*
  (html:dl
    (html:dt "�Ҥ�����G��")
    (html:dd
      (html:pre
        (html:code
          (intersperse
            "\n"
            `("(define (battle-main)"
              "  (action-g)"
              "  (battle-main))"
              )))))
    (html:dt "�Ҥ�����C��")
    (html:dd
      (html:pre
        (html:code
          (intersperse
            "\n"
            `("(define (battle-main)"
              "  (action-c)"
              "  (battle-main))"
              )))))
    (html:dt "G���C�����ߤ˻Ȥ�")
    (html:dd
      (html:pre
        (html:code
          (intersperse
            "\n"
            `("(define (battle-main)"
              "  (action-g)"
              "  (action-c)"
              "  (battle-main))"
              )))))
    (html:dt "G�ࢪC����P��")
    (html:dd
      (html:pre
        (html:code
          (intersperse
            "\n"
            `("(define (battle-main)"
              "  (action-g)"
              "  (action-c)"
              "  (action-p)"
              "  (wait 30)"
              "  (battle-main))"
              )))))
    (html:dt "��꤬P��ʤ�C���򡢤���ʳ��ʤ�G��򿶤�")
    (html:dd
      (html:pre
        (html:code
          (intersperse
            "\n"
            `("(define (battle-main)"
              "  (if (eq? 'p (info->action (enemy-info)))"
              "    (action-c)"
              "    (action-g))"
              "  (battle-main))"
              )))))
    (html:dt "��̵꤬�����ʤ�G���򡢤���ʳ��ʤ�P��򹽤���")
    (html:dd
      (html:pre
        (html:code
          (intersperse
            "\n"
            `("(define (battle-main)"
              "  (if (info->action (enemy-info))"
              "    (action-p)"
              "    (action-g))"
              "  (battle-main))"
              )))))
    (html:dt "��Ф�(�Դ���)")
    (html:dd
      (html:pre
        (html:code
          (intersperse
            "\n"
            `("(define (battle-main)"
              "  (case (info->action (enemy-info))"
              "    ((g)"
              "     (niyari)"
              "     (let loop ()"
              "       (if (eq? 'g (info->action (enemy-info)))"
              "         (begin (action-p) (loop))"
              "         (action-g))))"
              "    ((c) (niyari) (action-g))"
              "    ((p) (niyari) (action-c))"
              "    (else (wait 1)))"
              "  (battle-main))"
              "(define (niyari)"
              "  (say \"���ͤι�ư�����ڤä��ꡪ\"))"
              )))))
    (html:dt "������٤�")
    (html:dd
      (html:pre
        (html:code
          (intersperse
            "\n"
            `(";; ����Ƚ�����٤�����١�"
              ";; ��Ʈ���르�ꥺ��Ū�ˤ�����Ʊ��Ȧ��"
              ";; �֤Ҥ�����G��פ���äƤ��餱�ޤ���"
              "(define (battle-main)"
              "  (talk)"
              "  (action-g)"
              "  (battle-main))"
              "(define (talk)"
              "  (let ((hp-self (info->hp (self-info)))"
              "        (hp-enemy (info->hp (enemy-info))))"
              "    (cond"
              "      ((<= hp-enemy 10) (say \"����ǤȤɤ����\"))"
              "      ((<= hp-self 10) (say \"���ܥ���\"))"
              "      ((< hp-enemy hp-self) (say \"�եá��夤�ۤ�\"))"
              "      ((and"
              "         (= hp-enemy 100)"
              "         (= hp-self 100))"
              "       (say \"�����褤��\"))"
              "      ((= hp-enemy hp-self) (say \"�߳Ѥξ��餫��\"))"
              "      ((<= hp-self 20) (say \"�ҥ����á�\"))"
              "      ((<= hp-self 30) (say \"���ġĽ����Ƥ��줧����\"))"
              "      ((<= hp-self 40) (say \"���áġĻ������á�\"))"
              "      ((<= hp-self 50) (say \"�ơ��궯���ġ�\"))"
              "      ((<= hp-self 80) (say \"�ʤ���������Ĥϡġ�\"))"
              "      (else #f))))"
              )))))
    ))


(define *html:history*
  (html:ul
    (html:li (html:code "2008/04/13")
             (html:br)
             "�������ѤΥ��顼���ˤ⡢"
             "����̿Ū���顼�ײ��̤�Ф��褦�ˤ��ޤ�����"
             )
    (html:li (html:code "2008/03/10 (2)")
             (html:br)
             "������ʤ�Ǥ�20�ö᤯�⤫����ΤϤޤ����Ȥ������ǡ�"
             "battle-main��³������λ������ڥʥ�ƥ���Ϳ����褦��"
             "���ͤ��ѹ����ޤ�����"
             (html:br)
             "�����ȼ��������ʸ�ϵڤӥ���ץ륳���ɤ��ѹ����ޤ�����"
             )
    (html:li (html:code "2008/03/10 (1)")
             (html:br)
             "��(define (battle-main))����������饯����ͤȤ⤬"
             "���٤˾��ʤ����ƥå׿���battle-main��³������λ�����硢"
             "eval/sv������/æ�Ф���ݤΥ����С��إåɤ������礭���ʤꡢ"
             "���֤������ä����˥����ॢ���Ȥ��Ƥ��ޤ�������б����ޤ�����"
             (html:br)
             "(����ñ�˥����ॢ�����ÿ��򿭤Ф��������ʤΤǡ�"
             "�����Фβ���پ����ˤ�äƤϡ�"
             "����Ǥ⥿���ॢ���Ȥ����礬����ޤ���"
             "���λ��Ϥ����ʤ�����"
             "���ͤ���ȡ�battle-main��³������λ������ڥʥ�ƥ���Ϳ��������"
             "�ɤ��ä����Ϥ��Ƥ��ޤ���)"
             )
    ))


(provide "sb-text")

