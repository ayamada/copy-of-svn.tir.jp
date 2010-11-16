;#!/usr/local/gauche/bin/gosh
;#!/usr/local/gauche/bin/speedygosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; S���Хȥ顼cui

;;; ����:
;;; - sys-setrlimit����
;;; - sbattle.scm check < ...
;;;   �ǡ�stdin��ή���줿S���Υǡ����δʰץ����å��Τ߹Ԥ���
;;;   stdin����ϡ��ʲ��Τ褦��keywords���Ϥ�����
;;;   '(:p1name "�ץ쥤�䡼1��̾��"
;;;     :p2name Ʊ��
;;;     :p1sexpr �ץ쥤�䡼1��S����ʸ�����Ϳ����
;;;     :p2sexpr Ʊ��
;;;     :p1hp �ץ쥤�䡼1�ν��HP
;;;     :p2hp Ʊ��
;;;     )
;;;   �֤��ͤϡ�stdout��S���Ȥ����֤���롣
;;;   ������ϡ�`(ok)���֤롣
;;;   �۾���ϡ�`(error ,errors)���֤롣errors�ϥ��顼���Ƥ�ʸ�����list��
;;; - sbattle.scm < ...
;;;   �ǡ����Ҥδʰץ����å���ԤäƤ��顢�ºݤμ¹Ԥ�Ԥ���
;;;   stdin��check��Ʊ�͡�
;;;   �֤��ͤϡ�stdout��S���Ȥ����֤���롣
;;;   ������ϡ�`(done ,winner ,logs)���֤롣winner��1�ޤ���2��
;;;   �۾���ϡ�`(error ,errors)���֤롣errors�ϥ��顼���Ƥ�ʸ�����list��


;;; TODO: ������Ū�ˡ��󶡤��Ƥ�褤��¾�μ�³��
;;; - Ũ��̾����ʸ����Ȥ��Ƽ��Ф�
;;; - �в᥹�ƥå׿�������


(use text.tree)
(use srfi-1)
(use util.list)
(use gauche.parameter)
(use gauche.charconv)
(use rfc.sha1)
(use util.digest)

(add-load-path "lib")
(use eval-sv)
(use eval-cu-lite)


;;; ----
;;; �����Х��ͤ����
(define-constant *draw-step* 9999)
(define-constant *debug* #f)

;;; ----
;;; ��Ʈ�ѥѥ�᡼���ڤ�«��

;; info�ϰʲ��Τ褦��list�ˤʤ�
;; '(hp action wait bonus-flag)
(define p:player1-info (make-parameter #f))
(define p:player2-info (make-parameter #f))
(define p:player-turn-is (make-parameter #f)) ; 1�ޤ���2
(define p:bat-first-is (make-parameter #f)) ; �ɤä����蹶���ä�����1��2��
(define p:logger (make-parameter #f)) ; �����ϼ�³��
(define p:player1-cont (make-parameter #f))
(define p:player2-cont (make-parameter #f))
(define p:finish-cont (make-parameter #f)) ; ���夬�夤�����˸Ƥַ�³

;; '(hp action wait bonus-flag)
(define info->hp car)
(define info->action cadr)
(define info->wait caddr)
(define info->bonus-flag cadddr)

;; wait��˷�³�������Ϥ��٤μ�³��
(define pass
  (enfold-entity
    'pass
    (lambda args #t)))

(define (logging self? logobj)
  ((p:logger) (cons
                (if self?
                  (p:player-turn-is)
                  (+ (logxor (- (p:player-turn-is) 1) 1) 1)) ; ȿž
                logobj)))

(define (action-g)
  ;; ����wait
  (unless (eq? (info->action (%self-info)) 'g)
    (%wait 20)
    (pass)
    (logging #t '(ready g))
    (%update-self-info! ; ��������
      (lambda (hp action wait bonus-flag)
        (values hp 'g wait bonus-flag))))
  ;; ��ư��wait
  (%wait 60)
  (pass)
  ;; ����
  (logging #t '(attack g))
  (let1 enemy-action (info->action (%enemy-info))
    (cond
      ((eq? enemy-action 'p)
       (logging #f '(parry))
       (logging #t '(stun))
       (%update-self-info!
         (lambda (hp action wait bonus-flag)
           (values hp #f (+ wait 100) #f))))
      (else
        (%update-enemy-info!
          (lambda (hp action wait bonus-flag)
            (logging #f '(damage 10))
            (logging #f `(hp ,(- hp 10)))
            (values (- hp 10) action wait bonus-flag)))
        (when (<= (info->hp (%enemy-info)) 0)
          ((p:finish-cont) (p:player-turn-is))))))
  (pass)
  ;; ��ư��wait
  (%wait 20)
  (pass)
  (%update-self-info! ; bonus-flag���
    (lambda (hp action wait bonus-flag)
      (values hp action wait #f)))
  (pass))
(define (action-c)
  ;; ����wait
  (unless (eq? (info->action (%self-info)) 'c)
    (%wait 10)
    (pass)
    (logging #t '(ready c))
    (%update-self-info! ; ��������
      (lambda (hp action wait bonus-flag)
        (values hp 'c wait bonus-flag))))
  ;; ��ư��wait
  (%wait 20)
  (pass)
  ;; ����
  (logging #t '(attack c))
  (let1 enemy-action (info->action (%enemy-info))
    (cond
      ((eq? enemy-action 'g)
       (logging #f '(guard)))
      ;((eq? action 'c)
      ; (logging #f '(guard)))
      (else
        (%update-enemy-info!
          (lambda (hp action wait bonus-flag)
            (logging #f '(damage 6))
            (logging #f `(hp ,(- hp 6)))
            (values (- hp 6) action wait bonus-flag)))
        (when (<= (info->hp (%enemy-info)) 0)
          ((p:finish-cont) (p:player-turn-is))))))
  (pass)
  ;; ��ư��wait
  (%wait 40)
  (pass)
  (%update-self-info! ; bonus-flag���
    (lambda (hp action wait bonus-flag)
      (values hp action wait #f)))
  (pass))
(define (action-p)
  ;; ����wait
  (unless (eq? (info->action (%self-info)) 'p)
    (%wait 30)
    (pass)
    (logging #t '(ready p))
    (%update-self-info! ; ��������
      (lambda (hp action wait bonus-flag)
        (values hp 'p wait bonus-flag))))
  ;; ��ư��/��ư��wait
  (%wait 10) ; ����ϡ�(action-p)Ϣ³�����б��ΰ٤��Ѱ�
  (%update-self-info! ; bonus-flag���
    (lambda (hp action wait bonus-flag)
      (values hp action wait #f)))
  (pass))

(define (action-off . opt-wait-step)
  ;; ����wait
  (unless (eq? (info->action (%self-info)) #f)
    ;; ����wait��0�ʤΤ�����
    (%update-self-info!
      (lambda (hp action wait bonus-flag)
        (logging #t '(ready #f))
        (values hp
                #f
                (+ wait (get-optional opt-wait-step 0))
                bonus-flag))))
  (pass))

(define (wait step)
  ;(logging #t '(wait))
  (%wait step))
(define (self-info)
  (list-copy (%self-info)))
(define (enemy-info)
  (list-copy (%enemy-info)))

(define (say message . opt-prior)
  (let ((prior (get-optional opt-prior 3))
        (message-true (if (list? message)
                        (with-output-to-string
                          (lambda ()
                            (write/ss message)))
                        (x->string message))))
    (logging #t `(say ,message-true ,prior))))

;; �ʲ��ϡ������Ѽ�³��
(define (set-self-info! new-val)
  ((get-accessor #t) new-val))
(define (set-enemy-info! new-val)
  ((get-accessor #f) new-val))

(define (%self-info)
  ((get-accessor #t)))
(define (%enemy-info)
  ((get-accessor #f)))
(define (%wait step)
  (unless (is-a? step <integer>)
    (error "step must be <integer>" step))
  (when (< step 0)
    (error "step must be positive" step))
  (%update-self-info!
    (lambda (hp action wait bonus-flag)
      (values hp
              action
              (+ wait (if bonus-flag
                        (x->integer (* 0.8 step))
                        step))
              bonus-flag))))

(define (get-accessor self?) ; ��ʬ�ʤ�#t��Ũ�ʤ�#f
  (cond
    ((eqv? (p:player-turn-is) 1) (if self? p:player1-info p:player2-info))
    ((eqv? (p:player-turn-is) 2) (if self? p:player2-info p:player1-info))
    (else
      (error "assertion"))))

(define (%update-self-info! proc)
  (update-info! #t proc))
(define (%update-enemy-info! proc)
  (update-info! #f proc))
(define (update-info! self? proc)
  ;; (define (proc hp action wait bonus-flag)
  ;;   ...
  ;;   (values hp action wait bonus-flag))
  (let1 accessor (get-accessor self?)
    (receive r (apply proc (accessor))
      (accessor r))))









(define (get-bat-first p1sexpr p2sexpr)
  (define (get-digest-odd? sexpr)
    (let1 str (write-to-string sexpr)
      (#/(1|3|5|7|9|b|d|f)$/ (digest-hexify
                               (sha1-digest-string str)))))

  (let ((p1odd? (get-digest-odd? p1sexpr))
        (p2odd? (get-digest-odd? p2sexpr)))
    (if (xor p1odd? p2odd?) 2 1)))

(define (xor a b)
  (cond
    ((and a b) #f)
    ((or a b) #t)
    (else #f)))

(define *bind-alist*
  `(
    (action-g   ,action-g)
    (action-c   ,action-c)
    (action-p   ,action-p)
    (action-off ,action-off)
    (wait       ,wait)
    (self-info  ,self-info)
    (enemy-info ,enemy-info)
    (say        ,say)
    (info->hp         ,info->hp)
    (info->action     ,info->action)
    (info->wait       ,info->wait)
    (info->bonus-flag ,info->bonus-flag)
    ))
(define (get-eval/sv)
  (make-eval/sv
    :bind-alist *bind-alist*
    :isolate-port? (not *debug*)
    ))

(define (get-eval/cu)
  (make-eval/cu
    :default-threshold 256
    :macro-threshold   256
    :loop-threshold    256
    :bind-alist *bind-alist*
    :isolate-port? (not *debug*)
    ))

(define *continued* (gensym))
(define *sexpr-battle-main*
  ;; ����ȴ��̵�¥롼���к��Ȥ��ơ����˰��ƻ��Ƥ�Ǥ���
  ;; TODO: ����ˡ�lambda���'loop�ƻ뤬�Ĥ��С����פˤʤ롩����
  `(begin
     (,pass)
     (battle-main)))

;; ���μ�³���ϡ���Ʈ�ζ�����ʬ��¹Ԥ���
;; �֤��ͤϡ��ɤ��餬���ä�������Ʈ��������
;; ���顼�㳰���ꤲ�뤫���Τ�ʤ��������
(define (battle:start p1name p1hp p1sexpr
                      p2name p2hp p2sexpr)
  ;; �Ƽ���ѿ����Ѱդ���
  (let ((bat-first (get-bat-first p1sexpr p2sexpr)) ; 1�ޤ���2
        (total-steps 0) ; �¹ԥ��ƥåץ�����
        (r-logs '()) ; �����˥���Ͽ����
        (p1eval/sv (get-eval/sv))
        (p2eval/sv (get-eval/sv))
        )
    (define (num->player-name num)
      (cond
        ((eqv? 1 num) p1name)
        ((eqv? 2 num) p2name)
        (else #f)))

    ;; eval/sv���battle-main�����ꤹ��(sexpr�ϴ��˰����ʻ�����ǧ����Ƥ����!)
    (p1eval/sv p1sexpr)
    (p2eval/sv p2sexpr)
    ;; parameter�����ꤹ��
    (parameterize ((p:player1-info `(,p1hp #f 0 #f)) ; hp action wait bonus
                   (p:player2-info `(,p2hp #f 0 #f))
                   (p:player1-cont #f) ; ����ͤ϶�
                   (p:player2-cont #f) ; ����ͤ϶�
                   (p:player-turn-is bat-first) ; 1�ޤ���2
                   (p:bat-first-is bat-first) ; 1�ޤ���2
                   (p:logger (lambda (logobj)
                               (let1 logobj2 (cons
                                               (num->player-name (car logobj))
                                               (cdr logobj))
                                 (set! r-logs
                                   (cons
                                     logobj2
                                     r-logs)))))
                   (p:finish-cont #f)
                   )
      (define (supervisor type symbol expr args return except)
        ;; ��³�����촹����Ԥ�
        (let/cc cont ; ³�Ԥη�³����
          ;; ³�Ԥη�³��ʬ���Ȥ�parameter����¸����
          (cond
            ((eqv? 1 (p:player-turn-is)) (p:player1-cont cont))
            ((eqv? 2 (p:player-turn-is)) (p:player2-cont cont))
            (else
              (error "assertion")))
          ;; �Ȥꤢ����ȴ����
          (return *continued*))
        ;; ���ƥå׿��򥫥���Ȥ���
        (inc! total-steps)
        ;; ���ƥå����"1000���ƥå׷в�"�Τ褦�ʥ���Ĥ�
        (when (zero? (modulo total-steps 1000))
          ((p:logger) (list #f 'elapse total-steps)))
        (when (< *draw-step* total-steps)
          ((p:logger) (list #f 'draw))
          (cond
            ((< (info->hp (p:player2-info)) (info->hp (p:player1-info)))
             ((p:finish-cont) 1))
            ((< (info->hp (p:player1-info)) (info->hp (p:player2-info)))
             ((p:finish-cont) 2))
            (else
              ((p:finish-cont) (+ (logxor (- (p:bat-first-is) 1) 1) ; ȿž
                                  1)))))
        ;; wait������ʤ顢wait�򸺤餷��ȴ����
        ;; (�����Ѽ¹ԤʤΤǡ�cont�򹹿�����ɬ�פ�̵��)
        (when (< 0 (info->wait (%self-info)))
          (%update-self-info!
            (lambda (hp action wait bonus-flag)
              (values hp action (- wait 1) bonus-flag)))
          (return *continued*))
        ;; �ץ�����¹Ԥ���
        (apply expr args))

      ;; ��Ʈ�򳫻Ϥ���
      ;; r�ˤϡ�1��2������
      (let1 r (let/cc complete
                (p:finish-cont complete)
                (while #t
                  ;; �ޤ�������³����������
                  (cond
                    ((eqv? (p:player-turn-is) 1)
                     (when (p:player1-cont)
                       ((p:player1-cont))) ; cont������ʤ顢�¹Ԥ������ʤ�
                     ;; cont��̵���ʤ顢battle-main��ư����
                     (guard (e (else
                                 ;; �¹Ի����顼��ȯ��
                                 ((p:logger) (list (p:player-turn-is) 'fumble))
                                 (%update-self-info!
                                   (lambda (hp action wait bonus-flag)
                                     (values hp action (+ wait 120) #f)))
                                 (p:player1-cont #f))) ; ����battle-main����
                       (unless (eq? *continued*
                                    (p1eval/sv *sexpr-battle-main* supervisor))
                         ;; ���ｪλ(�ڥʥ�ƥ�Ϳ���ƺƵ�ư)
                         ((p:logger) (list (p:player-turn-is) 'fumble))
                         (%update-self-info!
                           (lambda (hp action wait bonus-flag)
                             (values hp action (+ wait 120) #f)))
                         (p:player1-cont #f))) ; ����battle-main����
                     (p:player-turn-is 2)) ; ���Υ�����
                    ((eqv? (p:player-turn-is) 2)
                     (when (p:player2-cont)
                       ((p:player2-cont))) ; cont������ʤ顢�¹Ԥ������ʤ�
                     ;; cont��̵���ʤ顢battle-main��ư����
                     (guard (e (else
                                 ;; �¹Ի����顼��ȯ��
                                 ((p:logger) (list (p:player-turn-is) 'fumble))
                                 (%update-self-info!
                                   (lambda (hp action wait bonus-flag)
                                     (values hp action (+ wait 120) #f)))
                                 (p:player2-cont #f))) ; ����battle-main����
                       (unless (eq? *continued*
                                    (p2eval/sv *sexpr-battle-main* supervisor))
                         ;; ���ｪλ(�ڥʥ�ƥ�Ϳ���ƺƵ�ư)
                         ((p:logger) (list (p:player-turn-is) 'fumble))
                         (%update-self-info!
                           (lambda (hp action wait bonus-flag)
                             (values hp action (+ wait 120) #f)))
                         (p:player2-cont #f))) ; ����battle-main����
                     (p:player-turn-is 1)) ; ���Υ�����
                    (else
                      (error "assertion" (p:player-turn-is))))))
        ;; �Ǹ�ˡ��ǽ���Ʈ��̤���󥰤���
        ((p:logger) (list r 'won))
        ;; ��̤��֤�
        (values r (reverse r-logs))))))



;;; ----


(define (main:battle keywords)
  (let-keywords keywords ((p1name #f) ; ʸ����
                          (p2name #f) ; ʸ����
                          (p1sexpr #f) ; ʸ����
                          (p2sexpr #f) ; ʸ����
                          (p1hp 100) ; integer
                          (p2hp 100) ; integer
                          )
    (receive (r logs) (battle:start p1name p1hp (sexpr-str->sexpr p1sexpr)
                                    p2name p2hp (sexpr-str->sexpr p2sexpr))
      `(done ,r ,logs))))

(define (main:checkonly keywords)
  ;; �������褿�ʳ��ǡ����˸��ڤϴ�λ���Ƥ���Τǡ���̤��֤������Ǥ褤
  '(ok))

(define (sexpr-is-not-valid? sexpr)
  (guard (e (else (ref e 'message)))
    (receive (eval/cu env) (get-eval/cu)
      (eval/cu sexpr)
      ;; battle-main��«����¸�ߤ�������ǧ����
      (unless (global-variable-bound? env 'battle-main)
        (error "battle-main��³�����������Ƥ��ޤ���"))
      #f)))

(define (sexpr-str->sexpr sexpr-str)
  (call-with-input-string
    sexpr-str
    (lambda (port)
      (list*
        'begin
        (let1 s (read port)
          (when (eof-object? s) ; �����Ĥ��ɤ�ɬ�פ�����
            (error "S�������Ǥ�"))
          s)
        (let next ()
          (let1 s (read port)
            (if (eof-object? s)
              '()
              (cons s (next)))))))))

(define (check-sexpr-string sexpr-string target-str)
  ;; S����read�����å���Ԥ�
  (let1 sexpr (guard (e (else
                          (error
                            (string-append
                              target-str
                              "��S����read���˥��顼��ȯ�����ޤ���"
                              "\n"
                              (ref e 'message)))))
                (sexpr-str->sexpr sexpr-string))
    ;; �ºݤ�eval/sv���̤�������˽�λ��������ǧ����
    (let1 sexpr-error (sexpr-is-not-valid? sexpr)
      (when sexpr-error
        (error
          (string-append
            target-str
            "��S����eval���˥��顼��ȯ�����ޤ���"
            "\n"
            sexpr-error)))
      #f)))


(define (check-keywords keywords)
  (let-keywords keywords ((p1name #f) ; ʸ����
                          (p2name #f) ; ʸ����
                          (p1sexpr #f) ; ʸ����
                          (p2sexpr #f) ; ʸ����
                          (p1hp 100) ; integer
                          (p2hp 100) ; integer
                          )
    (unless p1name
      (error "�ץ쥤�䡼1��̾�������ꤵ��Ƥ��ޤ���"))
    (unless p2name
      (error "�ץ쥤�䡼2��̾�������ꤵ��Ƥ��ޤ���"))
    (unless p1sexpr
      (error "�ץ쥤�䡼1��S�������ꤵ��Ƥ��ޤ���"))
    (unless p2sexpr
      (error "�ץ쥤�䡼2��S�������ꤵ��Ƥ��ޤ���"))
    (when (equal? "" p1name)
      (error "����饯��1��̾�������Ǥ�"))
    (when (equal? "" p2name)
      (error "����饯��2��̾�������Ǥ�"))
    (when (< 64 (string-size p1name))
      (error "����饯��1��̾����Ĺ�����ޤ�"))
    (when (< 64 (string-size p2name))
      (error "����饯��2��̾����Ĺ�����ޤ�"))
    (when (equal? p1name p2name)
      (error "����饯��1�ȥ���饯��2��̾����Ʊ���Ǥ�"))
    (when (equal? "" p1sexpr)
      (error "����饯��1��S�������Ǥ�"))
    (when (equal? "" p2sexpr)
      (error "����饯��2��S�������Ǥ�"))
    (when (< 2048 (string-size p1sexpr))
      (error "����饯��1��S����Ĺ�����ޤ�"))
    (when (< 2048 (string-size p2sexpr))
      (error "����饯��2��S����Ĺ�����ޤ�"))
    ;; S����read�����å���Ԥ�
    (check-sexpr-string p1sexpr "����饯��1")
    (check-sexpr-string p2sexpr "����饯��2")
    ;; ���顼�㳰��Ф����˥����å����̤ä�
    #t))

;; ���¤�ɬ�פʤΤ��ǧ���Ƥ��顢����餷���ͤ����ꤷ�Ƥ���
(define *rlimit-alist*
  (list
    (cons RLIMIT_AS 0.5) ; address space (virtual memory) limit
    ;(cons RLIMIT_CORE #f)
    (cons RLIMIT_CPU 20) ; CPU time in seconds
    (cons RLIMIT_DATA 0.5) ; max data size
    ;(cons RLIMIT_FSIZE #f)
    ;(cons RLIMIT_LOCKS #f)
    ;(cons RLIMIT_MEMLOCK #f)
    ;(cons RLIMIT_MSGQUEUE #f)
    ;(cons RLIMIT_NICE #f)
    ;(cons RLIMIT_NOFILE #f)
    ;(cons RLIMIT_NPROC #f)
    (cons RLIMIT_RSS 0.5) ; max resident set size
    ;(cons RLIMIT_RTPRIO #f)
    ;(cons RLIMIT_SIGPENDING #f)
    ;(cons RLIMIT_SBSIZE #f)
    (cons RLIMIT_STACK 0.5) ; max stack size
    ;(cons RLIMIT_OFILE #f)
    ))
(define (set-rlimit!)
  (for-each
    (lambda (res.val)
      (let ((resource (car res.val))
            (value (cdr res.val)))
        (when value
          (let/cc cancel
            (let1 old-rlimit (sys-getrlimit resource)
              (when (< 0 value 1)
                ;; 0����1�δ֤��ͤξ��ϡ����Ȳ�᤹��褦�ˤ���
                ;; â����RLIM_INFINITY�λ��ϲ��⤷�ʤ�
                ;; (̵�¤˳���ݤ��Ƥ�̵�¤Τޤ�)
                (when (equal? old-rlimit RLIM_INFINITY)
                  (cancel))
                (let1 new-value (x->integer (* value old-rlimit))
                  (set! value new-value)))
              ;; �����ȿ�Ǥ���
              (unless (equal? old-rlimit value)
                (sys-setrlimit resource value)))))))
    *rlimit-alist*))

(define (main argv)
  (define (main-thunk)
    ;; argv��Ȥäư���������뤫��
    ;; ����Ȥ�stdin��ͳ�ˤ��뤫���ɤä��������
    ;; �¹ԥ⡼�ɤ�argv�Ƿ��ơ�
    ;; S����stdin�Ǽ������Τ�������������
    ;; stdin��keywords��list�Ǽ������Τ�������������
    (guard (e (else
                ;; ���顼���̤Ȥ����֤�
                (parameterize ((debug-print-width #t))
                  `(error (,(ref e 'message))))))
      (let1 checkonly (cond
                        ((null? (cdr argv)) #f) ; ����̵��
                        ((equal? (cadr argv) "check") #t) ; ��������check
                        (else ; ����ʳ�
                          (errorf "invalid argv ~s" argv)))
        ;; �ޤ���stdin����keywords���ɤ߹���
        ;; (������encoding��(gauche-character-encoding)����Ԥ���ok�Ȥ���)
        (let1 keywords (read)
          ;; keywords�θ��ڤ�Ԥ���
          ;; ���꤬���ä����ϡ����Τޤޥ��顼�㳰���ꤲ�Ƥ褤
          (check-keywords keywords)
          (if checkonly
            (main:checkonly keywords)
            (main:battle keywords))))))

  ;; rlimit������ꤹ��
  (set-rlimit!)

  ;; �֤��ͤϾ��S�����֤��Τǡ�cgi-main�����port�Υ�åԥ󥰤�Ԥ�
  ;; stdout�ϰ��Ū��stderr��ή���褦�ˤ���
  ;; (�ְ�äƲ������Ϥ��Ƥ�����ˤ�����Ū)
  (let1 result (with-output-to-port (current-error-port) main-thunk)
    (write result)
    (newline)
    (flush)
    0)) ;; ������ｪλ�Ǥ����Τ�����



