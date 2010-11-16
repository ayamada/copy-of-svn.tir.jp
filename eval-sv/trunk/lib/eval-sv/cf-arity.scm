;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; "camouflage arity" module
;;;
;;;  Copyright (c) 2008 Atsuo Yamada, All rights reserved.
;;;
;;;  Permission is hereby granted, free of charge, to any person
;;;  obtaining a copy of this software and associated documentation
;;;  files (the "Software"), to deal in the Software without restriction,
;;;  including without limitation the rights to use, copy, modify,
;;;  merge, publish, distribute, sublicense, and/or sell copies of
;;;  the Software, and to permit persons to whom the Software is
;;;  furnished to do so, subject to the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;  AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
;;;  OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;;;  IN THE SOFTWARE.


(define-module eval-sv.cf-arity
  (use srfi-1)

  (export
    solve-arity ; TODO: ��ä��ɤ�̾����ͤ���
    camouflage-arity
    ))
(select-module eval-sv.cf-arity)

;; arity���ᤷ��(ʣ��¸�ߤ�����ϡֺǤ�ˤ����פ�)arity�ͤ����ͤ��֤���
;; (method�ξ�硢������arity����������֤���������ʤΤǡ����λ��ͤȤ���)
;; TODO: �������ϡ��ֺǤ�ˤ����פǤϤʤ���ʣ����arity�ͤ�������ϡ�
;;       arity�ͤ�merge��ɬ�פˤʤ롣���ȤǼ�����ľ������
(define (solve-arity proc)
  (define (arity->num+opt a)
    (if (arity-at-least? a)
      (list (arity-at-least-value a) #t)
      (list a #f)))

  (let1 a (arity proc)
    (cond
      ((null? a) (values 0 #t)) ; ������method�ǡ����줬����ġ�dummy�ͤ��֤�
      ((not (list? a)) (apply values (arity->num+opt a)))
      (else
        (let* ((arities a)
               (num+opts (map arity->num+opt arities)) ; '(num opt)��list
               (sorted (sort
                         num+opts
                         (lambda (x y)
                           (cond
                             ;; ���������ʬ�Ǥޤ�Ƚ��
                             ((< (car x) (car y)) #t)
                             ((< (car y) (car x)) #f)
                             ;; ���������ʬ��Ʊ���ʤ顢���ץ���ʥ������Ƚ��
                             ((cadr x) #t)
                             ((cadr y) #f)
                             (else #t)))))
               )
          (apply values (car sorted)))))))

;; proc�������դ���arity�򡢻�����Τ褦�˸�����褦�˵�������
(define (camouflage-arity arg-num has-optional? proc)
  (let* (
         ;; ���ꥸ�ʥ�ΰ�������ܥ�Υꥹ��(arg:1 arg:2)
         (arg-list (map
                     (lambda (i)
                       (string->symbol
                         (format "arg:~d" i)))
                     (iota arg-num)))
         ;; ������optional������Ĥ������(arg:1 arg:2 . opt)
         (arg-list+ (apply list* (append arg-list
                                         (if has-optional? '(opt) '(())))))
         ;; Ŭ�Ѳ�ǽ�ʷ��ˤ������(list* arg:1 arg:2 opt)
         ;; (���Ҥξ��ˤ�ꡢopt�Ͼ��list�ʤΤǡ��Դ���list�ˤϤʤ�ʤ�)
         (arg-list* `(list* ,@arg-list ,(if has-optional? 'opt '())))
         ;; eval���뼰
         (result `(lambda ,arg-list+
                    (apply ,proc ,arg-list*)))
         )
    (eval result (current-module))))



(provide "eval-sv/cf-arity")

