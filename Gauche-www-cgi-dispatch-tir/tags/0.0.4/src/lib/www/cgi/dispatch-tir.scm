;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; cgi dispatch module

;;;   Copyright (c) 2005 atsuo yamada, All rights reserved.
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; WARN: エントリテーブルは、モジュール毎に保持される。
;;;       モジュールをまたいで定義はできない事に注意。
;;;       また、モジュール名が同じだと、衝突してしまう事にも注意。

;;; note: 初回ディスパッチ時にテーブルを生成する。少々重い。
;;; note: alist-tableに保存するentityは、以下の形式のpairとなる。
;;;       (cons define-cgi-entryの束縛名 define-cgi-entryの実体)
;;;       なので、デバッグの際には注意する事！

;;; usage:
;;; - 以下のように定義していく。
;;;   (define-cgi-entry (disp-cgi-menu ...) '(("cmd" "menu")) body...)
;;;   (define-cgi-entry disp-cgi-fallback '() disp-cgi-menu)
;;;   (define-cgi-entry (disp-cgi-hoge ...) '(("c" "a") ("submit" :*)) body...)
;;; - 束縛名とbodyの間に、alistによるtemplate指定が入る以外は、
;;;   通常のdefineと違いは無い。グローバルな束縛としても扱える。
;;; - 実際にディスパッチしたい時は、以下のようにしてdefineした内容を取り出す。
;;;   (let1 proc (cgi-params-dispatch cgi-params) ...)
;;; -- これで、cgi-paramsにマッチするものを選んでくれる。
;;; -- (let1 proc (cgi-params-dispatch cgi-params fallback) ...)
;;;    のように、明示的にfallbackを指定する事も可能。
;;; --- この場合でも、templateに'()が指定されているものがあれば、
;;;     そっちが優先される。
;;; - templateのalistの値に:*を指定する事で、wildcard指定として使える。
;;; - 上記で定義したentryは、(cgi-entry->url 'disp-cgi-menu)等として、
;;;   容易にurl化したりform化したりする事ができる。
;;;   この機能を使えば、cgiスクリプト内に、cgiパラメータを直指定する事を
;;;   少なくできる。少なくとも、画面の遷移が容易に分かるようになる。


(define-module www.cgi.dispatch-tir
  (use srfi-1)
  (use srfi-2) ; and-let*

  (use text.tree)
  (use text.html-lite)
  (use rfc.uri)
  (use www.cgi)

  (use www.cgi.dispatch-tir.alist-table)

  (export
    define-cgi-entry
    cgi-params-dispatch

    cgi-entry->url
    cgi-entry->form

    cgi-entry-name->template-alist
    get-dispatch-table ; for debug
    ))
(select-module www.cgi.dispatch-tir)


(define *module->dispatch-table-table* (make-hash-table 'eq?))

(define (module->dispatch-table module)
  (hash-table-get *module->dispatch-table-table* module #f))


(define (add-dispatch-entry! module bind-name template entity)
  (unless (every
            (cut pair? <>)
            template)
    (errorf
      "define-cgi-entry's template must be alist. (module=~a bind=~a)"
      module
      bind-name))
  (alist-table-put!
    (or
      (module->dispatch-table module)
      (let1 new-table (make <alist-table>)
        (hash-table-put! *module->dispatch-table-table* module new-table)
        new-table))
    template
    (cons
      bind-name
      entity)))



;; 概要:
;; - define-cgi-entryは、通常のdefineとしても機能する。
;; -- 但し、通常の関数として直接呼ぶ場合は、templateは使用されない。
(define-syntax define-cgi-entry
  (syntax-rules ()
    ((_ (name . args) require-template . body)
     (define-cgi-entry name require-template (lambda args . body)))
    ((_ name require-template expr)
     (begin
       ;; トップレベルにdefineする
       (define name expr)
       ;; エントリテーブルに登録
       ((with-module www.cgi.dispatch-tir add-dispatch-entry!)
        (module-name (current-module)) 'name require-template name)))))



(define *dummy-fallback* (gensym))
;; note: alist-table-getの結果のcdrを返す必要がある
(define (module&cgi-params->dispatch module cgi-params . opt-fallback)
  (let1 dispatch-table (module->dispatch-table module)
    (if dispatch-table
      (if (null? opt-fallback)
        (cdr (alist-table-get dispatch-table cgi-params))
        (let1 r (alist-table-get dispatch-table cgi-params *dummy-fallback*)
          (if (eq? r *dummy-fallback*)
            (car opt-fallback)
            (cdr r))))
      (if (null? opt-fallback)
        (error "no one cgi-entry are not defined")
        (car opt-fallback)))))


;; module&cgi-params->dispatchのラッパー
(define-syntax cgi-params-dispatch
  (syntax-rules ()
    ((_ cgi-params . opt-fallback)
     ((with-module www.cgi.dispatch-tir module&cgi-params->dispatch)
       (module-name (current-module))
       cgi-params
       . opt-fallback))))





(define (module&cgi-entry->url module entry-name . keywords)
  (let-keywords* keywords ((base-url (self-url/path-info))
                           (params '()) ; 追加cgiパラメータ
                           )
    (let1 template (module&cgi-entry-name->template-alist module entry-name)
      (unless template
        (errorf "not found entry ~a" entry-name))
      (append-params-to-url
        base-url
        (append template params)))))

(define-syntax cgi-entry->url
  (syntax-rules ()
    ((_ entry-name . keywords)
     ((with-module www.cgi.dispatch-tir module&cgi-entry->url)
       (module-name (current-module))
       entry-name
       . keywords))))




(define (module&cgi-entry->form module entry-name . keywords)
  (let-keywords* keywords ((base-url (self-url/path-info))
                           (params '()) ; 追加cgiパラメータ
                           (internal-html #f) ; html:form内のhtml、または#f
                           (submit-label "") ; internal-htmlが#fの時用ボタン
                           ;; 以下、formの属性を、必要に応じて追加
                           (method "post")
                           (target #f)
                           (enctype #f)
                           (name #f)
                           (id #f)
                           )
    (let1 template (module&cgi-entry-name->template-alist module entry-name)
      (define (alist->hidden alist)
        (map
          (lambda (name+values)
            (let ((name (car name+values))
                  (values (cdr name+values)))
              (map
                (lambda (value)
                  (html:input
                    :type "hidden"
                    :name name
                    :value value))
                (if (list? values) values '("")))))
          alist))

      (unless template
        (errorf "not found entry ~a" entry-name))
      (tree->string
        (html:form
          :action base-url
          :method method
          :target target
          :enctype enctype
          :name name
          :id id
          (alist->hidden template)
          (alist->hidden params)
          (if internal-html
            internal-html
            (html:input
              :type "submit"
              :value submit-label)))))))

(define-syntax cgi-entry->form
  (syntax-rules ()
    ((_ entry-name . keywords)
     ((with-module www.cgi.dispatch-tir module&cgi-entry->form)
       (module-name (current-module))
       entry-name
       . keywords))))






(define (module&cgi-entry-name->template-alist module entry-name)
  (let1 dispatch-table (module->dispatch-table module)
    (alist-table-any
      dispatch-table
      (lambda (template bind-name+entity)
        (and
          (eq? entry-name (car bind-name+entity))
          template)))))

(define-syntax cgi-entry-name->template-alist
  (syntax-rules ()
    ((_ entry-name)
     ((with-module www.cgi.dispatch-tir module&cgi-entry-name->template-alist)
       (module-name (current-module))
       entry-name))))



;; for debug
(define-syntax get-dispatch-table
  (syntax-rules ()
    ((_)
     (hash-table-get
       (with-module www.cgi.dispatch-tir *module->dispatch-table-table*)
       (module-name (current-module))
       #f))))


;;; ----
;;; utility for cgi
;;; (came from tir02.cgi)



(define (append-params-to-url url params)
  (if (null? params)
    url
    (receive (url-without-fragment fragment) (let1 m (#/(\#.*)/ url)
                                               (if m
                                                 (values (m 'before) (m 1))
                                                 (values url "")))
      (call-with-output-string
        (lambda (p)
          (define delimiter
            (if (#/\?/ url-without-fragment)
              (lambda () "&")
              (lambda ()
                (set! delimiter (lambda () "&"))
                "?")))

          (display url-without-fragment p)
          (let loop ((left-params params))
            (if (null? left-params)
              (display fragment p)
              (let ((key-encoded (uri-encode-string (caar left-params)))
                    (vals (cdar left-params))
                    (next-left (cdr left-params))
                    )
                (for-each
                  (lambda (val)
                    (display (delimiter) p) ; "?" or "&"
                    (display key-encoded p)
                    (display "=" p)
                    (display (uri-encode-string (if (string? val) val "")) p))
                  (if (list? vals) vals '("")))
                (loop next-left)))))))))


(define (completion-uri uri server-name server-port https)
  (receive (uri-scheme
            uri-userinfo
            uri-hostname
            uri-port
            uri-path
            uri-query
            uri-fragment)
    (uri-parse uri)
    ;; uri-schemeが無い時にだけ補完する
    ;; 但し、server-nameが与えられていない場合は補完できないので、何もしない
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
          :flagment uri-fragment)))))

(define (path->url path)
  (if (#/^\// path)
    (completion-uri
      path
      (cgi-get-metavariable "SERVER_NAME")
      (cgi-get-metavariable "SERVER_PORT")
      (cgi-get-metavariable "HTTPS"))
    path))

(define (self-url)
  (path->url (self-path)))
(define (self-url/path-info)
  (path->url (self-path/path-info)))

(define (self-path)
  (or (cgi-get-metavariable "SCRIPT_NAME") "/"))

(define (self-path/path-info)
  ;; note: PATH_INFOは既にデコードされてしまっているので使わない事
  (let* ((r (or (cgi-get-metavariable "REQUEST_URI") "/"))
         (m (#/\?/ r))
         )
    (if m
      (m 'before)
      r)))

;;; ----

(provide "www/cgi/dispatch-tir")

