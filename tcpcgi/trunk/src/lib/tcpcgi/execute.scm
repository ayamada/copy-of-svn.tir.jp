;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$


;;; ToDo : filer回りの実装
;;; ToDo : filer自体は遅延実行するが、事前に他の情報を返す為の
;;;        インターフェースを用意する

;;; ToDo : nphと同じように遅延実行するが、
;;;        CGI/1.1の結果をHTTP/1.1に変換するexecute-typeを用意する。あとで。
;;;        これには、vportの使用が必須となる（クライアント直結に見せかける為）
;;;        また、クライアントへは、chunkedで送信する必要がある。
;;;        （そうしないとpersistent connectionを維持出来ない）
;;;        apacheは既にコレを実装している(pipeでだが)。

;;; note : filerのみ、ディレクトリを扱える。
;;;        それ以外のasis-file等はディレクトリを扱えない(直接ファイル指定のみ)

;;; ToDo : 余裕が出来たら、file, directoryは細分化させる？

;;; ToDo : filer以外のファイル出力の404対応
;;;        - filerは遅延実行なので、どの時点で404を返すかが微妙


(define-module tcpcgi.execute
  (use gauche.parameter)
  (use gauche.regexp) ; regexp-quote
  (use srfi-1) ; filter-map
  (use srfi-2) ; and-let*
  (use file.util)
  (use rfc.uri)
  (use text.tree)
  (use util.list) ; alist->hash-table intersperse
  (use www.cgi)

  (use tcpcgi.execute.filer)
  (export
    *execute-type-canonical-table*

    <tcpcgi.execute>

    lazy-execute?
    always-connection-close?
    filer?
    execute ; (必要なら、実行して)各種情報を得る
    lazy-execute ; (遅延実行タイプのみ)遅延実行する
    ))
(select-module tcpcgi.execute)


(define *execute-type-canonical-table*
  (hash-table
    'eq?
    ;; type-name(alias ok) -> true-type-name(alias ng)
    '(script . script) ; 通常のcgi
    '(cgi . script) ; alias
    '(cgi-thunk . script) ; alias
    '(cgi-script . script) ; alias
    '(redirect . redirect) ; path-infoを利用する302リダイレクト
    '(path-redirect . redirect) ; alias
    '(location . location) ; path-info無視の302リダイレクト
    '(filer . filer) ; ディレクトリ/ファイル表示
    '(file . filer) ; alias
    '(directory . filer) ; alias
    '(mount . filer) ; alias
    '(asis-file . asis-file) ; asisファイル表示
    '(asis . asis-file) ; alias
    '(nph-script . nph-script) ; nph動作のcgi
    '(nph . nph-script) ; alias
    '(nph-cgi-script . nph-script) ; alias
    '(pc-nph-script . pc-nph-script) ; persistent connectionを維持するnph
    '(pc-nph . pc-nph-script) ; alias
    ;; 以下は、path-dispatchやvhost-dispatchに直指定された時用
    '(host-redirect . host-redirect) ; ホスト名での302リダイレクト
    '(vhost-redirect . host-redirect) ; alias
    ))


;; ToDo : execute-listスロットが更新される毎にexecute-slot-update!を呼ぶ
(define-class <tcpcgi.execute> ()
  (
   ;; '(symbol target . keywords) ; 形式のリスト。
   ;; symbolは動作タイプ（その場合はtargetの型で動作が決定する）。省略可能。
   ;; targetには通常、cgi-thunkまたはpath文字列を指定する。
   ;; keywordsには認証パラメータ等を設定可能（未実装）。
   (execute-list :accessor execute-list-of
                 :init-keyword :execute-list
                 :init-value #f)
   ;; 以下のスロットは、execute-listスロットから自動生成される。
   ;; 動作タイプ。symbol。
   (execute-type :accessor execute-type-of
                 :init-value #f)
   ;; 動作ターゲット。以下のどれか。
   ;; proc, 文字列, <tcpcgi.execute.filer>
   (execute-target :accessor execute-target-of
                   :init-value #f)
   ;; 指定キーワード。execute-listから取得。
   (execute-keywords :accessor execute-keywords-of
                     :init-value '())
   ))


(define-method execute-slot-update! ((self <tcpcgi.execute>))
  (define (make-filer path . keywords)
    (apply
      make <tcpcgi.execute.filer>
      :root-path (sys-normalize-pathname
                   path
                   :absolute #t
                   :expand #t
                   :canonicalize #t)
      keywords))

  (let1 pre-execute-list (execute-list-of self)
    (when pre-execute-list ; #fの場合は何もしない
      (let* (
             ;; execute-listは、listではなくthunkや文字列一個だけの場合がある
             (execute-list (if (list? pre-execute-list)
                             pre-execute-list
                             (list pre-execute-list)))
             (car-execute-list (car execute-list))
             (pre-execute-type (cond
                                 ((symbol? car-execute-list) car-execute-list)
                                 ((procedure? car-execute-list) 'script)
                                 ((string? car-execute-list) 'filer)
                                 (else (error "invalid execute-list"))))
             (execute-type (hash-table-get
                             *execute-type-canonical-table* pre-execute-type))
             (cdr-execute-list (if (symbol? car-execute-list)
                                 (cdr execute-list)
                                 execute-list))
             (car-cdr-execute-list (car cdr-execute-list))
             (execute-target (case execute-type
                               ('filer (apply make-filer cdr-execute-list))
                               (else car-cdr-execute-list)))
             (execute-keywords (cdr cdr-execute-list))
             )
        (set! (ref self 'execute-type) execute-type)
        (set! (ref self 'execute-target) execute-target)
        (set! (ref self 'execute-keywords) execute-keywords)
        ))))


(define-method initialize ((self <tcpcgi.execute>) initargs)
  (next-method)
  (execute-slot-update! self))






(define-method lazy-execute? ((self <tcpcgi.execute>))
  (memq
    (execute-type-of self)
    '(filer
      nph-script
      pc-nph-script
      )))
(define-method always-connection-close? ((self <tcpcgi.execute>))
  (memq
    (execute-type-of self)
    '(nph-script
      )))
(define-method filer? ((self <tcpcgi.execute>))
  (eq? 'filer (execute-type-of self)))







(define (execute-script self input-port)
  (with-output-to-string
    (lambda ()
      (with-input-from-port
        input-port
        (execute-target-of self))))) ; thunkが返り、それが実行される
(define (execute-redirect self input-port)
  (list
    302
    :location (string-append
                (execute-target-of self)
                (or (cgi-get-metavariable "PATH_INFO") ""))))
(define (execute-location self input-port)
  (list
    302
    :location (execute-target-of self)))
(define (execute-host-redirect self input-port)
  (list
    302
    :location (let* ((scheme (if (cgi-get-metavariable "HTTPS")
                               "https"
                               "http"))
                     (host (execute-target-of self))
                     (default-port (if (string=? "http" scheme)
                                     80 443))
                     (port (and-let* ((server-port-string
                                        (cgi-get-metavariable "SERVER_PORT"))
                                      (server-port
                                        (x->number server-port-string))
                                      )
                             (and
                               (not (eqv? default-port server-port))
                               server-port)))
                     (path* (or (cgi-get-metavariable "REQUEST_URI") "/"))
                     )
                (uri-compose
                  :scheme scheme
                  :host host
                  :port port
                  :path* path*
                  ))))
(define (execute-asis-file self input-port)
  (let1 file (execute-target-of self)
    (cond
      ((not (file-exists? file)) (list 404))
      ((not (file-is-readable? file)) (list 401))
      ;; ToDo : request-methodのチェック等を行う必要がある
      (else (file->string file)))))

;; nph-script系。遅延実行される。直にクライアントにコンテンツを返す。
(define (execute-filer self input-port)
  ;; filerのmethodを呼び出し、適切に200, 404, 301等を返すようにする
  ;; ToDo : まだ
  ;; ToDo : asis-fileと共通化可能な部分は共通化する
  (let1 file (execute-target-of self)
    (cond
      ((not (file-exists? file)) (list 404))
      ((not (file-is-readable? file)) (list 401))
      ;; ToDo : request-methodのチェック等を行う必要がある
      (else (list 200)))))
(define (execute-nph-script self input-port)
  (list #f))





;; note : 以下のテーブルは各procedureの定義後で定義する必要がある
(define *execute-table*
  (hash-table
    'eq?
    ;; type-name -> procedure
    `(script . ,execute-script)
    `(redirect . ,execute-redirect)
    `(location . ,execute-location)
    `(host-redirect . ,execute-host-redirect)
    `(asis-file . ,execute-asis-file)

    `(filer . ,execute-filer)
    `(nph-script . ,execute-nph-script)
    `(pc-nph-script . ,execute-nph-script)
    ))



;; このmethodで得られる情報は、次のどちらか
;; - cgi/1.1-response (文字列)
;; - (cons response-code response-keywords)
;; -- response-keywordsは、各response-codeに固有な必要情報
;; cgi-thunk実行時にエラー例外が発生した場合、そのまま例外が投げられるので
;; 適切にキャッチする事が必要。
(define-method execute ((self <tcpcgi.execute>) . optional)
  (let* (
         (input-port (get-optional optional (current-input-port)))
         (execute-type (execute-type-of self))
         (proc (hash-table-get *execute-table* execute-type))
         )
    (proc self input-port)))







(define (lazy-execute-filer self)
  ;;;; ToDo : まだ
  #f)
(define (lazy-execute-nph-script self)
  ;; thunkを実行するだけ
  ((execute-target-of self)))


;; note : 以下のテーブルは各procedureの定義後で定義する必要がある
(define *lazy-execute-table*
  (hash-table
    'eq?
    ;; type-name -> procedure
    `(filer . ,lazy-execute-filer)
    `(nph-script . ,lazy-execute-nph-script)
    `(pc-nph-script . ,lazy-execute-nph-script) ; 接続維持の有無が違うだけ
    ))

(define-method lazy-execute ((self <tcpcgi.execute>) . optional)
  (and
    (lazy-execute? self)
    (let* (
           (input-port (get-optional optional (current-input-port)))
           (execute-type (execute-type-of self))
           (proc (hash-table-get *lazy-execute-table* execute-type))
           )
      (with-input-from-port
        input-port
        (lambda ()
          ;; 遅延実行時には、stdoutに直接結果を返す
          (proc self)
          #t))))) ; 遅延実行時には、返り値は意味を持たないが、一応





(provide "tcpcgi/execute")

