;;; tctest-play.el --- Input method tester. -*- lexical-binding: nil -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *General purpose key-input/result tester

(defvar tctest-play-default-params
  #s(hash-table data (:timeout 2 :error-filter tctest-default-error-filter))
  "Default values used by `tctest-play'.")

(defvar tctest--play-buffer nil "Buffer where keyboard macro is executed.")

(defun tctest-play (keys &rest args)
  "Create a new buffer and execute a keyboard macro defined by (kbd KEYS).
Then insert `<!>' at point.

Markers:
  `<TIMEOUT>' and `<ERROR:...>' indicate the point where the macro was
  abnormally terminated.
  `<DING>' indicates the point where the bell was rung.

Return the resulting buffer contents as a string.

Keyword arguments:
  :initial STR      Initial contents of the buffer.
  :timeout SECONDS  Stop the macro after the specified duration.
  :buf BUFFER       Use BUFFER instead of creating a new one.
  :no-ding t        Do not insert `<DING>'.
  :setup-fun FUN    Call FUN before running the macro.
  :cleanup-fun FUN  Call FUN after running the macro.
  :no-kbd t         Pass KEYS directly to `execute-kbd-macro'
                    without `kbd'.
  :error-filter FUN Use FUN to convert an error object to an
                    `<ERROR:>' string.
  :params HASH      Pass parameters using a hash-table object.

The HASH object passed as :params -- or a newly created one if
unspecified -- is populated with other parameters and passed as the
sole argument to both :setup-fun and :cleanup-fun."
  (let* ((params (tctest-fill-params tctest-play-default-params args))
	 (initial      (gethash :initial      params))
	 (timeout      (gethash :timeout      params))
	 (buf          (gethash :buf          params))
	 (no-kbd       (gethash :no-kbd       params))
	 (no-ding      (gethash :no-ding      params))
	 (setup-fun    (gethash :setup-fun    params))
	 (cleanup-fun  (gethash :cleanup-fun  params))
	 (error-filter (gethash :error-filter params)))
    (puthash :keys keys params)
    (when (null buf)
      (setq buf (generate-new-buffer "*play*"))
      (puthash :buf buf params))
    (setq tctest--play-buffer buf)
    (pop-to-buffer buf)
    (when initial
      (save-excursion
	(insert initial)))
    (when setup-fun
      (funcall setup-fun params))
    (unwind-protect
	(condition-case err
	    (with-timeout (timeout (error "TIMEOUT"))
	      (tctest--advice)
	      (set-match-data '(1 1)) ; somewhere harmless
	      (execute-kbd-macro (if no-kbd keys (kbd keys)))
	      (insert "<!>"))
	  (error
	   (insert (funcall error-filter err))))
      (tctest--unadvice no-ding)
      (when cleanup-fun
	(funcall cleanup-fun params)))
    buf))

(defun tctest-check (keys &rest args)
  "Compare the result of (tctest-play KEYS ...) with :expect STR parameter.
Return nil if they are equal.  Otherwise return the result as a string.
Specifying :show-buf t displays the result buffer with additional info."
  (let* ((params (tctest-fill-params nil args))
	 (show-buf    (gethash :show-buf    params))
	 (expect      (gethash :expect      params))
	 (cleanup-fun (gethash :cleanup-fun params))
	 (conf (current-window-configuration))
	 buf result status)
    (when (eq show-buf :defer-cleanup)
      (puthash :cleanup-postponed cleanup-fun params)
      (puthash :cleanup-fun nil params))
    (setq buf (apply #'tctest-play keys args))
    (setq result (with-current-buffer buf
		   (buffer-string)))
    (setq status (equal result expect))
    (if show-buf
	(tctest--show-status params status)
      (kill-buffer buf)
      (set-window-configuration conf))
    (if status
	nil
      result)))

(defvar tctest--ding-markers nil "List of points where (ding) is called.")
(defun tctest--advice ()
  "Temporarily disable features that prevent test execution."
  (setq tctest--ding-markers nil)
  (advice-add 'sit-for :around #'tctest-sit-for)
  (advice-add 'ding :around #'tctest--silent-ding))

(defun tctest--unadvice (no-ding)
  "Restore disabled features and insert <DING> markers."
  (when (not no-ding)
    (dolist (marker tctest--ding-markers)
      (goto-char marker)
      (insert "<DING>")))
  (advice-remove 'sit-for #'tctest-sit-for)
  (advice-remove 'ding #'tctest--silent-ding))

(defun tctest-sit-for (orig-fun &rest args)
  "Replace `sit-for' with no-op when in batch mode, where `sit-for' ignores
user inputs."
  (if noninteractive
      nil
    (apply orig-fun args)))

(defun tctest--silent-ding (&rest args)
  "`ding' that do not stop keyboard macro."
  ;; Inserting <DING> now invalidates (match-data) used by [MATCH].
  ;; Do it later.
  (with-current-buffer tctest--play-buffer
    (push (copy-marker (point)) tctest--ding-markers)))

(defun tctest-default-error-filter (err)
  "Default error-filter for `tctest-play'."
  (let ((type (nth 0 err))
	(msg  (nth 1 err)))
    (if (equal msg "TIMEOUT")
	"<TIMEOUT>"
      (format "<ERROR:%S:%s>" type msg))))

(defun tctest-fill-params (default-params plist)
  "Create a new hash-table (unless PLIST has :params HASH, in which
case the HASH is used) and fill it with key-value pairs in PLIST and
hash-table DEFAULT-PARAMS."
  (let ((params (plist-get plist :params)))
    (when (null params)
      (setq params (make-hash-table)))
    (when default-params
      (maphash (lambda (k v) (puthash k (gethash k params v) params))
	       default-params))
    (when (= (% (length plist) 2) 1)
      (error "Odd number of elements in KEY VALUE ... list"))
    (while plist
      (let* ((k (pop plist))
	     (v (pop plist)))
	(unless (eq k :params)
	  (puthash k v params))))
    params))

(defvar tctest--params nil
  "Params for `tctest-play', used by postponed cleanup-fun.")

(defun tctest--show-status (params status)
  "Append information to the result buffer of `tctest-play'."
  (let* ((initial  (gethash :initial  params))
	 (expect   (gethash :expect   params))
	 (buf      (gethash :buf      params))
	 (keys     (gethash :keys     params))
	 (show-buf (gethash :show-buf params))
	 (cleanup-msg (if (eq show-buf :defer-cleanup)
			  " and call cleanup-fun" ""))
	 status-pos)
    (with-current-buffer buf
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "----------- expect -----------\n")
      (insert (or expect "No :expect specified"))
      (unless (bolp) (insert "\n"))
      (insert "----------- status -----------\n")
      (insert (if status "OK" "FAILURE"))
      (setq status-pos (point))
      (insert "        ('q' to kill buffer" cleanup-msg ")\n")
      (insert "----------- initial -----------\n")
      (insert (or initial "No :initial specified"))
      (unless (bolp) (insert "\n"))
      (insert "----------- keys -----------\n")
      (insert keys "\n")
      (when current-input-method
	(toggle-input-method))
      (tctest-q-quit-mode)
      (setq-local tctest--params params)
      (add-hook 'kill-buffer-hook 'tctest--on-kill-buf nil t) ; t: local
      (goto-char status-pos))))

(defun tctest--on-kill-buf ()
  "Called when the result buffer of `tctest-play' is Killed."
  (let ((cleanup-fun (gethash :cleanup-postponed tctest--params)))
    (when cleanup-fun
      (funcall cleanup-fun tctest--params))))

(defvar tctest-q-quit-mode-map)
(define-minor-mode tctest-q-quit-mode
  "Minor mode where `q' kills buffer and window."
  :lighter " q-quit"
  :keymap '(("q" . tctest-quit-buffer))
  (setq buffer-read-only t)
  (setq minor-mode-overriding-map-alist
	(cons (cons 'tctest-q-quit-mode tctest-q-quit-mode-map)
	      minor-mode-overriding-map-alist)))

(defun tctest-quit-buffer ()
  "Kill the current buffer and its window."
  (interactive)
  (let ((buf (current-buffer)))
    (quit-window)
    (kill-buffer buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; T-Code specific features

(declare-function tcode-set-key      "tc")
(declare-function tcode-encode       "tc")
(declare-function tcode-key-to-char  "tc")
(defvar tcode-table)
(defvar tcode-mode-map)
(defvar tcode-use-postfix-bushu-as-default)
(defvar tcode-use-prefix-mazegaki)

(defvar tctest-cmp-default-params
  #s(hash-table data (:show-buf nil))
  "Default values used by `tctest-cmp'.")

(defun tctest-cmp (keys &rest args)
  "T-Code テスト用の設定を行なった上で、`tctest-check'を呼ぶ。"
  (toggle-input-method) ; load tc
  (toggle-input-method)
  (let* ((params (tctest-fill-params tctest-cmp-default-params args))
	 (setup-user-fun   (gethash :setup-fun   params))
	 (cleanup-user-fun (gethash :cleanup-fun params)))
    (when setup-user-fun
      (puthash :setup-user-fun   setup-user-fun   params))
    (when cleanup-user-fun
      (puthash :cleanup-user-fun cleanup-user-fun params))
    (puthash :setup-fun   #'tctest--setup params)
    (puthash :cleanup-fun #'tctest--cleanup params)
    (tctest-check (tctest-key-filter keys) :params params)))

(defvar tctest-key-abbrevs
  '(("[UNDO]" "C-x u" nil nil)
    ;; IME ON/OFF。テストの読みやすさのため、ON/OFF 別の記法を用意する。
    ("[IMON]"  "C-\\" nil nil)
    ("[IMOFF]" "C-\\" nil nil)
    ;; isearch のマッチ範囲を[]で囲む。
    ("[MATCH]" "C-c C-m" tctest-show-match nil)
    ;; モードラインの input method モード部分を挿入する。
    ("[MODE]"  "C-c C-l" tctest-show-modeline nil)
    ("[MODE]"  "C-c C-l" tctest-show-modeline isearch-mode-map))
  "キー列の略称の定義。(ABBREV KEY CMD KEYMAP) のリスト。
CMD が非 nil の場合は KEYMAP のシンボルが指すキーマップ上でバインドされる。
KEYMAP が nil の場合は tctest-mode-map が用いられる。")

(defun tctest-set-abbrev-keys (params)
  "`tctest-key-abbrevs' の設定に従ってキーバインドを行なう。"
  (dolist (tuple tctest-key-abbrevs)
    (let ((abbrev  (nth 0 tuple))
	  (key     (nth 1 tuple))
	  (cmd     (nth 2 tuple))
	  (key-map (or (nth 3 tuple) 'tctest-mode-map)))
      (when cmd
	(tctest-define-key params (symbol-value key-map) (kbd key) cmd)))))

(defun tctest-where (cmd)
  "コマンド CMD に割り当てられた T-Code キー列(文字のリスト)。"
  (let ((keys (tctest--where-iter cmd tcode-table nil)))
    (concat (mapcar #'tcode-key-to-char keys))))

;; key: 0..39 returned by (tcode-char-to-key CHAR)
(defun tctest--where-iter (cmd table keys-so-far)
  (cond ((eq cmd table)
	 (reverse keys-so-far))
	((vectorp table)
	 (catch 'out
	   (dotimes (i (length table))
	     (let ((ret (tctest--where-iter cmd (aref table i)
					    (cons i keys-so-far))))
	       (when ret
		 (throw 'out ret))))))))

(defun tctest-bushu-cmd (post)
  "前置(POST が nil のとき)または後置の部首変換コマンドを返す。"
  (if (eq post tcode-use-postfix-bushu-as-default)
      'tcode-bushu-begin-conversion
    'tcode-bushu-begin-alternate-conversion))

(defun tctest-maze-cmd (post)
  "前置(POST が nil のとき)または後置の交ぜ書き変換コマンドを返す。"
  (if (eq (not post) tcode-use-prefix-mazegaki)
      'tcode-mazegaki-begin-conversion
    'tcode-mazegaki-begin-alternate-conversion))

(defun tctest-bushu-keys (post)
  "前置(POST が nil のとき)または後置の部首変換キー列を返す。"
  (tctest-where (tctest-bushu-cmd post)))

(defun tctest-maze-keys (post)
  "前置(POST が nil のとき)または後置の交ぜ書き変換キー列を返す。"
  (tctest-where (tctest-maze-cmd post)))

(defun tctest-tc-char-p (ch)
  "CH が T-Code で入力される文字かどうか(厳密ではない)。
入力キー列中、この判定を満たす文字は、ASCII 文字列に変換される。"
  (= (char-width ch) 2))

(defun tctest--untc (keys)
  "文字列 KEYS のうち、T-Code 文字を ASCII 列に変換する。"
  (require 'tc)
  (let ((chs nil)
	key-seq)
    (dolist (ch (string-to-list keys))
      (if (tctest-tc-char-p ch)
	  (let ((key-seq (tcode-encode ch)))
	    (when (null key-seq)
	      (error "can't use non-tcode char in input keys"))
	    (dolist (key key-seq)
	      (push (tcode-key-to-char key) chs)))
	(push ch chs)))
    (concat (nreverse chs))))

(defun tctest--expand-key-abbrevs (keys)
  "文字列 KEYS 中の特殊キーワードを、対応するキー列に変換する。"
  (let* ((kuten-keys   (tctest-where 'tcode-switch-variable))
	 (2balnum-keys (tctest-where 'tcode-toggle-alnum-mode))
	 (conv-abbrevs
	  (list (list "[POSTBUSHU]" (tctest-bushu-keys t))
		(list "[PREBUSHU]"  (tctest-bushu-keys nil))
		(list "[POSTMAZE]"  (tctest-maze-keys t))
		(list "[PREMAZE]"   (tctest-maze-keys nil))
		;; 日本語句読点[KUTEN_J]と、ascii 句読点[KUTEN_A] は同じキー。
		;; テストの意図がわかりやすいよう、区別して書く。
		(list "[KUTEN_J]"   kuten-keys)
		(list "[KUTEN_A]"   kuten-keys)
		;; 全角英数[ALNUM_ZEN]と半角英数[ALNUM_HAN]も同じキー。
		(list "[ALNUM_ZEN]" 2balnum-keys)
		(list "[ALNUM_HAN]" 2balnum-keys))))
    (dolist (pair (append tctest-key-abbrevs conv-abbrevs))
      (let ((from (nth 0 pair))
	    (to   (nth 1 pair)))
	(setq to (concat " " to " "))
	(setq keys (replace-regexp-in-string (regexp-quote from)
					     to keys t t)))))
  keys)

(defun tctest-key-filter (keys)
  "文字列 KEYS の中の T-Code 文字と特殊キーワードをキー列に置換する。"
  (tctest--untc (tctest--expand-key-abbrevs keys)))

(defun tctest-show-modeline ()
  "モードラインの[]部分をバッファに挿入する。"
  (interactive)
  (insert (format "[%s]" (or current-input-method-title ""))))

(defun tctest-show-match ()
  "直前のサーチのマッチ位置を[]で囲む。"
  (interactive)
  (save-excursion
    (goto-char (match-end 0))
    (insert "]")
    (goto-char (match-beginning 0))
    (insert "[")))

(defun tctest--set-local-vars (alist)
  "(シンボル 値) のリストに従って、バッファローカル変数をセットする。"
  (dolist (pair alist)
    (let ((var (nth 0 pair))
	  (val (nth 1 pair)))
      (set (make-local-variable var) val))))

(defvar tctest-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-q") 'tctest-quit-buffer)
    map)
  "`tctest-cmp'によるテストで使用するキーバインディング。")

(define-derived-mode tctest-mode fundamental-mode "Tctest"
  "`tctest-cmp'のキーバインディングを提供する major mode。")

(defun tctest--setup (params)
  "`tctest-cmp'で作られるバッファの初期設定を行なう。"
  (let* ((vars           (gethash :vars           params))
	 (requires       (gethash :requires       params))
	 (input-method   (gethash :input-method   params))
	 (setup-user-fun (gethash :setup-user-fun params)))
    (tctest-mode) ;; これの前のsetq-localは忘れるので注意。
    (tctest-set-abbrev-keys params)
    (dolist (pkg requires)
      (require pkg))
    (when input-method
      (puthash :bak-input-method default-input-method params)
      (set-input-method input-method)
      (toggle-input-method)) ; 初期状態はIMオフ。
    (when vars
      (tctest--set-local-vars vars))
    (when setup-user-fun
      (funcall setup-user-fun params))))

(defun tctest--cleanup (params)
  "`tctest--setup'で行なった設定を取り消す。"
  (let ((bak-input-method (gethash :bak-input-method params))
	(define-keys      (gethash :define-key       params))
	(tcode-keys       (gethash :tcode-set-key    params))
	(cleanup-user-fun (gethash :cleanup-user-fun params)))
    (when cleanup-user-fun
      (funcall cleanup-user-fun params))
    (dolist (pair tcode-keys)
      (apply #'tcode-set-key pair))
    (dolist (triple define-keys)
      (apply #'define-key triple))
    (when bak-input-method
      (set-input-method bak-input-method))))

(defun tctest--add-to-list-param (params key val)
  "値がリストであるような PARAMS の要素の先頭に、VAL を追加する。"
  (let ((ls (gethash key params)))
    (push val ls)
    (puthash key ls params)))

(defun tctest-define-key (params map key cmd)
  "キーマップ MAP にキーバインディングを追加する。
`tctest-cmp'によるテスト終了時にこのバインディングは解除される。"
  (let ((old-cmd (lookup-key map key)))
    (tctest--add-to-list-param params :define-key (list map key old-cmd))
    (define-key map key cmd)))

(defun tctest-tcode-set-key (params key cmd)
  "`tcode-mode-map'にキーバインディングを追加する。
`tctest-cmp'によるテスト終了時にこのバインディングは解除される。"
  (let ((old-cmd (lookup-key tcode-mode-map key)))
    (tctest--add-to-list-param params :tcode-set-key (list key old-cmd))
    (tcode-set-key key cmd)))

(defun tctest-bind-katakana (params)
  "`#' でカタカナモードをオン/オフできるようにする。"
  (tctest--set-local-vars '((tcode-katakana-mode-indicator "カ")
			    (tcode-hiragana-mode-indicator "ひ")))
  (tctest-tcode-set-key params "#" 'tcode-toggle-katakana-mode))

(provide 'tctest-play)
