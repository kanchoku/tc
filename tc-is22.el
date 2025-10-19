;;; tc-is22.el --- T-Code isearch modification for Emacs 22.*.

;; Copyright (C) 1994,97-2001, 2005 Kaoru Maeda, Mikihiko Nakao, KITAJIMA Akira and Masayuki Ataka

;; Author: Kaoru Maeda <maeda@src.ricoh.co.jp>
;;      Mikihiko Nakao
;;      KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;;      Masayuki Ataka <masayuki.ataka@gmail.com>
;; Maintainer: Masayuki Ataka
;; Create: 12 Feb (Sat), 2005

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.

;;; Code:

(if (< (string-to-number emacs-version) 22)
    (error "tc-is22 cannot run on NEmacs/Mule/Emacs20/21.  Use Emacs 22 or later!"))

;;;
;;;  User Variables
;;;
(defvar tcode-isearch-start-state nil
  "*インクリメンタルサーチ開始時のTコードモードを指定する。
       nil: バッファのTコードモードに同期(デフォールト)。
       t:   バッファのTコードモードと独立。開始時はバッファと同じ。
       0:   バッファと独立に常に非Tコードモードサーチから開始。
       1:   バッファと独立に常にTコードモードサーチから開始。
バッファローカル変数。")
(make-variable-buffer-local 'tcode-isearch-start-state)
(setq-default tcode-isearch-start-state nil)

(defcustom tcode-isearch-enable-wrapped-search t
  "*2バイト文字でサーチするときに、空白や改行を無視する。"
  :type 'boolean :group 'tcode)

(defcustom tcode-isearch-ignore-regexp "[\n \t]*"
  "* 2バイト文字間に入る正規表現。
`tcode-isearch-enable-wrapped-search' が t のときのみ有効。"
  :type 'regexp :group 'tcode)

(defcustom tcode-isearch-special-function-alist
  '((tcode-bushu-begin-conversion . tcode-isearch-bushu-conversion-command)
    (tcode-bushu-begin-alternate-conversion
     . tcode-isearch-bushu-alternate-conversion-command)
    (tcode-mazegaki-begin-alternate-conversion . tcode-isearch-prefix-mazegaki)
    (tcode-mazegaki-begin-conversion . tcode-isearch-postfix-mazegaki)
    (tcode-toggle-alnum-mode))
  "*isearch中での特殊なコマンドの入力に対する代替コマンドの alist。"
  :group 'tcode)

;;;
;;; Default key binding
;;;
(when (eq tcode-emacs-version 'xemacs)
  (define-key isearch-mode-map "\C-\\" 'isearch-toggle-tcode)
  (put 'isearch-toggle-tcode 'isearch-command t)) ; for XEmacs

;;;
;;; patch to original functions in isearch.el of Emacs 22
;;;

;; isearch-word -> isearch-regexp-function from 25.1
(defsubst tcode-isearch-regexp-function ()
  (or (bound-and-true-p isearch-regexp-function)
      (bound-and-true-p isearch-word)))

;; isearch-message-state -> isearch--state-message from 24
(defsubst tcode-isearch--state-message (x)
  (or (when (fboundp 'isearch--state-message)
	(isearch--state-message x))
      (when (fboundp 'isearch-message-state)
	(isearch-message-state x))))

;; isearch-top-state -> none
;; -> (isearch--set-state (car isearch-cmds)) from 24
(defsubst tcode-isearch-top-state ()
  (or (bound-and-true-p isearch-top-state)
      (when (fboundp 'isearch--set-state)
	(isearch--set-state (car isearch-cmds)))))

(defadvice isearch-search-string (around tcode-handling activate)
  (let ((isearch-regexp (if (or (tcode-isearch-regexp-function) isearch-regexp)
                            isearch-regexp
                          tcode-isearch-enable-wrapped-search)))
    ad-do-it))

(defun isearch-printing-char ()
  "Add this ordinary printing character to the search string and search."
  (interactive)
  (let ((char (isearch-last-command-char)))
    (if (and (boundp 'tcode-mode) tcode-mode)
	;; isearch for T-Code
	(let* ((decoded (tcode-decode-chars (isearch-last-command-char)))
	       (action (car decoded))
	       (prev (tcode-isearch-bushu)))
	  (cond ((null action)
		 (ding))
		((stringp action)
		 (setq action
		       (mapconcat 'char-to-string
				  (tcode-apply-filters
				   (string-to-list action))
				  nil))
		 (tcode-isearch-process-string action prev))
		((char-or-string-p action)
		 (tcode-isearch-process-string
		  (char-to-string (car (tcode-apply-filters (list action))))
		  prev))
		((and (not (tcode-function-p action))
		      (consp action))
		 (tcode-isearch-process-string
		  (mapconcat 'char-to-string
			     (tcode-apply-filters
			      (mapcar 'string-to-char
				      (delq nil action)))
			     nil)
		  prev))
		((tcode-function-p action)
		 (let ((func (assq action
				   tcode-isearch-special-function-alist)))
		   (if func
		       (funcall (or (cdr func)
				    action))
		     (tcode-isearch-process-string
		      (mapconcat 'char-to-string (cdr decoded) nil)
		      prev))))
		(t
		 (ding))))
      ;; original behaviour
      (if (= char ?\S-\ )
	  (setq char ?\ ))
      (if (and enable-multibyte-characters
	       (>= char ?\200)
	       (<= char ?\377))
	  (if (keyboard-coding-system)
	      (isearch-process-search-multibyte-characters char)
	    (isearch-process-search-char (unibyte-char-to-multibyte char)))
	(if current-input-method
	    (isearch-process-search-multibyte-characters char)
	  (isearch-process-search-char char))))))

(defadvice isearch-process-search-char (around tcode-handling activate)
  "Extention for T-code"
  (if (and (not isearch-regexp)
	   (boundp 'tcode-isearch-enable-wrapped-search)
	   tcode-isearch-enable-wrapped-search
	   (memq char '(?$ ?* ?+ ?. ?? ?[ ?\\ ?] ?^)))
      (let ((s (char-to-string char)))
	(isearch-process-search-string (concat "\\" s) s))
    ad-do-it))

(defun isearch-yank-word ()
  "Pull next word from buffer into search string."
  (interactive)
  (isearch-yank-internal (lambda ()
			   (if (= (char-width (char-after)) 2)
			       (forward-char 1)
			     (forward-word 1))
			   (point))))

(defun isearch-yank-string (string)
  "Pull STRING into search string."
  ;; Downcase the string if not supposed to case-fold yanked strings.
  (if (and isearch-case-fold-search
	   (eq 'not-yanks search-upper-case))
      (setq string (downcase string)))
  (if isearch-regexp (setq string (regexp-quote string)))
  (setq isearch-string (concat isearch-string
			       (tcode-isearch-make-string-for-wrapping string))
	isearch-message
	(concat isearch-message
		(mapconcat 'isearch-text-char-description string ""))
	;; Don't move cursor in reverse search.
	isearch-yank-flag t)
  (isearch-search-and-update))

(defun isearch-repeat (direction)
  ;; Utility for isearch-repeat-forward and -backward.
  (if (eq isearch-forward (eq direction 'forward))
      ;; C-s in forward or C-r in reverse.
      (if (equal isearch-string "")
	  ;; If search string is empty, use last one.
	  (setq isearch-string
		(or (if isearch-regexp
			(car regexp-search-ring)
		      (car search-ring))
		    (error "No previous search string"))
		isearch-message
		(mapconcat 'isearch-text-char-description
			   (tcode-isearch-remove-ignore-regexp isearch-string)
			   "")
		isearch-case-fold-search isearch-last-case-fold-search)
	;; If already have what to search for, repeat it.
	(or isearch-success
	    (progn
	      (if isearch-wrap-function
		  (funcall isearch-wrap-function)
		(goto-char (if isearch-forward (point-min) (point-max))))
	      (setq isearch-wrapped t))))
    ;; C-s in reverse or C-r in forward, change direction.
    (setq isearch-forward (not isearch-forward)))

  (setq isearch-barrier (point))	; For subsequent \| if regexp.

  (if (equal isearch-string "")
      (setq isearch-success t)
    (if (and isearch-success
	     (equal (point) isearch-other-end)
	     (not isearch-just-started))
	;; If repeating a search that found
	;; an empty string, ensure we advance.
	(if (if isearch-forward (eobp) (bobp))
	    ;; If there's nowhere to advance to, fail (and wrap next time).
	    (progn
	      (setq isearch-success nil)
	      (ding))
	  (forward-char (if isearch-forward 1 -1))
	  (isearch-search))
      (isearch-search)))

  (isearch-push-state)
  (isearch-update))

(defun tcode-isearch-read-string ()
  "インクリメンタルサーチ中に文字列を読み込む。"
  (let* (overriding-terminal-local-map
	 (minibuffer-setup-hook (lambda ()
				  (tcode-activate tcode-mode)))
	 (string (read-string (concat "Isearch read: " isearch-message)
			      nil nil nil t)))
    (unless (string= string "")
      (tcode-isearch-process-string string nil))))

(defun tcode-isearch-prefix-mazegaki ()
  "インクリメンタルサーチ中に前置型の交ぜ書き変換を行う。"
  (let* (overriding-terminal-local-map
	 (minibuffer-setup-hook (lambda ()
				  (tcode-activate tcode-mode)
				  (tcode-mazegaki-put-prefix)))
	 (string (read-string (concat "Isearch read: " isearch-message)
			      nil nil nil t)))
    (unless (string= string "")
      (tcode-isearch-process-string string nil))))

(defun tcode-isearch-postfix-mazegaki ()
  "インクリメンタルサーチ中に後置型の交ぜ書き変換を行う。"
  (let ((orig-isearch-cmds isearch-cmds)
	normal-end)
    (unwind-protect
	(let ((current-string isearch-message))
	  ;; clear isearch states
	  (while (cdr isearch-cmds)
	    (isearch-pop-state))
	  (let* (overriding-terminal-local-map
		 (minibuffer-setup-hook
		  (lambda ()
		    (tcode-activate tcode-mode)
		    (tcode-mazegaki-begin-conversion nil)))
		 (string (read-string "Isearch read: "
				      current-string nil nil t)))
	    (unless (string= string "")
	      (tcode-isearch-process-string string nil)
	      (setq normal-end t))))
      (unless normal-end
	(setq isearch-cmds orig-isearch-cmds)
	(tcode-isearch-top-state)))))

(defun isearch-toggle-tcode ()
  "インクリメンタルサーチ中のTコードモードをトグルする。"
  (interactive)
  (unless tcode-isearch-start-state
    (toggle-input-method))
  (isearch-update))

(defun tcode-isearch-bushu-henkan (c1 c2)
  ;; インクリメンタルサーチ中に C1 と C2 とで部首合成変換する。
  (let ((c (tcode-bushu-compose-two-chars (string-to-char c1)
					  (string-to-char c2))))
    (if c
	(let ((s (char-to-string c)))
	  (let ((msg (tcode-isearch--state-message (car isearch-cmds))))
	    (while (and msg
			(string= msg (tcode-isearch--state-message (car isearch-cmds))))
	      (isearch-delete-char)))
	  (let ((msg (tcode-isearch--state-message (car isearch-cmds))))
	    (while (and msg
			(string= msg (tcode-isearch--state-message (car isearch-cmds))))
	      (isearch-delete-char)))
	  (isearch-process-search-string
	   (tcode-isearch-make-string-for-wrapping s) s))
      (ding)
      (isearch-update))))

(defun tcode-isearch-process-string (str prev)
  "文字 STR を検索文字列に加えて検索する。
PREV と合成できるときはその合成した文字で検索する。"
  (if (stringp prev)
      (tcode-isearch-bushu-henkan prev str)
    (isearch-process-search-string
     (if prev
	 ""
       (tcode-isearch-make-string-for-wrapping str)) str)))

(defun tcode-regexp-unquote (str)
  (let* ((ll (string-to-list str))
	 (l ll))
    (while l
      (if (eq (car l) ?\\)
	  (progn
	    (setcar l (car (cdr l)))
	    (setcdr l (cdr (cdr l)))))
      (setq l (cdr l)))
    (mapconcat (function char-to-string) ll nil)))

(defun tcode-isearch-remove-ignore-regexp (str)
  "変数 `tcode-isearch-enable-wrapped-search' が nil でないとき、
STR から `tcode-isearch-ignore-regexp' を取り除く。"
  (if (or (not tcode-isearch-enable-wrapped-search)
	  isearch-regexp)
      str
    (let (idx
	  (regexp-len (length tcode-isearch-ignore-regexp)))
      (while (setq idx (string-match
			(regexp-quote tcode-isearch-ignore-regexp)
			str))
	(setq str (concat (substring str 0 idx)
			  (substring str (+ idx regexp-len) nil))))
      (tcode-regexp-unquote str))))

(defun tcode-isearch-make-string-for-wrapping (string)
  (let ((string-list (and string
			  (string-to-list string))))
    (if (and tcode-isearch-enable-wrapped-search
	     (not isearch-regexp)
	     string-list)
	(mapconcat
	 (lambda (a)
	   (let ((s (char-to-string a)))
	     (cond ((and (string-match tcode-isearch-ignore-regexp s)
			 (> (match-end 0) 0))
		    tcode-isearch-ignore-regexp)
		   ((= (char-width a) 2)
		    (concat tcode-isearch-ignore-regexp s))
		   (t
		    (regexp-quote (char-to-string a))))))
	 string-list
	 nil)
      string)))

(defun tcode-isearch-start-bushu ()
  "Tコードモードインクリメンタルサーチ中の前置型部首合成変換を始める。"
  (tcode-bushu-init 2)
  (setq isearch-message (concat isearch-message "▲"))
  (isearch-push-state)
  (isearch-update))

(defun tcode-isearch-postfix-bushu ()
  "Tコードモードインクリメンタルサーチ中の後置型部首合成変換を始める。"
  (let ((p1 (string-match "..$" isearch-message))
	(p2 (string-match ".$"  isearch-message)))
    (if (null p1)
	(ding)
      (tcode-bushu-init 2)
      (tcode-isearch-bushu-henkan (substring isearch-message p1 p2)
				  (substring isearch-message p2)))))

(defun tcode-isearch-bushu ()
  "isearch-message中の部首合成の文字を調べる。"
  (cond
   ((string-match "▲$" isearch-message)
    t)
   ((string-match "▲.$" isearch-message)
    (substring isearch-message (string-match ".$" isearch-message)))
   (t
    nil)))

(defun tcode-isearch-bushu-alternate-conversion-command ()
  "isearch中で通常とは逆の型の部首合成変換を始める。"
  (interactive)
  (if tcode-use-postfix-bushu-as-default
      (tcode-isearch-start-bushu)
    (tcode-isearch-postfix-bushu)))

(defun tcode-isearch-bushu-conversion-command ()
  "isearch中で部首合成変換を始める。"
  (interactive)
  (if (not tcode-use-postfix-bushu-as-default)
      (tcode-isearch-start-bushu)
    (tcode-isearch-postfix-bushu)))

(defun tcode-isearch-init ()
  "Tコードモードインクリメンタルサーチの初期化を行う。"
  (setq tcode-mode (if (numberp tcode-isearch-start-state)
		       (if (zerop tcode-isearch-start-state) nil t)
		     (and (boundp 'tcode-mode)
			  tcode-mode)))
  (isearch-update))

(add-hook 'isearch-mode-hook 'tcode-isearch-init)

(provide 'tc-is22)

;;; tc-is22.el ends here
