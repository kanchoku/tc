;;; tc-sysdep.el --- system-dependent routines & variables for T-Code
;;
;; Copyright (C) 1997--2001 Kaoru Maeda, Yasushi Saito and Akira Kitajima.

;; Author: Kaoru Maeda <maeda@src.ricoh.co.jp>
;;	Yasushi Saito <yasushi@is.s.u-tokyo.ac.jp>
;;	Akira Kitajima <kitajima@isc.osakac.ac.jp>
;; Maintainer: Akira Kitajima
;; Version: $Id: tc-sysdep.el,v 1.25 2003/03/21 03:39:39 kitajima Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.

;;; Code:
(require 'tc-pre)

(when (or (not (boundp 'emacs-major-version))
	  (< emacs-major-version 19))
  (error "Old Emacs? Emacs-19 and later is supported."))

;;;
;;; Define some new functions that are not present in Emacs 18.
;;;

;;; `unless' and `when' are from subr.el of emacs-20.2.
(or (fboundp 'unless)
    (progn
      (defmacro unless (cond &rest body)
	"(unless COND BODY...): if COND yields nil, do BODY, else return nil."
	(cons 'if (cons cond (cons nil body))))
      (put 'unless 'lisp-indent-function 1)
      (put 'unless 'edebug-form-spec '(&rest form))))

(unless (fboundp 'when)
  (defmacro when (cond &rest body)
    "(when COND BODY...): if COND yields non-nil, do BODY, else return nil."
    (list 'if cond (cons 'progn body)))
  (put 'when 'lisp-indent-function 1)
  (put 'when 'edebug-form-spec '(&rest form)))

(unless (fboundp 'add-hook)
  (defun add-hook (hook fun)
    "Add to the value of HOOK the function FUNCTION.
FUNCTION is added at the beginning of the hook list.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
    (if (not (boundp hook)) (set hook nil))
    (let ((cur-value (eval hook)))
      (cond ((or (equal fun cur-value)
		 (and (consp cur-value)
		      (member fun cur-value)))
	     nil)		       ; already defined
	    ((null cur-value)
	     (set hook fun))
	    ((or (not (consp cur-value))
		 (eq (car cur-value) 'lambda))
	     (set hook (cons fun (list cur-value))))
	    (t
	     (set hook (nconc (list fun) cur-value)))))))

(unless (fboundp 'cancel-undo-boundary)
  (if (boundp 'buffer-undo-list)
      ;; for Emacs 20 or later
      (defun cancel-undo-boundary ()
	(and (listp buffer-undo-list)
	     (null (car buffer-undo-list))
	     (setq buffer-undo-list (cdr buffer-undo-list))))
    (defun cancel-undo-boundary ()
  ;; this function is just a dummy for emacs version 18,
      ;; and it actually does nothing
      nil)))

(unless (fboundp 'undo-boundary)
  (defun undo-boundary ()
  ;; this function is just a dummy for emacs version 18,
    ;; and it actually does nothing
    nil))

(unless (fboundp 'chars-in-string)
  (defun chars-in-string (s)
    (length s)))

(unless (fboundp 'japanese-hiragana)
  (defun japanese-hiragana (char)
    "文字 CHAR がカタカナならひらがなに変換する。
カタカナでない場合はそのままの値を返す。"
    (let ((str (char-to-string char)))
      (if (string-match (concat "^[ァ-ン]$") str)
	  (if (tcode-nemacs-p)
	      (let ((ch (mod char 256)))
		(+ (* ?\244 256) ch))
	    (let ((ch (char-component char 2)))
	      (make-character lc-jp ?\244 ch)))
	char))))

(unless (fboundp 'japanese-katakana)
  (defun japanese-katakana (char)
    "文字 CHAR がひらがなならカタカナに変換する。
ひらがなでない場合はそのままの値を返す。"
    (let ((str (char-to-string char)))
      (if (string-match (concat "^[ぁ-ん]$") str)
	  (if (tcode-nemacs-p)
	      (let ((ch (mod char 256)))
		(+ (* ?\245 256) ch))
	    (let ((ch (char-component char 2)))
	      (make-character lc-jp ?\245 ch)))
	char))))

(defconst tcode-jisx0208 (if (tcode-mule-2-p)
			     lc-jp
			   'japanese-jisx0208))

(unless (fboundp 'make-char)
  (if (fboundp 'make-character)
      (defun make-char (charset &optional code1 code2)
	(make-character (cond ((eq charset 'japanese-jisx0208)
			       lc-jp)
			      (t 
			       charset))
			code1
			code2))
    ;; nemacs
    (defun make-char (charset &optional code1 code2)
      (tcode-string-to-char (format "%c%c" code1 code2)))))

(unless (fboundp 'delete-text-in-column)
    ;;; from mule-util.el
  (defun delete-text-in-column (from to)
    "Delete the text between column FROM and TO (exclusive) of the current line.
Nil of FORM or TO means the current column.
If there's a charcter across the borders, the character is replaced with
the same width of spaces before deleting."
    (save-excursion
      (let (p1 p2)
	(if from
	    (progn
	      (setq p1 (move-to-column from))
	      (if (> p1 from)
		  (progn
		    (delete-char -1)
		    (insert-char ?  (- p1 (current-column)))
		    (forward-char (- from p1))))))
	(setq p1 (point))
	(if to
	    (progn
	      (setq p2 (move-to-column to))
	      (if (> p2 to)
		  (progn
		    (delete-char -1)
		    (insert-char ?  (- p2 (current-column)))
		    (forward-char (- to p2))))))
	(setq p2 (point))
	(delete-region p1 p2)))))

(unless (fboundp 'member)
  (defun member (elt list)
    "Return non-nil if ELT is an element of LIST.  \
Comparison done with `equal'.
The value is actually the tail of LIST whose car is ELT."
    (catch 'found
      (while list
	(and (equal elt (car list))
	     (throw 'found list))
	(setq list (cdr list))))))

(unless (fboundp 'delete)
  (defun delete (elt seq)
    (cond ((listp seq)
	   (let (deleted)
	     (while seq
	       (or (equal (car seq) elt)
		   (setq deleted (nconc deleted (cons (car seq) nil))))
	       (setq seq (cdr seq)))
	     deleted))
	  ((stringp seq)
	   (mapconcat 'char-to-string (delete elt (mapcar 'identity seq)) nil))
	  ((vectorp seq)
	   (vconcat (delete elt (mapcar 'identity seq))))
	  (t
	   seq))))

(unless (fboundp 'frame-width)
  (defmacro frame-width () (list 'screen-width)))

(unless (fboundp 'abs)
  (defun abs (arg)
    "Return the absolute value of ARG."
    (if (>= arg 0) arg (- arg))))

(unless (fboundp 'lambda)
  ;; from subr.el in Emacs 21.1
  (defmacro lambda (&rest cdr)
    "Return a lambda expression.
A call of the form (lambda ARGS DOCSTRING INTERACTIVE BODY) is
self-quoting; the result of evaluating the lambda expression is the
expression itself.  The lambda expression may then be treated as a
function, i.e., stored as the function value of a symbol, passed to
funcall or mapcar, etc.

ARGS should take the same form as an argument list for a `defun'.
DOCSTRING is an optional documentation string.
 If present, it should describe how to call the function.
 But documentation strings are usually not useful in nameless functions.
INTERACTIVE should be a call to the function `interactive', which see.
It may also be omitted.
BODY should be a list of lisp expressions."
;; Note that this definition should not use backquotes; subr.el should not
    ;; depend on backquote.el.
    (list 'function (cons 'lambda cdr))))

(unless (fboundp 'window-minibuffer-p)
  (defun window-minibuffer-p (&optional window)
    (string-match " \\*Mini" (buffer-name (window-buffer window)))))

(unless (fboundp 'buffer-substring-no-properties)
  (defun buffer-substring-no-properties (start end)
    (buffer-substring start end)))

(defun tcode-redo-command (ch)
  "キー CH を現在のキーマップで再実行する"
  (setq unread-command-events
	(cons (character-to-event ch) unread-command-events)))
(or (fboundp 'character-to-event)
    (defun character-to-event (ch)
      ch))
;; XEmacs
(or (fboundp 'isearch-last-command-char)
    (defun isearch-last-command-char ()
      last-command-event))
(or (boundp 'search-upper-case)
    (setq search-upper-case 'not-yanks))

(if (not (tcode-nemacs-p))
    (progn
      (defun tcode-string-to-char (p) (string-to-char p))
      (defmacro tcode-following-char () (list 'following-char))
      (defmacro tcode-preceding-char () (list 'preceding-char))
      (defmacro tcode-forward-char (p) (list 'forward-char p))
      (defmacro tcode-backward-char (p) (list 'backward-char p))
      (or (fboundp 'string-to-list)
	  (defun string-to-list (s)
	    (string-to-char-list s)))
      (defmacro tcode-delete-char (p) (list 'delete-char p)))
  ;;;
  ;;; NEmacs 用の定義
  ;;; 以下の関数では，2バイト文字を数値に変換すると，
  ;;; (1バイト目*256 + 2バイト目)になる。
  ;;;
  (defun char-width (c)
    (if (> c 256) 2 1))
  (unless (fboundp 'orig:char-to-string)
    (fset 'orig:char-to-string (symbol-function 'char-to-string))
    (defun char-to-string (c)
      (if (> c 256)
	  (format "%c%c" (/ c 256) (mod c 256))
	(orig:char-to-string c))))
  (defun tcode-string-to-char (s)
    (let ((1st (aref s 0)))
      (if (>= 1st 128)
          (+ (* 1st 256) (aref s 1))
        1st)))
  (defun tcode-following-char ()
    (let ((1st (following-char)))
      (if (and 1st
	       (>= 1st 128))
          (+ (* 1st 256) (char-after (1+ (point))))
        1st)))
  (defun tcode-preceding-char ()
    (let ((1st (char-after (save-excursion (backward-char 1) (point)))))
      (if (and 1st
	       (>= 1st 128))
	  (+ (* 1st 256) (preceding-char))
	(preceding-char))))
  (defun string-to-list (s)
    (kanji-word-list s))
  (defun tcode-forward-char (i)
    (if (< i 0)
	(while (< i 0)
	  (if (>= (preceding-char) 128) (backward-char 2)
	    (backward-char 1))
	  (setq i (1+ i)))
      (while (> i 0)
	(if (>= (char-after (point)) 128) (forward-char 2)
	  (forward-char 1))
	(setq i (1- i)))))
  (defmacro tcode-backward-char (i)
    (list 'tcode-forward-char (list '- 0 i)))

  (defun tcode-delete-char (i)
    (if (< i 0)
	(while (< i 0)
	  (if (>= (preceding-char) 128)
	      (delete-char -2)
	    (delete-char -1))
	  (setq i (1+ i)))
      (while (> i 0)
	(if (>= (char-after (point)) 128)
	    (delete-char 2)
	  (delete-char 1))
	(setq i (1- i)))))

  (defmacro string-width (p) (list 'length p)))

(defun tcode-do-auto-fill ()
  (cond ((boundp 'auto-fill-function)
	 ;; emacs-19
	 (and auto-fill-function
	      (> (current-column) fill-column)
	      (funcall auto-fill-function)))
	(t
	 ;; emacs-18
	 (and auto-fill-hook
	      (> (current-column) fill-column)
	      (do-auto-fill)))))

(unless (fboundp 'buffer-disable-undo)
  (defun buffer-disable-undo ()
    (setq disable-undo t)))

(unless (fboundp 'string)
  (defun string (&rest characters)
    "Concatenate all the argument characters and make the result a string."
    (mapconcat 'char-to-string characters nil)))

;;; support custom package

(or (fboundp 'defgroup)
    (defmacro defgroup (&rest args)))

(or (fboundp 'defcustom)
    (defmacro defcustom (symbol value doc &rest args)
      (list 'defvar symbol value doc)))

(defgroup input-methods nil
  "Input methods."
  :group 'mule)

(defgroup tcode nil
  "T-Code (Kanji direct input method)."
  :group 'input-methods)

;;; from mule-cmds.el of Emacs 20.0.91 for earlier version

(unless (boundp 'input-method-activate-hook)
  (defvar input-method-activate-hook nil
    "Normal hook run just after an input method is activated.")

  (defvar input-method-inactivate-hook nil
    "Normal hook run just after an input method is inactivated.")

  (defvar input-method-after-insert-chunk-hook nil
    "Normal hook run just after an input method insert some chunk of text."))


;;; from mule-cmds.el of Emacs 20.3 for earlier version

(unless (boundp 'input-method-verbose-flag)
  (defcustom input-method-verbose-flag 'default
    "*A flag to control extra guidance given by input methods.
The value should be nil, t, `complex-only', or `default'.

The extra guidance is done by showing list of available keys in echo
area.  When you use the input method in the minibuffer, the guidance
is shown at the bottom short window (split from the existing window).

If the value is t, extra guidance is always given, if the value is
nil, extra guidance is always suppressed.

If the value is `complex-only', only complex input methods such as
`chinese-py' and `japanese' give extra guidance.

If the value is `default', complex input methods always give extra
guidance, but simple input methods give it only when you are not in
the minibuffer.

See also the variable `input-method-highlight-flag'."
    :type '(choice (const t) (const nil) (const complex-only) (const default))
    :group 'mule)

  (defcustom input-method-highlight-flag t
    "*If this flag is non-nil, input methods highlight partially-entered text.
For instance, while you are in the middle of a Quail input method sequence,
the text inserted so far is temporarily underlined.
The underlining goes away when you finish or abort the input method sequence.
See also the variable `input-method-verbose-flag'."
    :type 'boolean
    :group 'mule))

;;; mode-line

(defun tcode-find-symbol-in-tree (item tree)
  "ITEM を任意の構造のリスト TREE からequalで探す。
見つかるとt, 見つからなければ nil。"
  (if (consp tree)
      (or (tcode-find-symbol-in-tree item (car tree))
	  (tcode-find-symbol-in-tree item (cdr tree)))
    (equal item tree)))

;;; set mode-line-format
(or (and (boundp 'tcode-mode-indicator)
	 (tcode-find-symbol-in-tree 'tcode-mode-indicator
				    (default-value 'mode-line-format)))
    (cond ((or (tcode-nemacs-p)
	       (tcode-xemacs-p))
	   
	   (setq-default mode-line-format
			 (cons '(tcode-ready-in-this-buffer
				 ("[" tcode-mode-indicator "]"))
			       (default-value 'mode-line-format)))
	   (setq mode-line-format (default-value 'mode-line-format)))
	  ((or (tcode-mule-1-p)
	       (tcode-mule-2-p))
	   (setq-default mode-line-format
			 (cons '(mc-flag ("[" tcode-mode-indicator "]"))
			       (default-value 'mode-line-format))))
	  ((tcode-mule-3-p)
	   (setq-default mode-line-mule-info
			 '(enable-multibyte-characters
			   ((current-input-method-title
			     ("[" current-input-method-title "]"))
			    "%Z"))))
	  ((tcode-mule-4-p)
	   (setq-default mode-line-mule-info
			 '("" (current-input-method-title
			       ("[" current-input-method-title "]")) "%Z")))))

(defvar tcode-mode-indicator 'current-input-method-title)
(unless (boundp 'current-input-method-title)
  (defvar current-input-method-title "--")
  (make-variable-buffer-local 'current-input-method-title)
  (setq-default current-input-method-title "--"))

;;; for toggle-input-method

;; modified from mule-cmds.el in Emacs 21.2

(unless (fboundp 'register-input-method)
  (defvar input-method-alist nil)
  (defun register-input-method (input-method lang-env &rest args)
    "Register INPUT-METHOD as an input method for language environment LANG-ENV.
INPUT-METHOD and LANG-ENV are symbols or strings.

The remaining arguments are:
	ACTIVATE-FUNC, TITLE, DESCRIPTION, and ARGS...
ACTIVATE-FUNC is a function to call to activate this method.
TITLE is a string to show in the mode line when this method is active.
DESCRIPTION is a string describing this method and what it is good for.
The ARGS, if any, are passed as arguments to ACTIVATE-FUNC.
All told, the arguments to ACTIVATE-FUNC are INPUT-METHOD and the ARGS.

This function is mainly used in the file \"leim-list.el\" which is
created at Emacs build time, registering all Quail input methods
contained in the Emacs distribution.

In case you want to register a new Quail input method by yourself, be
careful to use the same input method title as given in the third
parameter of `quail-define-package'.  (If the values are different, the
string specified in this function takes precedence.)

The commands `describe-input-method' and `list-input-methods' need
these duplicated values to show some information about input methods
without loading the relevant Quail packages."
    (if (symbolp lang-env)
	(setq lang-env (symbol-name lang-env)))
    (if (symbolp input-method)
	(setq input-method (symbol-name input-method)))
    (let ((info (cons lang-env args))
	  (slot (assoc input-method input-method-alist)))
      (if slot
	  (setcdr slot info)
	(setq slot (cons input-method info))
	(setq input-method-alist (cons slot input-method-alist))))))

(unless (fboundp 'inactivate-input-method)
  (defun inactivate-input-method ()
    (if inactivate-current-input-method-function
	(funcall inactivate-current-input-method-function))
    (setq current-input-method nil)))

(unless (fboundp 'activate-input-method)
  (defvar default-input-method nil)
  (make-variable-buffer-local 'default-input-method)
  (defvar current-input-method nil)
  (make-variable-buffer-local 'current-input-method)
  (defvar inactivate-current-input-method-function nil)
  (make-variable-buffer-local 'inactivate-current-input-method-function)

  (defun activate-input-method (input-method)
    (if (and current-input-method
	     (not (string= current-input-method input-method)))
	(inactivate-input-method))
    (let* ((slot (assoc input-method input-method-alist)))
      (apply (nth 2 slot) input-method (nthcdr 5 slot))
      (setq current-input-method input-method))))

(when (or (memq tcode-emacs-version '(nemacs mule-1 mule-2))
	  (and (eq tcode-emacs-version 'xemacs)
	       (< emacs-major-version 21)
	       (< emacs-minor-version 3)))
  (defun toggle-input-method (&optional arg)
    (interactive "P")
    (if (and current-input-method (not arg))
	(inactivate-input-method)
      (if (null default-input-method)
	  (let* ((completion-ignore-case t)
		 (im (completing-read "Input method: " 
				      tcode-package-name-alist)))
	    (activate-input-method im))
	(if arg
	    (let* ((completion-ignore-case t)
		   (im (completing-read
			(format "Input method (default %s): " 
				default-input-method)
			tcode-package-name-alist))
		   (input-method (if (or (null im)
					 (string= im ""))
				     default-input-method
				   im)))
	      (activate-input-method input-method))
	  (activate-input-method default-input-method))))))


(provide 'tc-sysdep)

;;; tc-sysdep.el ends here
