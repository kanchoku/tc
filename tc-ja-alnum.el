;;; tc-ja-alnum.el --- a Japanese 2byte alphabet and number input method

;; Copyright (C) 2002 KITAJIMA Akira.

;; Author: KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;; Maintainer: KITAJIMA Akira
;; Keyword: input method
;; Create: 16 December, 2002

;; $Id: tc-ja-alnum.el,v 2.1 2002/12/18 02:38:57 kitajima Exp $

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

;;; Commentary:

;; Do you *really* need this package?  :-P

;;; Code:

(require 'tc)

(defvar tcode-2byte-alnum-mode nil)
(make-variable-buffer-local 'tcode-2byte-alnum-mode)

(defun tcode-2byte-alnum-inactivate ()
  "Tコード2バイト英数モードを無効にする。"
  (tcode-2byte-alnum-activate -1))

(defun tcode-2byte-alnum-mode-line-redisplay ()
  (let ((slot (assoc current-input-method input-method-alist)))
    (setq current-input-method-title (if tcode-2byte-alnum-mode
					 (nth 3 slot)
				       tcode-transparent-mode-indicator)))
  (set-buffer-modified-p (buffer-modified-p)))

(defun tcode-2byte-alnum-activate (&optional arg)
  "Tコード2バイト英数モードを有効にする。ARGが負の整数のときは無効にする。"
  (if (and arg
	   (< (prefix-numeric-value arg) 0))
      ;; inactivate T-Code mode
      (unwind-protect
	  (progn
	    (setq tcode-2byte-alnum-mode nil)
	    (run-hooks 'input-method-deactivate-hook))
	(setq input-method-function nil))
    ;; activate T-Code mode
    (setq tcode-2byte-alnum-mode t)
    (run-hooks 'input-method-activate-hook)
    (when (boundp 'input-method-function)
      (set (make-local-variable 'input-method-function)
	   'tcode-2byte-alnum-input-method)))
  (tcode-2byte-alnum-mode-line-redisplay))

;;;###autoload
(defun tcode-use-2byte-alnum (package-name &rest libraries)
  (setq deactivate-current-input-method-function 'tcode-2byte-alnum-inactivate
	describe-current-input-method-function nil
	current-input-method package-name)
  (tcode-2byte-alnum-mode 1))

(defun tcode-2byte-alnum-mode (&optional arg)
  (interactive "P")
  (tcode-2byte-alnum-activate (or arg
				  (if tcode-2byte-alnum-mode -1 1)))
  (tcode-2byte-alnum-mode-line-redisplay))

(defun tcode-2byte-alnum-input-method (ch)
  "The 2byte alnum input function for T-Code."
  (setq last-command 'self-insert-command
	last-command-event ch)
  (list (tcode-1-to-2 ch)))

(unless (boundp 'input-method-function)
  (defun tcode-2byte-alnum-self-insert-command (&optional arg)
    "Translate character and insert."
    (interactive "P")
    (let ((ch (car (tcode-2byte-alnum-input-method last-command-event))))
      (insert (char-to-string ch))))

  ;; マイナーモード
  (unless (assq 'tcode-2byte-alnum-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons (list 'tcode-2byte-alnum-mode nil) minor-mode-alist)))

  (defvar tcode-2byte-alnum-mode-map nil)

  (if (boundp 'minor-mode-map-alist)
      (progn
	;; 自動的にマイナーモードキーマップに変更
	(unless tcode-2byte-alnum-mode-map
	  (setq tcode-2byte-alnum-mode-map (make-keymap))
	  (let ((c ? ))
	    (while (< c ?~)
	      (define-key tcode-2byte-alnum-mode-map 
		(char-to-string c)
		'tcode-2byte-alnum-self-insert-command)
	      (setq c (1+ c)))))

	(or (assq 'tcode-2byte-alnum-mode minor-mode-map-alist)
	    (setq minor-mode-map-alist
		  (cons (cons 'tcode-2byte-alnum-mode 
			      tcode-2byte-alnum-mode-map)
			minor-mode-map-alist))))
    ;; 自前でマイナーモードキーマップに変更
    (make-variable-buffer-local 'tcode-2byte-alnum-mode-map)
    (defvar tcode-2byte-alnum-original-local-map nil)
    (make-variable-buffer-local 'tcode-2byte-alnum-original-local-map)

    (unless (fboundp 'tcode:tcode-2byte-alnum-activate)
      (fset 'tcode:tcode-2byte-alnum-activate
	    (symbol-function 'tcode-2byte-alnum-activate))
      (defun tcode-2byte-alnum-activate (&optional arg)
	(or tcode-2byte-alnum-mode
	    (setq tcode-2byte-alnum-original-local-map
		  (or (current-local-map)
		      (current-global-map))))
	(unless tcode-2byte-alnum-mode-map
	  (setq tcode-2byte-alnum-mode-map
		(if tcode-2byte-alnum-original-local-map
		    (copy-keymap tcode-2byte-alnum-original-local-map)
		  (make-sparse-keymap)))
	  (let ((i ? ))
	    (while (< i ?~)
	      (define-key tcode-2byte-alnum-mode-map
		(char-to-string i)
		'tcode-2byte-alnum-self-insert-command)
	      (setq i (1+ i)))))
	(tcode:tcode-2byte-alnum-activate arg)
	(use-local-map (if tcode-2byte-alnum-mode
			   tcode-2byte-alnum-mode-map
			 (or tcode-2byte-alnum-original-local-map
			     (current-local-map)
			     (current-global-map))))))))

(register-input-method "japanese-2byte-alnum"
		       "Japanese"
		       'tcode-use-2byte-alnum
		       "Ａ"
		       "a Japanese 2byte alphabet and number input method")

(provide 'tc-ja-alnum)

;;; tc-ja-alnum.el ends here
