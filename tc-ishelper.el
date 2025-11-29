;;; tc-ishelper.el --- T-Code isearch supports.  -*- lexical-binding: nil -*-

;; Copyright (C) 2025 Github kanchoku/tc contributors

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;   T-Code 用の isearch 拡張機能。ts-is22.el を置き換えるモジュール。

;;; Code:

(require 'tc-iscommon)

;;;
;;; advice による isearch 拡張の実装
;;;

(declare-function tcode-function-p    "tc")
(declare-function tcode-apply-filters "tc")
(declare-function tcode-decode-chars  "tc")

(defun tcode--isearch-printing-char (orig-fun &optional char count)
  "isearch 中に文字キーが押されると呼ばれる。必要に応じて T-Code 実装を呼ぶ。"
  (if (bound-and-true-p tcode-mode)
      (tcode--input-method-for-isearch char count)
    (funcall orig-fun char count)))

;; tc-is22.el の isearch-printing-char の追加部分を抜き出したもの。
(defun tcode--input-method-for-isearch (char count)
  "isearch 中の日本語入力。`tcode-input-method'の簡易版。"
  (let* ((decoded (tcode-decode-chars char))
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
	   (ding)))))

;; この関数の役割は tcode--wrapped-search-regexp に移った。
(defalias 'tcode-isearch-make-string-for-wrapping #'identity)

;;;
;;; 初期化
;;;

(defvar tcode-use-isearch)

(defun tcode--ishelper-init ()
  "tc-ishelper.el のロード時に初期化を行なう。"
  (when (eq tcode-use-isearch 'advice)
    (advice-add 'isearch-printing-char :around #'tcode--isearch-printing-char))
  (add-hook 'isearch-mode-hook #'tcode-isearch-init))

(tcode--ishelper-init)
(provide 'tc-ishelper)
