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

(eval-when-compile
  (require 'cl-macs))  ; for cl-callf used in isearch-define-mode-toggle
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
;;; wrapped-search
;;;

(defun tcode--wrapped-search-regexp (string &optional lax)
  "STRING または、STRING の日本語文字の前に行折り返しなどの空白が挟ま
れた文字列にマッチする正規表現を返す。"
  (mapconcat (lambda (ch)
	       (let ((s-ch (char-to-string ch)))
		 (cond ((= (char-width ch) 2)
			(concat tcode-isearch-ignore-regexp s-ch))
		       (t
			(regexp-quote s-ch)))))
	     (string-to-list string)
	     nil))

(defun tcode--define-toggle-wrap ()
  "isearch-toggle-wrap を定義する。"
  (let ((orig-binding (lookup-key isearch-mode-map (kbd "M-s @"))))
    ;; isearch-toggle-wrap が定義されることをバイトコンパイラに知らせ
    ;; るために eval-and-compile が必要。
    (eval-and-compile
      ;; isearch 内のコマンド isearch-toggle-wrap を作り、"M-s @" にバイ
      ;; ンドする。キーバインドは特に必要ないが、このマクロの仕様上スキッ
      ;; プできない。一時的にバインドしてすぐに元に戻す。
      (isearch-define-mode-toggle wrap "@" tcode--wrapped-search-regexp
	"Turning on wrap search turns off regexp mode."))
    (define-key isearch-mode-map (kbd "M-s @") orig-binding)))

(defun tcode--wrapped-search-init-state ()
  "isearch 開始時、必要に応じて wrapped-search を有効にする。"
  (when (and isearch-mode
	     tcode-isearch-enable-wrapped-search
	     (not isearch-regexp)            ; regexp search でないとき
	     (null isearch-regexp-function)) ; word/symbol search でないとき
    (isearch-toggle-wrap)))

;;;
;;; 初期化
;;;

(defvar tcode-use-isearch)

(defun tcode--ishelper-init ()
  "tc-ishelper.el のロード時に初期化を行なう。"
  (tcode--define-toggle-wrap)
  (when (eq tcode-use-isearch 'advice)
    (advice-add 'isearch-printing-char :around #'tcode--isearch-printing-char))
  (add-hook 'isearch-mode-hook #'tcode-isearch-init)
  (add-hook 'isearch-mode-hook #'tcode--wrapped-search-init-state))

(tcode--ishelper-init)
(provide 'tc-ishelper)
