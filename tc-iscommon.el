;;; tc-iscommon.el --- Excerpts from tc-is22.el. -*- lexical-binding: nil -*-

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

;;; Commentary:

;;   tc-is22.el から、emacs 本体の関数の置き換えを行わない関数を抜き出
;;   したもの。

;;; Code:

(declare-function tcode-bushu-init                       "tc")
(declare-function tcode-activate                         "tc")
(declare-function tcode-isearch-make-string-for-wrapping "tc-ishelper")
(declare-function tcode-bushu-compose-two-chars          "tc-bushu")
(declare-function tcode-mazegaki-begin-conversion        "tc-mazegaki")
(declare-function tcode-mazegaki-put-prefix              "tc-mazegaki")

(defvar tcode-mode)
(defvar tcode-use-postfix-bushu-as-default)

;;;
;;;  User Variables
;;;
(defvar-local tcode-isearch-start-state nil
  "*インクリメンタルサーチ開始時のTコードモードを指定する。
       nil: バッファのTコードモードを引き継ぐ(デフォルト)。
       0:   開始時、Tコードモードをオフにする。
       1:   開始時、Tコードモードをオンにする。
バッファローカル変数。")

(defcustom tcode-isearch-enable-wrapped-search t
  "*2バイト文字でサーチするときに、空白や改行を無視する。"
  :type 'boolean :group 'tcode)

(defcustom tcode-isearch-ignore-regexp "[\n \t]*"
  "* 2バイト文字間に入る正規表現。
`tcode-isearch-enable-wrapped-search' が t のときのみ有効。"
  :type 'regexp :group 'tcode)

;;;
;;; Implementations
;;;

(defcustom tcode-isearch-special-function-alist
  '((tcode-bushu-begin-conversion . tcode-isearch-bushu-conversion-command)
    (tcode-bushu-begin-alternate-conversion
     . tcode-isearch-bushu-alternate-conversion-command)
    (tcode-mazegaki-begin-alternate-conversion . tcode-isearch-prefix-mazegaki)
    (tcode-mazegaki-begin-conversion . tcode-isearch-postfix-mazegaki)
    (tcode-toggle-alnum-mode))
  "*isearch中での特殊なコマンドの入力に対する代替コマンドの alist。"
  :group 'tcode)

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


(defun tcode-isearch-set-im-mode (enable)
  "isearch 中に、input method の有効化/無効化を行う。"
  (when (or (and enable (null current-input-method))
	    (and (not enable) current-input-method))
    (isearch-toggle-input-method)))

(defun tcode-isearch-init ()
  "isearch 開始時、input method の状態をセットする。"
  (when (numberp tcode-isearch-start-state)
    (tcode-isearch-set-im-mode (not (zerop tcode-isearch-start-state)))))

(provide 'tc-iscommon)
