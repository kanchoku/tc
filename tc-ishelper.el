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

;;;
;;;  User Variables
;;;
(defvar tcode-isearch-start-state nil
  "*インクリメンタルサーチ開始時のTコードモードを指定する。
       nil: バッファのTコードモードを引き継ぐ(デフォールト)。
       0:   開始時、Tコードモードをオフにする。
       1:   開始時、Tコードモードをオンにする。
バッファローカル変数。")
(make-variable-buffer-local 'tcode-isearch-start-state)
(setq-default tcode-isearch-start-state nil)

(defcustom tcode-isearch-enable-wrapped-search t
  "*2バイト文字でサーチするときに、空白や改行を無視する。"
  :type 'boolean :group 'tcode)

(defcustom tcode-isearch-ignore-regexp "[\n \t]*"
  "*2バイト文字間に入る正規表現。
`tcode-isearch-enable-wrapped-search' が t のときのみ有効。"
  :type 'regexp :group 'tcode)

;;;
;;; advice による isearch 拡張の実装
;;;

(declare-function tcode-isearch-bushu-henkan   "tc-isbushumaze")
(declare-function tcode-isearch-process-string "tc-isbushumaze")
(declare-function tcode-isearch-bushu          "tc-isbushumaze")
(declare-function tcode-function-p    "tc")
(declare-function tcode-apply-filters "tc")
(declare-function tcode-decode-chars  "tc")
(defvar tcode-isearch-special-function-alist)

(defun tcode--advice-isearch-init ()
  "tc-ishelper.el のロード時に初期化を行なう。"
  (require 'tc-isbushumaze)
  (advice-add 'isearch-printing-char :around #'tcode--isearch-printing-char))

(defun tcode--isearch-printing-char (orig-fun &optional char count)
  "isearch 中に文字キーが押されると呼ばれる。必要に応じて T-Code 実装を呼ぶ。"
  (if (bound-and-true-p tcode-mode)
      (tcode--input-method-for-isearch char count)
    (funcall orig-fun char count)))

;; tc-is22.el の isearch-printing-char の追加部分を抜き出したもの。
(defun tcode--input-method-for-isearch (char count)
  "isearch 中の日本語入力。tcode-input-method の簡易版。"
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
(defun tcode-isearch-make-string-for-wrapping (s) s)

;;;
;;; wrapped-search
;;;

(defun tcode--wrapped-search-regexp (string &optional lax)
  "STRING または、STRING の日本語文字の前に行折り返しなどの空白が狭ま
れた文字列にマッチする正規表現を返す。"
  (mapconcat (lambda (ch)
	       (let ((s-ch (char-to-string ch)))
		 (cond ((= (char-width ch) 2)
			(concat tcode-isearch-ignore-regexp s-ch))
		       (t
			(regexp-quote s-ch)))))
	     (string-to-list string)
	     nil))

;;;
;;; isearch-toggle-wrap の定義
;;;

;; emacs-24ではisearch-regexp-functionが無いので、このファイルの実装方
;; 法では wrapped-search を提供できない。
(defconst tcode--has-wrapped-search (fboundp 'isearch-define-mode-toggle)
  "wrapped-search を実装可能な emacs バージョンかどうか。")

(unless tcode--has-wrapped-search
  ;; emacs-24 用ダミー実装。
  (defmacro isearch-define-mode-toggle (&rest args)))

(require 'cl-macs)
;; isearch 内のコマンド isearch-toggle-wrap を作り、"M-s @" にバインド
;; する。
;;  - FIXME: キーバインドは特に必要ないが、このマクロの仕様上スキップ
;;    できない。将来の emacs 本体によるバインディングと衝突しないキー
;;    を選びたい。衝突時にキー変更することを考えると、ユーザーが使用し
;;    ないよう、すぐにバインドを消すべきか。
;;  - ???: このマクロはトップレベルに置かないとエラー。emacs -qでこの
;;    ファイルだけをロードした場合に発生。原因不明。
(isearch-define-mode-toggle wrap "@" tcode--wrapped-search-regexp
  "Turning on wrap search turns off regexp mode.")

;;;
;;; モード設定
;;;

(defun tcode-isearch-set-im-mode (enable)
  "isearch 中に、input method の有効化/無効化を行う。"
  (when (or (and enable (null current-input-method))
	    (and (not enable) current-input-method))
    (isearch-toggle-input-method)))

(defun tcode-isearch-init ()
  "isearch 開始時、input method の状態をセットする。"
  (when (numberp tcode-isearch-start-state)
    (tcode-isearch-set-im-mode (not (zerop tcode-isearch-start-state)))))

(defun tcode--wrapped-search-init-state ()
  "isearch モード開始時、必要に応じて wrapped-search を有効にする。"
  (when (and isearch-mode
	     tcode-isearch-enable-wrapped-search
	     (boundp 'isearch-regexp-function) ; この2行は emacs-24 での
	     (fboundp 'isearch-toggle-wrap)    ; compiler warning 対策。
	     (not isearch-regexp)            ; regexp search でないとき
	     (null isearch-regexp-function)) ; word/symbol search でないとき
    (isearch-toggle-wrap)))

;;;
;;; 初期化
;;;

(defvar tcode-use-isearch)

(defun tcode--ishelper-init ()
  "tc-ishelper.el のロード時に初期化を行なう。"
  (when (eq tcode-use-isearch :advice)
    (tcode--advice-isearch-init))
  (add-hook 'isearch-mode-hook #'tcode-isearch-init)
  (when tcode--has-wrapped-search
    (add-hook 'isearch-mode-hook #'tcode--wrapped-search-init-state)))

(tcode--ishelper-init)
(provide 'tc-ishelper)
