;;; tc-mazegaki.el --- mazegaki conversion in T-Code

;; Copyright (C) 1996--2003 Kaoru Maeda, Yasushi Saito and KITAJIMA Akira.

;; Author: Kaoru Maeda <maeda@src.ricoh.co.jp>
;;	Yasushi Saito <yasushi@is.s.u-tokyo.ac.jp>
;;	KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;; Maintainer: KITAJIMA Akira
;; Created: 30 Apr 1996
;; Version: $Id: tc-mazegaki.el,v 1.38 2003/05/18 08:46:08 kitajima Exp $
;; Keywords: wp, japanese, input method

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

(require 'tc)

(defgroup mazegaki-conversion nil
  "交ぜ書き変換"
  :group 'tcode)

;;; カスタマイズできる変数

(defvar tcode-mazegaki-selected-candidate-register ?\[
  "* 交ぜ書き変換で最後に確定した文字列を保存しておくレジスタ。
nil の場合には保存されない。")

(defvar tcode-mazegaki-dictionary-name "mazegaki.dic"
  "交ぜ書き変換辞書のファイル名。")

(defconst tcode-mazegaki-buffer-name " *tcode: mazegaki dictionary*")
;; 交ぜ書き辞書のバッファ名

(unless (assq tcode-mazegaki-buffer-name tcode-dictionaries)
  (setq tcode-dictionaries (cons (cons tcode-mazegaki-buffer-name
				       tcode-mazegaki-dictionary-name)
				 tcode-dictionaries)))

(defcustom tcode-mazegaki-yomi-max 10 "* 交ぜ書き変換の読みの最大文字数。"
  :group 'mazegaki-conversion)

(defvar tcode-mazegaki-terminate-char-list
  (mapcar (lambda (ch) (tcode-string-to-char ch))
	  '("、" "。" "，" "．" "・" "「" "」" "（" "）"))
  "* 交ぜ書き変換の読みに含まれない2バイト文字のリスト。")

(defcustom tcode-mazegaki-init-hook nil
  "* 最初に tc-mazegaki.el をロードするときに呼ばれる hook。"
  :type 'hook :group 'mazegaki-conversion)

(defvar tcode-mazegaki-command-summary-alist
  '(("縮める" . tcode-mazegaki-relimit-left)
    ("伸ばす" . tcode-mazegaki-relimit-right)
    ("確定"   . tcode-mazegaki-finish)
    ("戻す"   . tcode-mazegaki-restore-yomi-and-quit)
    ("一覧"   . tcode-mazegaki-table-mode)
    ("次表または縮め" . tcode-mazegaki-select-candidate-or-relimit)
    ("登録&確定" . tcode-mazegaki-make-entry-and-finish))
  "* `tcode-mazegaki-command-summary' で表示される機能の alist。")

(defvar tcode-mazegaki-enable-inflection t
  "* nil でないとき、可変語尾変換ができる。")

(defvar tcode-mazegaki-prefix-mark
  (if (or (tcode-mule-1-p)
	  (and (boundp 'window-system)
	       (not window-system)))
      (if (fboundp 'make-glyph)
	  (make-glyph "△")
	"△")
    nil)
  "* 交ぜ書き変換の始点を表すしるし。")

(defvar tcode-mazegaki-face
  (if (or (tcode-mule-1-p)
	  (and (boundp 'window-system)
	       (not window-system)))
      nil
    (prog1
	(make-face 'mazegaki-conversion)
      (set-face-underline 'mazegaki-conversion t)))
  "* 交ぜ書き変換の変換対象を表す文字列に用いるface。
mule2 以上または XEmacs の場合のみ有効。")
(defvar tcode-mazegaki-prefix-overlay nil)

(defvar tcode-mazegaki-stroke-priority-list
; キー配置
;  0  1  2  3  4    5  6  7  8  9
; 10 11 12 13 14   15 16 17 18 19
; 20 21 22 23 24   25 26 27 28 29
; 30 31 32 33 34   35 36 37 38 39
  '(22 23 21 24 20
    12 13 11 14 10
    27 26 28 25 29
    17 16 18 15 19)
  "* 候補を並べるときの位置。
このリストにないキーは使用されない。")

(defvar tcode-mazegaki-alternative-select-first-keys
  '(22)
  "* 候補を二つの中から一つ選ぶ場合の、左側(第1候補)を選ぶキーのリスト。")
(defvar tcode-mazegaki-alternative-select-second-keys
  '(23)
  "* 候補を二つの中から一つ選ぶ場合の、右側(第2候補)を選ぶキーのリスト。")

(defvar tcode-mazegaki-complete-max 10
  "* 補完の際に扱う候補の数の最大値。
候補がこの値以上なら補完は行わない。")

(defconst tcode-mazegaki-inflection-mark "—"
  "読みが活用する場合に、活用語尾を表すためにつける文字列。")

(defvar tcode-mazegaki-max-suffix-length 4
  "* 読みの中の活用語尾の最大文字数。")

(defcustom tcode-mazegaki-fixed-priority-count 4
  "* 学習するときに、学習の対象外となる候補の数。
現在の読みについて、選択された候補の順番がこの数以下ならば、学習されない。"
  :group 'mazegaki-conversion)

(defcustom tcode-mazegaki-inline-candidate-count 0
  "* 候補が複数あっても、第1候補をインライン表示する回数"
  :group 'mazegaki-conversion)

(defvar tcode-mazegaki-splitter "|"
  "* 登録の際の、読みおよび漢字の区切りを表す正規表現")

;;; その他の変数

(defvar tcode-mazegaki-yomi-list nil
  "変換開始時点でカーソルより前にあった文字のリスト。
カーソルの直前の文字が car、その前が cadr、以下同様。
最大 `tcode-mazegaki-yomi-max' 文字まで。")

(defvar tcode-mazegaki-current-yomi-length 0
  "現在変換対象となっている文字列の長さ。
\(length tcode-mazegaki-yomi-list\)以下である。")
(defvar tcode-mazegaki-current-offset 0
  "`tcode-mazegaki-yomi-list' の何番目の文字から読みとみなすか。")
(defvar tcode-mazegaki-current-yomi-point nil)

(defvar tcode-mazegaki-yomi-fixed nil
  "可変長読み変換中ならば nil、そうでないなら t。
つまり、「fj」を前置して読みが入力された場合にはこの変数の値は t であり、
それ以外の場合は nil である。")

(defvar tcode-mazegaki-map nil "交ぜ書き変換中のキーマップ。")
(defvar tcode-mazegaki-mode nil "交ぜ書き変換中かどうかを表す変数。
実際には、交ぜ書き変換対象の先頭のポイントを保持している。")
(make-variable-buffer-local 'tcode-mazegaki-mode)

(defvar tcode-mazegaki-prefix nil)
(make-variable-buffer-local 'tcode-mazegaki-prefix)

(defvar tcode-mazegaki-suffix ""
  "変換の対象外となる接尾語。")

(defvar tcode-mazegaki-conversion-overlay nil
  "交ぜ書き変換の変換対象を表す overlay。")
(make-variable-buffer-local 'tcode-mazegaki-conversion-overlay)

(defvar tcode-mazegaki-inflection-only nil
  "活用変換中かどうか。")

(defvar tcode-mazegaki-with-inflection nil
  "活用する読みを変換対象としているかどうか。")

(defvar tcode-mazegaki-candidate-state nil
  "交ぜ書き候補表示ステート。
'next     次候補表示のための中間状態
'one      候補がひとつしかなくインライン表示中
'priority 複数候補の最初の方をインライン表示中
'table    表形式で選択中

変換開始(変換直後または読み長さ変更直後)
  → 候補数によってoneまたはnext

next
  candidate-indexが候補数より小さくかつ
  inline-candidate-countより小さい
    → priority
  else
    → table

one
  SPC入力
    → 縮め
  = 入力
    → one

priority
  SPC入力
    → candidate-index++し、next
  = 入力
    → table

table
  SPC入力
    → 次ページ
")

(defvar tcode-mazegaki-candidate-index nil
  "priorityステートでインライン表示中の候補は何番目か(0オリジン)。
See also variable `tcode-mazegaki-candidate-state'.")

(defvar tcode-mazegaki-start-marker nil)
(make-variable-buffer-local 'tcode-mazegaki-start-marker)

;;;; 辞書の検索と変換

(cond ((tcode-mule-1-p)
       (defun tcode-mazegaki-delete-conversion-face ()
	 (if tcode-mazegaki-prefix-overlay
	     (save-excursion
	       (goto-char tcode-mazegaki-prefix-overlay)
	       (if (looking-at (regexp-quote tcode-mazegaki-prefix-mark))
		   (delete-char 1))))
	 (setq tcode-mazegaki-prefix-overlay nil))
       (defun tcode-mazegaki-put-conversion-face ()
	 (let ((point (or tcode-mazegaki-mode
			  tcode-mazegaki-prefix)))
	   (when point
	     (save-excursion
	       (goto-char point)
	       (tcode-mazegaki-delete-conversion-face)
	       (setq tcode-mazegaki-prefix-overlay (point))
	       (insert tcode-mazegaki-prefix-mark))))))
      ((tcode-xemacs-p)
       (defun tcode-mazegaki-put-conversion-face ()
	 "変換開始位置から point までを指定された face にする。
face の指定は変数 `tcode-mazegaki-prefix-mark' を設定することにより行う。"
	 (let ((point (or tcode-mazegaki-mode
			  tcode-mazegaki-prefix)))
	   (when point
	     (when tcode-mazegaki-prefix-mark
	       (setq tcode-mazegaki-prefix-overlay
		     (if (extentp tcode-mazegaki-prefix-overlay)
			 (set-extent-endpoints tcode-mazegaki-prefix-overlay
					       point
					       point)
		       (make-extent point point)))
	       (set-extent-begin-glyph tcode-mazegaki-prefix-overlay
				       tcode-mazegaki-prefix-mark))
	     (if (extentp tcode-mazegaki-conversion-overlay)
		 (set-extent-endpoints tcode-mazegaki-conversion-overlay
				       point
				       (point))
	       (setq tcode-mazegaki-conversion-overlay
		     (make-extent point (point)))
	       (set-extent-face tcode-mazegaki-conversion-overlay
				tcode-mazegaki-face)))))
       (defun tcode-mazegaki-delete-conversion-face ()
	 "交ぜ書き変換用の face と prefix を除く。"
	 (if (extentp tcode-mazegaki-prefix-overlay)
	     (detach-extent tcode-mazegaki-prefix-overlay))
	 (if (extentp tcode-mazegaki-conversion-overlay)
	     (detach-extent tcode-mazegaki-conversion-overlay))))
      (t
       (unless (fboundp 'make-overlay)
	 (require 'overlay))
       ;; face により変換対象領域を表示するための関数の定義
       (defun tcode-mazegaki-put-conversion-face ()
	 "変換開始位置から point までを指定された face にする。
face の指定は変数 `tcode-mazegaki-face' を設定することにより行う。
変数 `tcode-mazegaki-prefix-mark' が設定されていれば、その文字列を
先頭に置く。"
	 (let ((point (or tcode-mazegaki-mode
			  tcode-mazegaki-prefix)))
	   (when point
	     ;; prefix を置く。
	     (when tcode-mazegaki-prefix-mark
	       (setq tcode-mazegaki-prefix-overlay
		     (if (overlayp tcode-mazegaki-prefix-overlay)
			 (move-overlay tcode-mazegaki-prefix-overlay
				       point
				       point)
		       (make-overlay point point)))
	       (overlay-put tcode-mazegaki-prefix-overlay
			    'before-string
			    tcode-mazegaki-prefix-mark))
	     ;; face を設定する。
	     (if (overlayp tcode-mazegaki-conversion-overlay)
		 (move-overlay tcode-mazegaki-conversion-overlay point (point))
	       (setq tcode-mazegaki-conversion-overlay
		     (make-overlay point (point)))
	       (overlay-put tcode-mazegaki-conversion-overlay
			    'face
			    tcode-mazegaki-face)))))
       (defun tcode-mazegaki-delete-conversion-face ()
	 "交ぜ書き変換用の face と prefix を除く。"
	 (if (overlayp tcode-mazegaki-prefix-overlay)
	     (delete-overlay tcode-mazegaki-prefix-overlay))
	 (if (overlayp tcode-mazegaki-conversion-overlay)
	     (delete-overlay tcode-mazegaki-conversion-overlay)))))

;;;###autoload
(defun tcode-mazegaki-switch-to-dictionary ()
  "current-buffer を交ぜ書き辞書に切り替える。
交ぜ書き辞書がまだ読み込まれていない場合には読み込む。"
  (interactive)
  (let ((buffer (tcode-set-work-buffer tcode-mazegaki-buffer-name
				       tcode-mazegaki-dictionary-name)))
    (if (called-interactively-p 'interactive)
	(switch-to-buffer buffer))))

(defun tcode-mazegaki-construct-yomi (len &optional offset inflection)
  "`tcode-mazegaki-yomi-list' から、長さ LEN の読みを作る。
OFFSET が指定されている場合は、OFFSET 文字は活用語尾とし、読みには含めず、
INFLECTION が nil でなければ `tcode-mazegaki-inflection-mark' で置き換える。"
  (let ((list (mapcar 'cdr
		      (if offset
			  (nthcdr offset tcode-mazegaki-yomi-list)
			tcode-mazegaki-yomi-list)))
	(str ""))
    (while (> len 0)
      (setq str (concat (car list) str)
	    list (cdr list)
	    len (1- len)))
    (if inflection
	(concat str tcode-mazegaki-inflection-mark)
      str)))

(defun tcode-mazegaki-list-to-string (list from len)
  "文字の LIST の FROM から LEN 文字分の文字列を作る。"
  (let ((str ""))
    (setq list (nthcdr from list))
    (while (> len 0)
      (setq str  (concat (cdr (car list)) str)
	    list (cdr list)
	    len  (1- len)))
    str))

(defun tcode-mazegaki-get-reverse-yomi-list ()
  "現 point より後方にある日本語列または英単語一つのリストを返す。
リストの長さは最大 `tcode-mazegaki-yomi-max' 文字。"
  (setq tcode-mazegaki-suffix ""
	tcode-mazegaki-yomi-fixed tcode-mazegaki-mode)
  (save-excursion
    (let (context reverse-list)
      (catch 'finish
	(while (and (< (length reverse-list)
		       tcode-mazegaki-yomi-max)
		    (setq context
			  (tcode-scan-backward
			   1 tcode-mazegaki-terminate-char-list)))
	  (let ((ch (cdr (car context)))
		(point (car (car context))))
	    (if (and reverse-list
		     (or (= (point) point)
			 (>= (count-lines point (point)) (if (bolp) 2 3))))
		(throw 'finish nil))
	    (if (and (null reverse-list)
		     (memq (tcode-string-to-char ch)
			   tcode-mazegaki-terminate-char-list))
;; `tcode-mazegaki-terminate-char-list' にある文字をとばす。
		(setq tcode-mazegaki-suffix
		      (concat ch tcode-mazegaki-suffix))
	      ;; 読みを1字得る
	      (setq reverse-list (cons (car context) reverse-list))
	      (if (and tcode-mazegaki-prefix
		       (= point tcode-mazegaki-prefix))
		  (throw 'finish nil)))
	    (goto-char point))))
      (nreverse reverse-list))))

(defun tcode-mazegaki-search-yomi (yomi)
  "現在のバッファから YOMI を見つけ、 point を移動する。
見つからない場合は、 point はその YOMI があるべき場所に移動する。"
  (let ((min (point-min))
	(max (point-max))
	str)
    (catch 'found
      (and (eobp)
	   (forward-line -1))
      (while (< min max)
	(beginning-of-line)
	(cond ((string< (setq str (buffer-substring
				   (point)
				   (progn
				       (while (/= (tcode-following-char) ?/)
					 (tcode-forward-char 1))
				       (1- (point)))))
			yomi)
	       ;; もっと大きい
	       (forward-line 1)
	       (goto-char (ash (+ (setq min (point)) max) -1)))
	      ((string< yomi str)
	       ;; もっと小さい
	       (beginning-of-line)
	       (goto-char (ash (+ min (setq max (point))) -1)))
	      ;; 見つけた
	      (t
	       (beginning-of-line)
	       (throw 'found (point))))))))

(defun tcode-mazegaki-lookup (new)
  "現在の読みより短い最長の読みを探す。
NEW が nil でなければ、新しく探し始める。
見つかればその読みの長さおよび offset (常に 0) を、なければ nil を返す。
交ぜ書き辞書のポイントはその読みのところに移動する。"
  (save-excursion
    (setq tcode-mazegaki-with-inflection nil)
    (tcode-mazegaki-switch-to-dictionary)
    (let* ((max (length tcode-mazegaki-yomi-list))
	   (min (if tcode-mazegaki-yomi-fixed (1- max) 0))
	   (i (if new max (1- tcode-mazegaki-current-yomi-length))))
      (catch 'found
	(while (> i min)
	  (when (tcode-mazegaki-search-yomi (tcode-mazegaki-construct-yomi i))
	    (setq tcode-mazegaki-current-yomi-point (point))
	    (throw 'found (cons (car (nth (1- i) tcode-mazegaki-yomi-list))
				(cons i 0))))
	  (setq i (1- i)))))))

(defun tcode-mazegaki-lookup-with-inflection (new)
  "現在の読みより短い、活用する最長の読みを探す。
NEW が nil でなければ、新しく探し始める。
見つかればその読みの長さおよび offset を、なければ nil を返す。
交ぜ書き辞書のポイントはその読みのところに移動する。"
  (save-excursion
    (setq tcode-mazegaki-with-inflection t)
    (tcode-mazegaki-switch-to-dictionary)
    (let* ((max (length tcode-mazegaki-yomi-list))
	   (min 0)
	   (i (if new max tcode-mazegaki-current-yomi-length))
	   (offset (cond (new
			  (cond ((>= (+ i tcode-mazegaki-max-suffix-length)
				     max)
				 (- max i))
				(tcode-mazegaki-yomi-fixed
				 -1)
				(t
				 tcode-mazegaki-max-suffix-length)))
			 (tcode-mazegaki-yomi-fixed
			  -1)
			 (t
			  (1- tcode-mazegaki-current-offset)))))
      (catch 'found
	(while (> i min)
	  (while (>= offset 0)
	    (and (string-match "^[ぁ-ン]*$"
			       (tcode-mazegaki-construct-yomi offset))
		 (tcode-mazegaki-search-yomi
		  (tcode-mazegaki-construct-yomi i offset t))
		 (setq tcode-mazegaki-current-yomi-point (point))
		 (throw 'found (cons (car (nth (1- (+ i offset))
					       tcode-mazegaki-yomi-list))
				     (cons i offset))))
	    (setq offset (if tcode-mazegaki-yomi-fixed -1 (1- offset))))
	  (setq i (1- i)
		offset (cond ((>= (+ i tcode-mazegaki-max-suffix-length)
				  max)
			      (- max i))
			     (tcode-mazegaki-yomi-fixed
			      -1)
			     (t
			      tcode-mazegaki-max-suffix-length))))))))

(defun tcode-mazegaki-lookup-reverse (new)
  "現在の読みよりも長い最短の読みを見つける。
見つかればその読みの長さおよび offset (常に 0) を、なければ nil を返す。
交ぜ書き辞書のポイントはその読みのところに移動する。"
  (save-excursion
    (setq tcode-mazegaki-with-inflection nil)
    (tcode-mazegaki-switch-to-dictionary)
    (let* ((max (length tcode-mazegaki-yomi-list))
	   (i (cond (new (if tcode-mazegaki-yomi-fixed max 1))
		    (tcode-mazegaki-yomi-fixed (1+ max))
		    (t (1+ tcode-mazegaki-current-yomi-length)))))
      (catch 'found
	(while (<= i max)
	  (when (tcode-mazegaki-search-yomi (tcode-mazegaki-construct-yomi i))
	    (setq tcode-mazegaki-current-yomi-point (point))
	    (throw 'found (cons (car (nth (1- i) tcode-mazegaki-yomi-list))
				(cons i 0))))
	  (setq i (1+ i)))))))

(defun tcode-mazegaki-lookup-with-inflection-reverse (new)
  "現在の読みよりも長い最短の活用する読みを見つける。
見つかればその読みの長さおよび offset を、なければ nil を返す。
交ぜ書き辞書のポイントはその読みのところに移動する。"
  (save-excursion
    (setq tcode-mazegaki-with-inflection t)
    (tcode-mazegaki-switch-to-dictionary)
    (let* ((max (length tcode-mazegaki-yomi-list))
	   (i (if new 1 tcode-mazegaki-current-yomi-length))
	   (offset (cond (new (cond ((not tcode-mazegaki-yomi-fixed)
				     0)
				    ((<= (- max i)
					tcode-mazegaki-max-suffix-length)
				     (- max i))
				    (t
				     max)))
			 ((or tcode-mazegaki-yomi-fixed
			      (>= tcode-mazegaki-current-offset
				  tcode-mazegaki-max-suffix-length))
			  max)
			 (t (1+ tcode-mazegaki-current-offset)))))
      (catch 'found
	(while (<= i max)
	  (while (<= (+ i offset) max)
	    (and (string-match "^[ぁ-ン]*$"
			       (tcode-mazegaki-construct-yomi offset))
		 (tcode-mazegaki-search-yomi
		  (tcode-mazegaki-construct-yomi i offset t))
		 (setq tcode-mazegaki-current-yomi-point (point))
		 (throw 'found (cons (car (nth (1- (+ i offset))
					       tcode-mazegaki-yomi-list))
				     (cons i offset))))
	    (setq offset (if (>= offset
				 tcode-mazegaki-max-suffix-length)
			     max
			   (1+ offset))))
	  (setq i (1+ i)
		offset (cond ((not tcode-mazegaki-yomi-fixed)
			      0)
			     ((<= (- max i)
				  tcode-mazegaki-max-suffix-length)
			      (- max i))
			     (t max))))))))

(defun tcode-mazegaki-erase-previous-candidate ()
  "現在バッファに表示されている変換候補を消去する。"
  (if tcode-mazegaki-mode
      (delete-region tcode-mazegaki-mode (point))))

;;;###autoload
(defun tcode-mazegaki-convert (arg &optional inflection)
  "現 point より後方にある日本語列を「読み」として交ぜ書き変換を試る。

ARG は次を意味する。
 * C-u のみまたは - のみ
   活用する語として変換(読みの長さは最長一致)
 * 整数
   その絶対値を読みの長さとして変換
   ただし、変換するのは、正の場合は活用しない語、
   負の場合は活用する語とする。

INFLECTION が nil でなければ、ARG の値によらず、活用する語として変換を行う。"
  (interactive "*P")
  (tcode-mazegaki-candidate-select-init)
  (setq tcode-mazegaki-inflection-only (or (eq arg '-)
					   (and (integerp arg)
						(< arg 0))
					   (and arg
						(listp arg))
					   inflection))
  (let ((tcode-mazegaki-yomi-max (if (integerp arg)
				     (if (>= arg 0) arg (- arg))
				   tcode-mazegaki-yomi-max)))
    (unless (setq tcode-mazegaki-yomi-list
		  (tcode-mazegaki-get-reverse-yomi-list))
      (let (tcode-auto-help)
	(tcode-mazegaki-finish)
	(error "読みがありません。"))))
  (and (integerp arg)
       (or tcode-mazegaki-yomi-fixed
	   (= (length tcode-mazegaki-yomi-list)
	      (if (>= arg 0) arg (- arg)))
	   (error "読みが短かすぎます。"))
       (setq tcode-mazegaki-yomi-fixed t))
  ;; 辞書に読みがあるか調べ、あれば変換する。
  (let ((with-inflection (and tcode-mazegaki-enable-inflection
			      (or inflection
				  (not (and (integerp arg)
					    (> arg 0)))))))
    (let* ((tcode-mazegaki-enable-inflection with-inflection)
	   (yomi-info (or (and (not tcode-mazegaki-inflection-only)
			       (tcode-mazegaki-lookup t))
			  (and (or tcode-mazegaki-enable-inflection
				   tcode-mazegaki-inflection-only)
			       (tcode-mazegaki-lookup-with-inflection t)))))
      (if yomi-info
	  ;; 候補が見つかった。
	  (prog1
	      (setq tcode-mazegaki-current-yomi-length (car (cdr yomi-info))
		    tcode-mazegaki-current-offset (cdr (cdr yomi-info)))
	    ;; i 文字の候補の先頭に印(△)をつける。
	    (save-excursion
	      (goto-char (car yomi-info))
	      (if tcode-mazegaki-mode
		  (if (/= tcode-mazegaki-mode (point))
		      (let (tcode-auto-help)
			(tcode-mazegaki-finish)
			(error "読みが長すぎます。")))
		(setq tcode-mazegaki-mode (point))))
	    (condition-case nil
		(let ((echo-keystrokes 0))
		  (tcode-mazegaki-put-conversion-face)
		  (tcode-mazegaki-select-candidate)
		  (while tcode-mazegaki-mode
		    (let* ((keyseq (read-char))
			   (command (lookup-key tcode-mazegaki-map
						(char-to-string keyseq))))
		      (if (not (commandp command))
			  (progn
			    (tcode-mazegaki-finish)
			    (tcode-redo-command keyseq))
			(setq prefix-arg current-prefix-arg
			      this-command command)
			(command-execute command))))
		  (tcode-mazegaki-finish))
	      (quit
	       (ding)
	       (tcode-mazegaki-restore-yomi-and-quit))))
	;; 候補が無かった。
	(setq this-command 'tcode-mazegaki-begin-conversion)
					; 登録を行うときに用いる細工
	(ding)
	(tcode-verbose-message
	 (tcode-substitute-command-keys
	  (concat "適当な漢字はありません"
		  " (「\\[tcode-mazegaki-make-entry-and-finish]」で登録)")))
	nil))))

;;;###autoload
(defun tcode-mazegaki-begin-conversion (arg)
  "交ぜ書き変換を開始する。"
  (interactive "*P")
  (undo-boundary)
  (cond (tcode-mazegaki-mode
	 (if tcode-mazegaki-yomi-list
	     (tcode-mazegaki-get-reverse-yomi-list))
	 (tcode-mazegaki-convert arg))
	(tcode-use-prefix-mazegaki
	 (tcode-mazegaki-put-prefix))
	(t
	 (tcode-mazegaki-convert arg))))

;;;###autoload
(defun tcode-mazegaki-begin-alternate-conversion (arg)
  "交ぜ書き変換を開始する。ただし、前置型・後置型が逆。"
  (interactive "*P")
  (call-interactively (if tcode-use-prefix-mazegaki
			  'tcode-mazegaki-convert
			'tcode-mazegaki-put-prefix)))

;;;###autoload
(defun tcode-mazegaki-lookup-with-prefix (char-list)
  "CHAR-LISTが読みの先頭になっている候補のリストを返す。"
  (let ((tcode-mazegaki-yomi-list 
	 (mapcar (lambda (e)
		   (cons 0 (char-to-string e)))
		 (reverse char-list)))
	(prefix (regexp-quote (mapconcat 'char-to-string char-list nil)))
	candidate-list
	result)
    (save-excursion
      (tcode-mazegaki-switch-to-dictionary)
      (tcode-mazegaki-search-yomi prefix)
      (while (looking-at prefix)
	(setq tcode-mazegaki-current-yomi-point (point)
	      candidate-list (nconc candidate-list 
				    (tcode-mazegaki-get-candidate-list)))
	(forward-line 1))
      ;; 重複の削除
      (while candidate-list
	(let ((candidate (car candidate-list)))
	  (setq result (nconc result (list candidate))
		candidate-list (delete candidate candidate-list)))))
    result))

;;;; 候補の選択
(defun tcode-mazegaki-candidate-select-init ()
  "ステート情報を初期値に戻す。"
  (setq tcode-mazegaki-candidate-state nil
	tcode-mazegaki-candidate-index 0))

(defun tcode-mazegaki-find-kanji-entry ()
  "現在の読みの(最初の)エントリまで交ぜ書き辞書の point を移動する。
正確には、point は最初のエントリの先頭にある\"/\"の直後に移動する。"
  (tcode-mazegaki-switch-to-dictionary)
  (goto-char tcode-mazegaki-current-yomi-point)
  (beginning-of-line)
  (search-forward " /" nil t))

(defun tcode-mazegaki-get-number-of-candidate ()
  "現在の読みの候補の数を得る。"
  (save-excursion
    (tcode-mazegaki-find-kanji-entry)
    (let ((noc 0)
	  (p (point)))
      (while (not (eolp))
	(if (= (following-char) ?/)
	    (setq noc (1+ noc)))
	(tcode-forward-char 1))
      noc)))

(defun tcode-mazegaki-get-candidate-list ()
  "現在の読みから候補のリストを作る。"
  (save-excursion
    (tcode-mazegaki-find-kanji-entry)
    (let (list)
      (while (not (eolp))
	(setq list (nconc list
			  (list (buffer-substring
				 (point)
				 (prog2
				     (search-forward "/" nil t)
				     (1- (point))))))))
      list)))

(defun tcode-mazegaki-make-candidate-table (candidate-list)
  " CANDIDATE-LIST から候補の表を作る。
候補の表における位置は、変数 `tcode-mazegaki-stroke-priority-list' に従う。"
  (let ((plist tcode-mazegaki-stroke-priority-list)
	(table (make-vector 40 nil)))
    (while (and candidate-list plist)
      (aset table (car plist) (car candidate-list))
      (setq candidate-list (cdr candidate-list)
	    plist (cdr plist)))
    table))

(defun tcode-mazegaki-select (candidate-table noc current-offset
					  &optional msg suffix)
  "CANDIDATE-TABLE から候補を選択させる。
NOC (候補の数)と CURRENT-OFFSET から現在何番目の表を表示しているか計算する。"
  (let* ((plist-size (length tcode-mazegaki-stroke-priority-list))
	 (whole-page (/ (+ noc (1- plist-size)) plist-size))
	 (page (- (1+ whole-page)
		  (/ (+ (- noc current-offset) (1- plist-size)) plist-size)))
	 (whole-table (or (and (catch 'found
				 ;; 3段目以外を使うことを確かめる。
				 (mapc (lambda (e) (if (or (< e 20)
							   (>= e 30))
						       (throw 'found t)))
				       tcode-mazegaki-stroke-priority-list)
				 nil)
			       (> whole-page 1))
			  ;; 3段目以外に候補があるか調べる
			  (catch 'found
			    (let ((i 0))
			      (while (< i 40)
				(and (aref candidate-table i)
				     (throw 'found t))
				(setq i (if (= i 19) 30 (1+ i)))))))))
    (if whole-table
	(progn
	  (if suffix
	      (setq msg (concat msg "  " suffix)))
	  (if (not (and (window-minibuffer-p (selected-window))
			(null msg)))
	      (message (or msg "")))
	  (tcode-display-help-buffer
	   (tcode-draw-table candidate-table page whole-page) t))
      (let ((candidate-list (mapcar (lambda (n)
				      (or (aref candidate-table n)
					  "-"))
				    '(20 21 22 23 24 25 26 27 28 29))))
	(message
	 (concat msg
		 (if (= whole-page 1)
		     ""
		   (format "(%d/%d)  " page whole-page))
		 (apply 'format
			(cons "[%s %s %s %s] %s  %s [%s %s %s %s]"
			      candidate-list))
		 "  "
		 suffix)))))
  (let* ((echo-keystrokes 0)
	 (ch (read-char))
	 (key (tcode-char-to-key ch)))
    (message "")
    (if (< key 0)
	ch
      (or (aref candidate-table (mod key 40))
	  ch))))

(defun tcode-mazegaki-make-table-and-select (&optional
					     msg candidate-list inline)
  "現在の読みから候補を選択させ、その文字列または文字(キー)を返す。"
  (or candidate-list
      (setq candidate-list (tcode-mazegaki-get-candidate-list)))
  (let* ((noc (length candidate-list))
	 (plist-size (length tcode-mazegaki-stroke-priority-list))
	 (suffix (and tcode-mazegaki-with-inflection
		       (concat (tcode-mazegaki-construct-yomi
				tcode-mazegaki-current-yomi-length
				tcode-mazegaki-current-offset)
			       "("
			       (if (zerop tcode-mazegaki-current-offset)
				   tcode-mazegaki-inflection-mark
				 (tcode-mazegaki-construct-yomi
					      tcode-mazegaki-current-offset))
			       ")"))))
    (if (<= noc 1)
	(car candidate-list)
      (if (and inline
	       (= noc 2)
	       tcode-mazegaki-alternative-select-first-keys
	       tcode-mazegaki-alternative-select-second-keys)
	  (let ((first-candidate (car candidate-list))
		(second-candidate (car (cdr candidate-list))))
	    (tcode-mazegaki-erase-previous-candidate)
	    (insert "{" first-candidate "," second-candidate "}")
	    (if tcode-mazegaki-with-inflection
		(insert (tcode-mazegaki-construct-yomi
			 tcode-mazegaki-current-offset)
			tcode-mazegaki-suffix))
	    (tcode-mazegaki-put-conversion-face)
	    (let* ((c (read-char))
		   (key (tcode-char-to-key c)))
	      (tcode-mazegaki-restore-yomi-and-quit t)
	      (cond ((memq key tcode-mazegaki-alternative-select-first-keys)
		     first-candidate)
		    ((memq key tcode-mazegaki-alternative-select-second-keys)
		     second-candidate)
		    (t
		     c))))
	;; noc >(=) 2
	(save-excursion
	  (let ((current-offset 0)
		(candidate (tcode-mazegaki-select
			(tcode-mazegaki-make-candidate-table candidate-list)
			noc 0 msg suffix)))
	    (while (and (char-or-string-p candidate)
			(not (stringp candidate))
			(or (= candidate ? )
			    (= candidate ?\C-?)
			    (= candidate ?\C-h)))
	      (setq current-offset (if (= candidate ? )
				       (+ current-offset plist-size)
				     (- current-offset plist-size)))
	      (if (>= current-offset noc)
		  (setq current-offset 0))
	      (if (< current-offset 0)
		  (let ((v current-offset))
		    (while (< (setq v (+ v plist-size)) noc)
		      (setq current-offset v))))
	      (setq candidate
		    (tcode-mazegaki-select
		     (tcode-mazegaki-make-candidate-table
		      (nthcdr current-offset candidate-list))
		     noc current-offset msg suffix)))
	    (tcode-auto-remove-help t)
	    candidate))))))

(defun tcode-mazegaki-show-candidate-inline (candidate)
  "候補をインライン表示する。"
  (tcode-mazegaki-erase-previous-candidate)
  (insert candidate
	  (tcode-mazegaki-list-to-string
	   tcode-mazegaki-yomi-list
	   0
	   tcode-mazegaki-current-offset)
	  tcode-mazegaki-suffix)
  (tcode-mazegaki-put-conversion-face))

(defun tcode-mazegaki-select-candidate ()
  "現在の読みから候補を選択する。"
  (let* ((candidate-list (tcode-mazegaki-get-candidate-list))
	 (noc (length candidate-list))
	 (msg (and (not (window-minibuffer-p (selected-window)))
		   (tcode-verbose-message "(? でヘルプ)"))))
    (if (eq tcode-mazegaki-candidate-state 'priority)
	(setq tcode-mazegaki-candidate-index 
	      (1+ tcode-mazegaki-candidate-index)
	      tcode-mazegaki-candidate-state 'next))
    (if (null tcode-mazegaki-candidate-state)
	(setq tcode-mazegaki-candidate-state (if (= noc 1) 'one 'next)))
    (if (eq tcode-mazegaki-candidate-state 'next)
	(setq tcode-mazegaki-candidate-state
	      (if (and (< tcode-mazegaki-candidate-index noc)
		       (< tcode-mazegaki-candidate-index
			  tcode-mazegaki-inline-candidate-count))
		  'priority
		'table)))
    (cond ((eq tcode-mazegaki-candidate-state 'one)
	   (tcode-mazegaki-show-candidate-inline (car candidate-list))
	   (when (and tcode-mazegaki-yomi-fixed
		      (not tcode-mazegaki-enable-inflection))
	     (tcode-mazegaki-finish)
	     (setq msg nil))
	   (if msg
	       (message msg)))
	  ((eq tcode-mazegaki-candidate-state 'priority)
	   (tcode-mazegaki-show-candidate-inline
	    (nth tcode-mazegaki-candidate-index candidate-list))
	   (if msg
	       (message msg)))
	  ((eq tcode-mazegaki-candidate-state 'table)
	   (let ((selected-candidate (tcode-mazegaki-make-table-and-select
				  msg candidate-list t)))
	     (cond ((stringp selected-candidate)
		    (tcode-mazegaki-show-candidate-inline selected-candidate)
		    (tcode-mazegaki-finish))
		   ((char-or-string-p selected-candidate)
		    (tcode-redo-command selected-candidate))))))))

(unless (memq 'tcode-mazegaki-select-candidate 
	      tcode-no-wait-display-help-command-list)
  (setq tcode-no-wait-display-help-command-list
	(cons 'tcode-mazegaki-select-candidate 
	      tcode-no-wait-display-help-command-list)))

(defun tcode-mazegaki-table-mode ()
  "候補一覧表示に切り換える。"
  (interactive)
  (cond ((eq tcode-mazegaki-candidate-state 'one)
	 ;; nop
	 )
	((eq tcode-mazegaki-candidate-state 'priority)
	 (setq tcode-mazegaki-candidate-state 'table
	       tcode-mazegaki-candidate-index 
	       (tcode-mazegaki-get-number-of-candidate))
	 (tcode-mazegaki-select-candidate))
	(t
	 ;; nop
	 )))

(defun tcode-mazegaki-select-candidate-from-table ()
  "現在の読みから候補一覧表を作成し、そこから候補を選択する。"
  (interactive "*")
  (let ((selected-candidate (tcode-mazegaki-make-table-and-select
			 (and (not (window-minibuffer-p (selected-window)))
			      (tcode-verbose-message "(? でヘルプ)"))
			 nil t))
	(noc (tcode-mazegaki-get-number-of-candidate)))
    (cond ((stringp selected-candidate)
	   (tcode-mazegaki-show-candidate-inline selected-candidate)
	   (unless (and (= noc 1)
			(or tcode-mazegaki-enable-inflection
			    (not tcode-mazegaki-yomi-fixed)))
	     (tcode-mazegaki-finish)))
	  ((char-or-string-p selected-candidate)
	   (tcode-redo-command selected-candidate)))))

;;;; 読みの区切り直し・候補の確定

(defun tcode-mazegaki-restore-yomi-and-quit (&optional not-quit)
  "読みの状態に戻して、交ぜ書き変換を終了する。
NOT-QUIT が nil でないときは、読みの状態に戻すだけで、終了はしない。"
  (interactive "P")
  (tcode-mazegaki-erase-previous-candidate)
  (tcode-mazegaki-candidate-select-init)	; 念のため
  (insert (tcode-mazegaki-list-to-string
	   tcode-mazegaki-yomi-list 0
	   (+ tcode-mazegaki-current-yomi-length
	      tcode-mazegaki-current-offset))
	  tcode-mazegaki-suffix)
  (tcode-mazegaki-put-conversion-face)
  (unless not-quit
    (tcode-mazegaki-delete-conversion-face)
    (unless (window-minibuffer-p (selected-window))
	(message ""))
    (tcode-do-auto-fill)
    (setq tcode-mazegaki-mode nil
	  tcode-mazegaki-prefix nil)
    (while tcode-mazegaki-start-marker
      (goto-char (car tcode-mazegaki-start-marker))
      (setq tcode-mazegaki-start-marker (cdr tcode-mazegaki-start-marker)))))

(defun tcode-mazegaki-relimit (length offset)
  "現在の読みを区切り直し、候補を選択させる。
区切り直す読みは、長さ LENGTH と OFFSET とで表される。"
  (tcode-mazegaki-candidate-select-init)
  (save-excursion
    (goto-char tcode-mazegaki-mode)
    (let ((old-yomi-total (+ tcode-mazegaki-current-yomi-length
			     tcode-mazegaki-current-offset))
	  (new-yomi-total (+ length offset)))
      (if (<= old-yomi-total new-yomi-total)
	  ;; マークの前の読みを消す
	  (delete-region
	   (car (nth (- new-yomi-total old-yomi-total)
		     (nthcdr (1- old-yomi-total) tcode-mazegaki-yomi-list)))
	   (point))
	;; マークの前に読みを入れる
	(insert (tcode-mazegaki-list-to-string
		 tcode-mazegaki-yomi-list
		 new-yomi-total
		 (- old-yomi-total new-yomi-total)))))
    (setq tcode-mazegaki-mode (point)))
  (tcode-mazegaki-put-conversion-face)
  (setq tcode-mazegaki-current-yomi-length length
	tcode-mazegaki-current-offset offset)
  (tcode-mazegaki-restore-yomi-and-quit t)
  (tcode-mazegaki-select-candidate))

(defun tcode-mazegaki-relimit-right ()
  "読みを縮める。"
  (interactive)
  (let ((p (save-excursion
	     (tcode-mazegaki-switch-to-dictionary)
	     (point)))
	(orig-with-inflection tcode-mazegaki-with-inflection)
	(yomi-info (or (and (not tcode-mazegaki-inflection-only)
			    (not tcode-mazegaki-with-inflection)
			    (tcode-mazegaki-lookup nil))
		       (and (or tcode-mazegaki-enable-inflection
				tcode-mazegaki-inflection-only)
			    (tcode-mazegaki-lookup-with-inflection
			     (not tcode-mazegaki-with-inflection))))))
    (if yomi-info
	(tcode-mazegaki-relimit (car (cdr yomi-info)) (cdr (cdr yomi-info)))
      (save-excursion
	(tcode-mazegaki-switch-to-dictionary)
	(goto-char p))
      (setq tcode-mazegaki-with-inflection orig-with-inflection)
      (ding)
      (tcode-verbose-message "これ以上読みは縮められません。"))))

(defun tcode-mazegaki-relimit-left ()
  "読みを伸ばす。"
  (interactive)
  (let ((p (save-excursion
	     (tcode-mazegaki-switch-to-dictionary)
	     (point)))
	(orig-with-inflection tcode-mazegaki-with-inflection)
	(yomi-info (or (and (or tcode-mazegaki-enable-inflection
				tcode-mazegaki-inflection-only)
			    tcode-mazegaki-with-inflection
			    (tcode-mazegaki-lookup-with-inflection-reverse 
			     nil))
		       (and (not tcode-mazegaki-inflection-only)
			    (tcode-mazegaki-lookup-reverse
			     tcode-mazegaki-with-inflection)))))
    (if yomi-info
	(tcode-mazegaki-relimit (car (cdr yomi-info)) (cdr (cdr yomi-info)))
      (save-excursion
	(tcode-mazegaki-switch-to-dictionary)
	(goto-char p))
      (setq tcode-mazegaki-with-inflection orig-with-inflection)
      (ding)
      (tcode-verbose-message "これ以上読みは伸ばせません。"))))

(defun tcode-mazegaki-select-candidate-or-relimit ()
  "読みを縮めるか、漢字の一覧から候補を選択する。
読みを縮めるのは、現在の候補の数が一つの場合のみ。"
  (interactive)
  (if (/= (tcode-mazegaki-get-number-of-candidate) 1)
      (tcode-mazegaki-select-candidate)
    (cancel-undo-boundary)
    (tcode-mazegaki-relimit-right)))

(defun tcode-mazegaki-prioritize (candidate)
  "現在の読みの CANDIDATE を学習する。
ただし、学習する候補は、変数 `tcode-mazegaki-fixed-priority-count' で
指定した数よりも順序が後のもののみ。"
  (save-excursion
    (tcode-mazegaki-find-kanji-entry)
    (if tcode-mazegaki-with-inflection
	(setq candidate
	      (substring candidate
			 0
			 (- (length (tcode-mazegaki-construct-yomi
				     tcode-mazegaki-current-offset))))))
    (let ((eol (save-excursion (end-of-line) (point)))
	  (latest-point (point))
	  (i 1)
	  (beg (point)))
      (catch 'done
	(while (search-forward "/" eol t)
	  (when (equal (buffer-substring beg (1- (point)))
			 candidate)
	    (if (<= i tcode-mazegaki-fixed-priority-count)
		(throw 'done nil)
	      (delete-region beg (point))
	      (goto-char latest-point)
	      (insert candidate "/")
	      (throw 'done t)))
	  (if (= i tcode-mazegaki-fixed-priority-count)
	      (setq latest-point (point)))
	  (setq beg (point)
		i (1+ i)))))))

(defun tcode-mazegaki-finish ()
  "交ぜ書き変換を確定して、交ぜ書き変換モードを抜ける。"
  (interactive)
  (tcode-mazegaki-candidate-select-init)
  (when tcode-mazegaki-prefix
    (setq tcode-mazegaki-prefix nil)
    (tcode-mazegaki-delete-conversion-face))
  (if tcode-mazegaki-mode
      (let* ((beg (if (and (stringp tcode-mazegaki-prefix-mark)
			   (save-excursion 
			     (goto-char tcode-mazegaki-mode)
			     (looking-at 
			      (regexp-quote tcode-mazegaki-prefix-mark))))
		      (match-end 0)
		    tcode-mazegaki-mode))
	     (end (point))
	     (kakutei (prog1
			  (buffer-substring beg end)
			(and tcode-mazegaki-selected-candidate-register
			     (copy-to-register
			      tcode-mazegaki-selected-candidate-register 
			      beg end)))))
	(if overwrite-mode
	    (delete-text-in-column nil (+ (current-column)
					  (string-width kakutei)))
	  (tcode-do-auto-fill))
	(and (boundp 'self-insert-after-hook)
	     self-insert-after-hook
	     (funcall self-insert-after-hook tcode-mazegaki-mode (point)))
	(run-hooks 'input-method-after-insert-chunk-hook)
	(tcode-mazegaki-prioritize kakutei)
	(and tcode-record-file-name
	     (setq tcode-mazegaki-occurrence
		   (+ (chars-in-string kakutei) tcode-mazegaki-occurrence)))
	(setq tcode-mazegaki-mode nil
	      tcode-mazegaki-prefix nil)
	(tcode-mazegaki-delete-conversion-face)
	(when tcode-mazegaki-start-marker
	  ;; recursive conversion
	  (goto-char (car tcode-mazegaki-start-marker))
	  (setq tcode-mazegaki-start-marker (cdr tcode-mazegaki-start-marker))
	  (tcode-mazegaki-convert nil))
	(unless (window-minibuffer-p (selected-window))
	  (message ""))
	(unless (string= kakutei "")
	  (setq tcode-help-char
		(let ((kakutei-chars (string-to-list kakutei)))
		  (while (cdr kakutei-chars)
		    (setq kakutei-chars (cdr kakutei-chars)))
		  (char-to-string (car kakutei-chars)))))
	(and tcode-auto-help
	     kakutei
	     (not (string= kakutei ""))
	     (tcode-display-direct-stroke
	      (substring kakutei
			 0
			 (and (not (string= tcode-mazegaki-suffix ""))
			      (- (length tcode-mazegaki-suffix))))
	      (tcode-mazegaki-construct-yomi
	       (+ tcode-mazegaki-current-yomi-length
		  tcode-mazegaki-current-offset)))
	     (tcode-auto-remove-help-char)))))

(add-hook 'tcode-clear-hook 'tcode-mazegaki-finish)

(defun tcode-mazegaki-recursive-convert-backward ()
  (interactive)
  (let ((marker (make-marker))
	(beg tcode-mazegaki-mode))
    (let (tcode-mazegaki-start-marker)	; protect
      (tcode-mazegaki-restore-yomi-and-quit))
    (setq tcode-mazegaki-start-marker 
	  (cons (set-marker marker (point))
		tcode-mazegaki-start-marker))
    (goto-char beg)
    (tcode-mazegaki-convert nil)))

(defun tcode-mazegaki-cancel-previous-recursive-convert ()
  (interactive)
  (when tcode-mazegaki-start-marker
    (let (tcode-mazegaki-start-marker)	; protect
      (tcode-mazegaki-restore-yomi-and-quit))
    (goto-char (car tcode-mazegaki-start-marker))
    (setq tcode-mazegaki-start-marker (cdr tcode-mazegaki-start-marker))
    (tcode-mazegaki-convert nil)))

;;;; 登録と削除

(defun tcode-mazegaki-get-yomi-and-kanji (prompt &optional str)
  "交ぜ書き登録時の「読み」と「漢字」を適当に得る。"
  (let* ((minibuffer-setup-hook
	  ;; avoid referencing undefined variables in NEmacs.
	  (and (boundp 'minibuffer-setup-hook)
	       (cons 'toggle-input-method minibuffer-setup-hook)))
	 (yomi (if tcode-mazegaki-mode
		   (buffer-substring tcode-mazegaki-mode (point))
		 (read-from-minibuffer (concat prompt "読み ")
				       (if (and str
						(or (tcode-mule-2-p)
						    (tcode-mule-3-p)
						    (tcode-mule-4-p)))
					   (cons str 1)
					 str))))
	 (kanji (read-from-minibuffer
		 (format "%s漢字(読み=%s) " prompt yomi))))
    (list (mapconcat 'char-to-string
		     (delq ?\n (string-to-list yomi))
		     nil)
	  (mapconcat 'char-to-string
		     (delq ?\n (string-to-list kanji))
		     nil))))

(defun tcode-split-string-by-regexp (string splitter)
  "文字列 STRING を、正規表現 SPLITTER を区切りとして分割する。"
  (let (l)
    (while (string-match splitter string)
      (setq l (nconc l (list (substring string 0 (match-beginning 0))))
	    string (substring string (match-end 0))))
    (nconc l (list string))))

(defun tcode-mazegaki-yomi-combination (yomi-list kanji-list)
  "読みと漢字との取り得る組み合わせをすべて列挙する。"
  (if (<= (length yomi-list) 1)
      (append kanji-list yomi-list)
    (let ((yomi-car (car yomi-list))
	  (yomi-cdr (cdr yomi-list))
	  (kanji-car (car kanji-list))
	  (kanji-cdr (cdr kanji-list)))
      (if (string= yomi-car kanji-car)
	  (mapcar (lambda (a) (concat kanji-car a))
		  (tcode-mazegaki-yomi-combination yomi-cdr kanji-cdr))
	(nconc
	 (mapcar (lambda (a) (concat kanji-car a))
		 (tcode-mazegaki-yomi-combination yomi-cdr kanji-cdr))
	 (mapcar (lambda (a) (concat yomi-car a))
		 (tcode-mazegaki-yomi-combination yomi-cdr kanji-cdr)))))))

;;;###autoload
(defun tcode-mazegaki-make-entry (yomi kanji)
  "読み YOMI、漢字 KANJI で、新たなエントリを交ぜ書き辞書に登録する。
正しく登録されれば t、そうでなければ nil を返す。
読みおよび漢字が `tcode-mazegaki-splitter' で区切ってある場合には、
それらを組み合わせた読みすべてについて登録する。"
  (interactive (tcode-mazegaki-get-yomi-and-kanji "登録■"))
  (and (called-interactively-p 'interactive)
       (> (string-width yomi) tcode-mazegaki-yomi-max)
       (message (concat "読み「%s」の長さ(%d)は `tcode-mazegaki-yomi-max' "
			"の値を超えています。")
		yomi (string-width yomi))
       (setq tcode-mazegaki-yomi-max (string-width yomi)))
  (let ((yomi-list (tcode-split-string-by-regexp yomi
						 tcode-mazegaki-splitter)))
    (if (> (length yomi-list) 1)
	;; multi pattern
	(let ((kanji-list
	       (tcode-split-string-by-regexp kanji
					     tcode-mazegaki-splitter)))
	  (unless (= (length yomi-list) (length kanji-list))
	    (error "区切り方が間違っています。"))
	  (setq yomi-list (tcode-mazegaki-yomi-combination
			   yomi-list kanji-list)
		kanji (car yomi-list)
		yomi-list (cdr yomi-list))
	  (mapcar (lambda (yomi)
		    (tcode-mazegaki-make-entry yomi kanji))
		  yomi-list))
      (save-excursion
	(tcode-mazegaki-switch-to-dictionary)
	(tcode-mazegaki-search-yomi yomi)
	(setq tcode-mazegaki-current-yomi-point (point))
	;; 既に読みが登録されていれば、そこに追加し、
	;; 登録されていなければ、新たにエントリを作る。
	(cond ((not (looking-at (concat yomi " /")))
	       (insert yomi " /" kanji "/\n")
	       t)
	      ((re-search-forward (concat "/" kanji "/")
				  (save-excursion (end-of-line) (point))
				  t)
	       (and (called-interactively-p 'interactive)
		    (progn
		      (ding)
		      (message "「%s」はすでに登録されています。" kanji)))
	       nil)
	      (t
	       (tcode-mazegaki-find-kanji-entry)
	       (insert kanji "/")))))))

(defun tcode-mazegaki-inflection-p (yomi)
  "読みが活用するものかどうかを表す述語。"
  (let ((length (length tcode-mazegaki-inflection-mark)))
    (and (stringp yomi)
	 (> (length yomi) length)
	 (string= (substring yomi (- length))
		  tcode-mazegaki-inflection-mark))))

;;;###autoload
(defun tcode-mazegaki-make-entry-and-finish ()
  "新たなエントリを交ぜ書き辞書に登録し、確定する。
読みは、交ぜ書き変換中あるいは直前に変換に失敗していた場合は、そこから得る。
そして、登録後に登録した漢字を確定する。
交ぜ書き変換中あるいは変換に失敗した直後以外の場合は、確定は行わない。"
  (interactive)
  (let ((yomi-exists (or tcode-mazegaki-mode
			 (eq last-command 'tcode-mazegaki-begin-conversion))))
    (if tcode-mazegaki-mode
	(tcode-mazegaki-restore-yomi-and-quit))
    (let* ((yomi-kanji (tcode-mazegaki-get-yomi-and-kanji
			"登録■" (if yomi-exists
				     (tcode-mazegaki-list-to-string
				      tcode-mazegaki-yomi-list 0
				      (length tcode-mazegaki-yomi-list))
				   nil)))
	   (yomi (car yomi-kanji)))
      (tcode-mazegaki-make-entry yomi (car (cdr yomi-kanji)))
      (if yomi-exists
	  ;; 確定する
	  (let* ((split-yomi
		  (tcode-split-string-by-regexp yomi
						tcode-mazegaki-splitter))
		 (real-yomi (let (s)
			      (while split-yomi
				(setq s (concat s (car split-yomi))
				      split-yomi (cdr split-yomi)))
			      s)))
	    (tcode-mazegaki-convert
	     (let ((yomi-list (string-to-list real-yomi)))
	       (if (<= (car yomi-list) 255)
		   1			; alphabet
		 (length yomi-list)))
	     (tcode-mazegaki-inflection-p real-yomi)))))))

;;;###autoload
(defun tcode-mazegaki-delete-entry (yomi kanji)
  "読み YOMI、漢字 KANJI のエントリを交ぜ書き辞書から削除する。"
  (interactive
   (let* ((minibuffer-setup-hook
	   ;; avoid referencing undefined variables in NEmacs.
	   (and (boundp 'minibuffer-setup-hook)
		(cons 'toggle-input-method minibuffer-setup-hook)))
	  (yomi (if tcode-mazegaki-mode
		    (buffer-substring tcode-mazegaki-mode (point))
		  (read-from-minibuffer "削除■読み ")))
	  (found (save-excursion
		   (tcode-mazegaki-switch-to-dictionary)
		   (tcode-mazegaki-search-yomi yomi)))
	  (candidate (and found
		      (tcode-mazegaki-make-table-and-select
		       (format "削除■読み %s " yomi))))
	  (yes (and (stringp candidate)
		    (y-or-n-p
		     (format
		      "読み「%s」漢字「%s」を削除しますか? "
		      yomi candidate))))
	  (kanji (cond (yes
			candidate)
		       ((not found)
			(error "読み「%s」は登録されていません。" yomi))
		       ((error "削除中止")))))
     (list yomi kanji)))
  (save-excursion
    (tcode-mazegaki-switch-to-dictionary)
    (if (and (tcode-mazegaki-search-yomi yomi)
	     (setq tcode-mazegaki-current-yomi-point (point))
	     (re-search-forward (concat "\\(/" kanji "\\)/")
				(save-excursion (end-of-line) (point))
				t))
	(prog2		  ; 1行全体を削除したとき t それ
				     ; 以外は nil を返す
	    (progn
	      (delete-region (match-beginning 1) (match-end 1))
	      (tcode-mazegaki-find-kanji-entry))
	    (and (looking-at "$")
		 (prog1 t		; 1行全体を削除
		   (beginning-of-line)
		   (delete-region (point) (progn (forward-line 1) (point)))))
	  (and (called-interactively-p 'interactive)
	       (message "読み「%s」漢字「%s」を削除しました。" yomi kanji)))
      (when (called-interactively-p 'interactive)
	(ding)
	(message "読み「%s」漢字「%s」は登録されていません。"
		 yomi kanji)))))

(defun tcode-mazegaki-apply-entries-region (beg end func msg)
  "リージョン中の辞書の項目それぞれに対して FUNC を適用する。
FUNC へは、第1引数に読み、第2引数に漢字が順次渡される。

辞書の項目は1行ずつ指定する。各行の書式は

読み /漢字/[漢字/]*\\n"
  (interactive "r")
  (save-excursion
    (save-restriction
      (message "一括%s中..." msg)
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((line 1))
	(condition-case nil
	    (while (not (eobp))
	      (let* ((eol (save-excursion (end-of-line) (point)))
		     (bol (point))
		     (yomi (buffer-substring (point)
					     (progn
					       (search-forward " /" eol)
					       (goto-char (- (point) 2))
					       (point)))))
		(re-search-forward "/$" eol)
		(goto-char (1- (point)))
		(while (re-search-backward "/\\(.+\\)" bol t)
		  (funcall func yomi (buffer-substring
				      (match-beginning 1)
				      (match-end 1))))
		(setq line (1+ line))
		(forward-line 1)))
	  (error
	   (ding)
	   (message "%d 行目で辞書の書式が間違っています。" line)))
	(message "一括%s中...完了" msg))
      (widen))))

;;;###autoload
(defun tcode-mazegaki-make-entries-region (beg end)
  "リージョン中の辞書の項目を一括して登録する。
辞書の書式は関数 `tcode-mazegaki-apply-entries-region' を参照。"
  (interactive "r")
  (tcode-mazegaki-apply-entries-region 
   beg end 'tcode-mazegaki-make-entry "登録"))

;;;###autoload
(defun tcode-mazegaki-make-entries-buffer (&optional buffer)
  "バッファ中の辞書の項目を一括して登録する。
コマンド `tcode-mazegaki-make-entries-region' 参照。"
  (interactive)
  (save-excursion
    (if buffer
	(set-buffer buffer))
    (tcode-mazegaki-make-entries-region (point-min) (point-max))))

;;;###autoload
(defun tcode-mazegaki-delete-entries-region (beg end)
  "リージョン中の辞書の項目を一括して削除する。
辞書の書式は関数 `tcode-mazegaki-apply-entries-region' を参照。"
  (interactive "r")
  (tcode-mazegaki-apply-entries-region
   beg end 'tcode-mazegaki-delete-entry "削除"))

;;;###autoload
(defun tcode-mazegaki-delete-entries-buffer (&optional buffer)
  "バッファ中の辞書の項目を一括して削除する。
コマンド `tcode-mazegaki-delete-entries-region' 参照。"
  (interactive)
  (save-excursion
    (if buffer
	(set-buffer buffer))
    (tcode-mazegaki-delete-entries-region (point-min) (point-max))))

;;;###autoload
(defun tcode-mazegaki-delete-entry-by-last-yomi (arg)
  "最後に入力した読みから漢字を選択し、それを削除する。
引数 ARG が nil でないときは、読みも新たに入力する。"
  (interactive "P")
  (if (or (null tcode-mazegaki-yomi-list)
	  arg)
      (call-interactively 'tcode-mazegaki-delete-entry)
    (let* ((yomi (save-excursion
		   (tcode-mazegaki-switch-to-dictionary)
		   (beginning-of-line)
		   (looking-at "\\([^/]+\\) /")
		   (buffer-substring (match-beginning 1) (match-end 1))))
	   (kanji (tcode-mazegaki-make-table-and-select
		   (format "削除■読み「%s」漢字は? " yomi))))
      (if (not (stringp kanji))
	  (message "削除中止")
	(if (y-or-n-p (format "読み「%s」漢字「%s」を削除しますか "
			      yomi kanji))
	    (tcode-mazegaki-delete-entry yomi kanji))))))

;;;; 読みからの補完

(defun tcode-mazegaki-make-completion-prompt (yomi comp-list)
  "補完候補が複数ある場合の、選択のためのプロンプトの文字列を作る。"
  (let* ((prompt (concat yomi "{"
			 (substring (car comp-list) (length yomi) nil)))
	 (max-len (- (frame-width) 12)) ; 12 is for suffix
	 (yomi-len (length yomi))
	 (diff (substring (car (setq comp-list (cdr comp-list)))
			  yomi-len nil))
	 (new-prompt (concat prompt ", " diff)))
    (concat
     (catch 'overflow
       (while comp-list
	 (or (< (string-width new-prompt) max-len)
	     (throw 'overflow prompt))
	 (setq prompt new-prompt
	       comp-list (cdr comp-list)
	       diff (and comp-list
			 (substring (car comp-list) yomi-len nil))
	       new-prompt (concat prompt ", " diff)))
       prompt)
     (if comp-list
	 (format ", … (+%d)" (length comp-list))
       "}"))))

;;;###autoload
(defun tcode-mazegaki-complete (&optional conversion)
  "交ぜ書き辞書の読みから補完を行う。
候補が複数あるときのキー割り当ては次のとおり。

    SPC		次の候補を先頭にする
    DEL		最後の候補を先頭にする
    TAB		先頭の候補を補完し終了する
    LFD		先頭の候補を補完し終了・その後変換する
    RET		選択せずに補完を終了する

CONVERSION が nil でないとき、補完後(補完を行った場合のみ)変換を行う。"
  (interactive "*P")
  (setq tcode-mazegaki-yomi-list (tcode-mazegaki-get-reverse-yomi-list))
  (unless tcode-mazegaki-yomi-list
    (error "補完できません。"))
  (let* ((yomi-len (length tcode-mazegaki-yomi-list))
	 (yomi-prefix
	  (catch 'found
	    (while (> yomi-len 0)
	      (let ((yomi (regexp-quote
			   (tcode-mazegaki-construct-yomi yomi-len))))
		(save-excursion
		  (tcode-mazegaki-switch-to-dictionary)
		  (tcode-mazegaki-search-yomi yomi)
		  (and (looking-at yomi)
		       (or (not (looking-at (concat yomi " /")))
			   (save-excursion
			     (forward-line 1)
			     (looking-at yomi)))
		       (throw 'found yomi))))
	      (and tcode-mazegaki-mode
		   tcode-mazegaki-yomi-fixed
		   (throw 'found nil))
	      (setq yomi-len (1- yomi-len)))))
	 (completion-list
	  (and yomi-prefix
	       (save-excursion
		 (tcode-mazegaki-switch-to-dictionary)
		 (let (list 
		       yomi 
		       (i 0)
		       (yomi-len (length yomi-prefix)))
		   (while (string= yomi-prefix
				   (buffer-substring (point) 
						     (+ (point) yomi-len)))
		     (setq yomi (buffer-substring (point)
						  (save-excursion
						    (search-forward " /")
						    (- (point) 2)))
			   i (1+ i))
		     (if (> i tcode-mazegaki-complete-max)
			 (error "「%s」は候補の数が多すぎます。"
				yomi-prefix))
		     (unless (string= yomi yomi-prefix)
		       (setq list (nconc list (list (cons yomi nil)))))
		     (forward-line 1))
		   list))))
	 (most (and completion-list
		    (try-completion yomi-prefix completion-list)))
	 comp)
    (or completion-list
	(error "補完できません。"))
    ;; for NEmacs
    (and (stringp most)
	 (let ((last-char (aref most (1- (length most)))))
	   (and (= (mod (string-width most) 2) 1)
		(> last-char 127)
		(<= last-char 255)
		(setq most (substring most 0 -1))
		(string= yomi-prefix most)
		(setq most nil))))
    ;; 補完できる部分を補完
    (when most
      (delete-region (car (nth (1- yomi-len) tcode-mazegaki-yomi-list))
		     (point))
      (insert (if (stringp most)
		  most
		yomi-prefix))
      (tcode-do-auto-fill))
    (if (stringp most)
	(setq yomi-prefix most))
    (setq comp (and completion-list
		    (all-completions yomi-prefix completion-list)))
    (when (> (length comp) 1)
      ;; 選択モード
      (catch 'quit
	(while t
	  (message (tcode-mazegaki-make-completion-prompt yomi-prefix comp))
	  (let ((ch (read-char)))
	    (cond ((= ch ? )
		   ;; 次の候補を先頭へ
		   (setq comp (nconc (cdr comp) (list (car comp)))))
		  ((= ch ?\C-?)
		   ;; 最後の候補を先頭へ
		   (let ((list comp))
		     (while (and (cdr list)
				 (cdr (cdr list)))
		       (setq list (cdr list)))
		     (setq comp (append (cdr list) comp))
		     (rplacd list nil)))
		  ((or (= ch ?\t)
		       (= ch last-command-event))
		   ;; 先頭の候補を選択して終了
		   (insert (substring (car comp)
				      (length yomi-prefix) nil))
		   (tcode-do-auto-fill)
		   (throw 'quit t))
		  ((= ch ?\n)
		   ;; 先頭の候補を選択して終了・終了後に変換
		   (insert (substring (car comp)
				      (length yomi-prefix) nil))
		   (tcode-do-auto-fill)
		   (setq conversion t)
		   (throw 'quit t))
		  ((= ch ?\r)
		   ;; 候補を選択せず終了
		   (setq conversion nil)
		   (throw 'quit t))
		  (t
		   ;; 終了し、そのコマンドを実行
		   (tcode-redo-command ch)
		   (setq conversion nil)
		   (throw 'quit t))))))
      (unless (window-minibuffer-p (selected-window))
	(message "")))
    (if conversion
	(tcode-mazegaki-convert nil))))

;;;###autoload
(defun tcode-mazegaki-complete-and-convert ()
  "交ぜ書き辞書の読みから補完を行い、その後変換する。
詳細はコマンド `tcode-mazegaki-complete' 参照。"
  (interactive "*")
  (tcode-mazegaki-complete t))

;;;; その他(キー割り当てや辞書の保存など)

(defun tcode-mazegaki-backward-delete-char (arg)
  "コマンド `backward-delete-char' と同じか、更に交ぜ書き変換を終了する。
交ぜ書き変換を終了するのは、開始点を消したとき。"
  (interactive "*p")
  (if (<= (point) tcode-mazegaki-mode)
      (tcode-mazegaki-finish)
    (backward-delete-char arg)))

(defun tcode-mazegaki-command-summary ()
  "交ぜ書き変換時のキーの割り当て一覧を表示する。
表示する内容は、変数 `tcode-mazegaki-command-summary-alist' で指定する。"
  (interactive)
  (message
   (mapconcat
    (lambda (elm)
      (let* ((key (where-is-internal (cdr elm) tcode-mazegaki-map t))
	     (key-str (if (null key)
			  (error
			   (concat "`tcode-mazegaki-command-summary-alist' "
				   "の関数名に間違いがあります。"))
			(key-description key))))
	(format "%s=%s" key-str (car elm))))
    tcode-mazegaki-command-summary-alist " "))
  (sit-for 5))

;;;###autoload
(defun tcode-mazegaki-put-prefix ()
  "前置型交ぜ書き変換の開始地点の印を付ける。"
  (interactive)
  (setq tcode-mazegaki-prefix (point))
  (add-hook 'post-command-hook 'tcode-mazegaki-put-conversion-face))

;;;###autoload
(defun tcode-mazegaki-add-prefix (char)
  "前置型交ぜ書き変換を始めるためのフィルタ。"
  (tcode-mazegaki-put-prefix)
  char)

(defun tcode-mazegaki-self-insert-or-convert (arg)
  "コマンド `self-insert-command' と同じか、交ぜ書き変換を行う。
交ぜ書き変換を行うのは、「読み」が入力されている場合のみ。
変換を行えば t、行わなければ nil を返す。"
  (interactive "*P")
  (if (not tcode-mazegaki-prefix)
      (self-insert-command (prefix-numeric-value arg))
					; `self-insert-command' は nil を返す。
    (setq tcode-mazegaki-yomi-list (tcode-mazegaki-get-reverse-yomi-list))
    (tcode-mazegaki-convert (length tcode-mazegaki-yomi-list)
			    current-prefix-arg)
    t))			       ; 変換したとき t を返す。

(unless (featurep 'tc-mazegaki)
  (setq tcode-mazegaki-map (make-keymap))

  (tcode-set-key " " 'tcode-mazegaki-self-insert-or-convert)

  (mapc
   (lambda (elm)
     (define-key tcode-mazegaki-map (car elm) (cdr elm)))
   '((" "     . tcode-mazegaki-select-candidate-or-relimit)
     ("\C-u"  . tcode-mazegaki-restore-yomi-and-quit)
     ("\C-m"  . tcode-mazegaki-finish)
     ("<"     . tcode-mazegaki-relimit-left)
     (">"     . tcode-mazegaki-relimit-right)
     ("|"     . tcode-mazegaki-make-entry-and-finish)
     ("\C-\?" . tcode-mazegaki-backward-delete-char)
     ("="     . tcode-mazegaki-table-mode)
     ("\C-l"  . recenter)
     ("\C-b"  . tcode-mazegaki-recursive-convert-backward)
     ("\C-f"  . tcode-mazegaki-cancel-previous-recursive-convert)
     ("?"     . tcode-mazegaki-command-summary)))

  (run-hooks 'tcode-mazegaki-init-hook))

(provide 'tc-mazegaki)

;;; tc-mazegaki.el ends here
