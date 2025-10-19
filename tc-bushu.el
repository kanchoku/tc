;;; tc-bushu.el --- bushu conversion on T-Code

;; Copyright (C) 1996-2003 Kaoru Maeda, Yasushi Saito and Akira Kitajima.

;; Author: Kaoru Maeda <maeda@src.ricoh.co.jp>
;;	Yasushi Saito <yasushi@is.s.u-tokyo.ac.jp>
;;	Akira Kitajima <kitajima@isc.osakac.ac.jp>
;;	YAGI Tatsuya <ynyaaa@ybb.ne.jp>
;; Maintainer: Akira Kitajima
;; Created: 15 Sep 2001
;; Version: $Id: tc-bushu.el,v 2.51 2003/03/21 03:42:30 kitajima Exp $
;; Keywords: wp

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

(defcustom tcode-bushu-sequence-sensitive t
  "* nilでない場合、部首の並べ方によって合成される文字の優先度が変わる。"
  :type 'boolean :group 'tcode)

(defcustom tcode-bushu-prioritized-chars nil
  "* 優先度が同じ場合に優先される文字のリスト。
文字列で指定する。"
  :type 'string :group 'tcode)

(defvar tcode-bushu-inhibited-output-regexp
  "^\\({.*}\\|[えしへアイウエオカクケサシタチテトニヌネノハヒホムメヨリルレロワン]\\)$")

(defvar tcode-bushu-reverse-dictionary-name "bushu.rev"
  "逆引き部首合成辞書のファイル名")
(defconst tcode-bushu-reverse-buffer-name " *tcode: bushu reverse dictionary*")

(defvar tcode-bushu-expand-file-name "bushu.expand")
(defconst tcode-bushu-expand-buffer-name " *tcode: bushu expand*")

(defvar tcode-bushu-index2-file-name "bushu.index2")
(defconst tcode-bushu-index2-buffer-name " *tcode: bushu index2*")

(defvar tcode-bushu-help-dictionary-name "bushu.help"
  "部首合成変換ヘルプ辞書のファイル名")
(defconst tcode-bushu-help-buffer-name " *tcode: bushu help dictionary*")

(defvar tcode-bushu-functions
  '(tcode-bushu-compose-explicitly
    tcode-bushu-complete-compose-set
    tcode-bushu-complete-diff-set
    tcode-bushu-strong-compose-set
    tcode-bushu-strong-diff-set
    tcode-bushu-weak-diff-set
    tcode-bushu-weak-compose-set)
  "* A list of functions to apply to characters for a character composition.")

(defvar tcode-bushu-list nil)

(defvar tcode-bushu-use-cache t)

;;;
;;; 部首合成変換ための基本データ操作
;;;

(defun tcode-bushu-search (str)
  "現在のバッファ中の、 STR で始まる行のうち、最初のものを見つける。"
  (let ((min (point-min))
	(max (point-max))
	kouho)
    (or (catch 'found
	  (and (eobp)
	       (forward-line -1))
	  (while (< min max)
	    (cond ((string< (setq kouho
				  (buffer-substring (progn
						      (beginning-of-line)
						      (point))
						    (save-excursion
						      (end-of-line)
						      (point))))
			    str)
		   (forward-line 1)
		   (goto-char (ash (+ (setq min (point)) max) -1)))
		  ((string< str kouho)
		   (goto-char (ash (+ min (setq max (point))) -1)))
		  ((throw 'found t)))))
	(progn
	  (beginning-of-line)
	  (looking-at (regexp-quote str))))))

;;; (defun tcode-bushu-parse-entry ()
;;;   "現在の行を文字のリストとして返す。
;;; ポイントは行末に移動する。"
;;;   (if (eobp)
;;;       nil
;;;     (string-to-list
;;; 	     (buffer-substring (point)
;;; 			       (progn (end-of-line) (point))))))

(defun tcode-bushu-b2s (x)
  "漢字の構成部品(部首)を文字列に変換する。"
  (if (stringp x) x (char-to-string x)))

(defun tcode-bushu-bl2s (l)
  "漢字の構成部品(部首)のリストを文字列に変換する。"
  (mapconcat 'tcode-bushu-b2s l nil))

(defun tcode-bushu-parse-entry ()
  "現在の行を文字のリストとして返す。
ただし{}で囲まれた部分は文字列として返す。
ポイントは行末に移動する。"
  (if (eobp)
      nil
    (let (ret)
      (while (looking-at "\\([^{}\n]*\\)\\({[^{}\n]*}\\)")
	(setq ret (nconc ret (string-to-list (match-string 1))
			 (list (match-string 2))))
	(goto-char (match-end 0)))
      (nconc ret (string-to-list (buffer-substring
				  (point) (progn (end-of-line) (point))))))))

(defun tcode-bushu-for-char (char)
  "CHARを構成する部首のリストを返す。
CHARとして文字列も受け付ける。"
  (let* ((str (tcode-bushu-b2s char))
	 (cache (get (intern-soft str tcode-stroke-table) 'bushu)))
    (if (and cache
	     tcode-bushu-use-cache)
	(copy-sequence cache)
      (if (member char tcode-bushu-list)
	  (progn
	    (put (intern str tcode-stroke-table) 'bushu (list char))
	    (list char))
	(with-current-buffer (get-buffer tcode-bushu-expand-buffer-name)
	  (if (tcode-bushu-search str)
	      (let ((bushu-list (cdr (tcode-bushu-parse-entry))))
		(put (intern str tcode-stroke-table)
		     'bushu
		     bushu-list)
		(copy-sequence bushu-list))
	    (put (intern str tcode-stroke-table) 'bushu (list char))
	    (list char)))))))

(defun tcode-bushu-lookup-index2-entry-internal (str)
  (with-current-buffer (get-buffer tcode-bushu-index2-buffer-name)
    (when (tcode-bushu-search (concat str " "))
      (search-forward " ")
      (tcode-bushu-parse-entry))))

(defun tcode-bushu-lookup-index2-entry-1 (char)
  "CHARを部首として持つ文字のリストを返す。
返すリストにはCHARも含まれる。
CHARとして文字列も受け付ける。"
  (cons char (tcode-bushu-lookup-index2-entry-internal (tcode-bushu-b2s char))))

(defun tcode-bushu-< (char1 char2)
  "部首CHAR1とCHAR2の順序関係。
CHAR1, CHAR2として文字列も受け付ける。"
  (if (stringp char1)
      (if (stringp char2)
	  (string< char1 char2)
	t)
    (if (stringp char2)
	nil
      (< char1 char2))))

(defun tcode-bushu-lookup-index2-entry-2 (char char2)
  "CHARとCHAR2を部首として持つ文字のリストを返す。
CHARとして文字列も受け付ける。"
  (let ((str (if (tcode-bushu-< char char2)
		 (concat (tcode-bushu-b2s char) (tcode-bushu-b2s char2))
	       (concat (tcode-bushu-b2s char2) (tcode-bushu-b2s char)))))
    (tcode-bushu-lookup-index2-entry-internal str)))

(defun tcode-bushu-lookup-index2-entry-many (char n)
  "CHARをN個以上部首として持つ文字のリストを返す。"
  (if (= n 1)
      (tcode-bushu-lookup-index2-entry-1 char)
    (tcode-bushu-lookup-index2-entry-internal
     (if (stringp char)
	 (apply 'concat (make-list n char))
       (make-string n char))
     )))

(defun tcode-count (elt list)
  "LIST中のELTの数を返す。"
  (let ((n 0))
    (while list
      (if (equal elt (car list))
	  (setq n (1+ n)))
      (setq list (cdr list)))
    n))

(defun tcode-bushu-included-char-list (bushu &optional n)
  "BUSHU を N 個以上含む文字のリストを返す。N省略時は N = 1 とみなす。"
  (tcode-bushu-lookup-index2-entry-many bushu (or n 1)))

(defun tcode-bushu-included-set-p (list1 list2)
  "LIST1がLIST2に含まれる集合かどうかを表す述語。
同じ要素が複数ある場合は、LIST2に含まれる数の方が少なければnilを返す。"
  (let (x n (ret t))
    (while list1
      (setq x (car list1)
	    n (tcode-count x list1)
	    list1 (cdr list1))
      (if (> n (tcode-count x list2))
	  (setq ret nil
		list1 nil)))
    ret))

(defun tcode-bushu-same-set-p (list1 list2)
  "LIST1とLIST2が同じ集合かどうかを表す述語。
同じ要素が複数ある場合は、同じ数だけ含まれていないと等しいとはみなさない。"
  (and (eq (length list1) (length list2))
       (tcode-bushu-included-set-p list1 list2)))

(defun tcode-char-list-for-bushu (bushu-list)
  "BUSHU-LISTで構成される字の集合を求める。"
  ;; 長さ2以下の場合を特別扱いする。
  (cond ((null bushu-list) nil)
	((null (cdr bushu-list))
	 (let* ((bushu (car bushu-list))
		(included (tcode-bushu-included-char-list bushu))
		(ret nil) l)
	   (while included
	     (setq l (tcode-bushu-for-char (car included)))
	     (if (and (equal bushu (car l))
		      (null (cdr l)))
		 (setq ret (cons (car included) ret)))
	     (setq included (cdr included)))
	   (nreverse ret)))
	((null (nthcdr 2 bushu-list))
	 (let* ((bushu1 (car bushu-list))
		(bushu2 (nth 1 bushu-list))
		(included (tcode-bushu-lookup-index2-entry-2 bushu1 bushu2))
		(ret nil) l)
	   (while included
	     (setq l (tcode-bushu-for-char (car included)))
	     (if (or (and (equal bushu1 (car l))
			  (equal bushu2 (nth 1 l))
			  (null (nthcdr 2 l)))
		     (and (equal bushu2 (car l))
			  (equal bushu1 (nth 1 l))
			  (null (nthcdr 2 l))))
		 (setq ret (cons (car included) ret)))
	     (setq included (cdr included)))
	   (nreverse ret)))
	(t
	 (let* ((included (tcode-bushu-lookup-index2-entry-2
			   (car bushu-list) (nth 1 bushu-list)))
		(ret nil) l)
	   (while included
	     (if (tcode-bushu-same-set-p (tcode-bushu-for-char (car included))
					 bushu-list)
		 (setq ret (cons (car included) ret)))
	     (setq included (cdr included)))
	   (nreverse ret)))))

(defun tcode-uniq (list)
  (let* ((ret (copy-sequence list))
	 (l ret))
    (while l
      (setcdr l (delete (car l) (cdr l)))
      (setq l (cdr l)))
    ret))

;;;
;;; 部首合成変換用データの作成・ファイル操作
;;;

(defun tcode-bushu-add-to-index2 (char component)
  (with-current-buffer (get-buffer tcode-bushu-index2-buffer-name)
    (setq component (sort component 'tcode-bushu-<))
    (let ((l nil) bushu)
      (while component
	(setq bushu (car component)
	      component (cdr component)
	      l (cons (list bushu) l))
	(let ((tmp component) bushu2)
	  (while tmp
	    (setq bushu2 (car tmp)
		  tmp (cdr tmp)
		  l (cons (list bushu bushu2) l))
	    (when (equal bushu bushu2)
	      (let ((n 2))
		(while (equal bushu (car tmp))
		  (setq n (1+ n)
			component (cdr component)
			tmp (cdr tmp)))
		(if (> n 2)
		    (setq l (cons (make-list n bushu) l)))))
	    (while (equal bushu2 (car tmp))
	      (setq tmp (cdr tmp)))
	    ))
	(while (equal bushu (car component))
	  (setq component (cdr component)))
	)
      (while l
	(if (tcode-bushu-search (concat (tcode-bushu-bl2s (car l)) " "))
	    (unless (member char (nthcdr 3 (tcode-bushu-parse-entry)))
	      (insert char))
	  (apply 'insert (car l))
	  (insert ?\  char ?\n))
	(setq l (cdr l))))))

(defun tcode-bushu-make-index2 ()
  (tcode-set-work-buffer tcode-bushu-expand-buffer-name
			 tcode-bushu-expand-file-name)
  (let ((coding-system (and (boundp 'buffer-file-coding-system)
			    buffer-file-coding-system))
	(noe (count-lines (point-min) (point-max)))
	(count 0)
	(percent -1))
    (with-current-buffer (get-buffer-create tcode-bushu-index2-buffer-name)
      (erase-buffer)
      (or (not (boundp 'buffer-file-coding-system))
	  (set-buffer-file-coding-system coding-system)))
    (goto-char (point-min))
    (while (not (eobp))
      (when (and tcode-verbose-message
		 (/= percent (setq percent (/ (* 100 count) noe))))
	(message "部首合成辞書の拡張索引を作成中(%d%%)..." percent))
      (let ((entry (tcode-bushu-parse-entry)))
	(setq count (1+ count))
	(if entry
	    (tcode-bushu-add-to-index2 (car entry) (cdr entry))))
      (forward-line 1))
    (tcode-save-buffer tcode-bushu-index2-buffer-name
		       tcode-bushu-index2-file-name t)
    (when tcode-verbose-message
      (message "部首合成辞書の拡張索引を作成中(100%%)...完了"))))

(defun tcode-bushu-expand-add-entry (char component)
  (with-current-buffer (get-buffer tcode-bushu-expand-buffer-name)
    (if (not (tcode-bushu-search (tcode-bushu-b2s char)))
	(insert char (tcode-bushu-bl2s component) ?\n)
      (end-of-line)
      (insert (tcode-bushu-bl2s component)))))

(defun tcode-bushu-expand-char (char trace)
  (if (member char tcode-bushu-list)
      (list char)
    (let ((str (tcode-bushu-b2s char)))
      (with-current-buffer (get-buffer tcode-bushu-expand-buffer-name)
	(tcode-bushu-search str)
	(let ((entry (tcode-bushu-parse-entry)))
	  (if (and entry (equal char (car entry)))
	      ;; すでに展開済み
	      (cdr entry)
	    ;; 展開はまだ。
	    (set-buffer (get-buffer tcode-bushu-reverse-buffer-name))
	    (if trace
		(tcode-bushu-search str)
	      (beginning-of-line))
	    (let ((entry (tcode-bushu-parse-entry)))
	      (if (and entry (cdr entry) (equal char (car entry)))
		  ;; 展開できる
		  (let ((component
			 (apply 'nconc
				(mapcar
				 (lambda (bushu)
				   (if (member bushu trace)
				       ;; 循環している
				       (list ?⊥ bushu)
				     (tcode-bushu-expand-char
				      bushu
				      (cons bushu trace))))
				 (cdr entry)))))
		    (tcode-bushu-expand-add-entry char component)
		    component)
		;; 展開できない = 部首
		(setq tcode-bushu-list (cons char tcode-bushu-list))
		(list char)))))))))

;;; obsolete
(defun tcode-bushu-expand-all ()
  "各文字について、部首の集合を求める。"
  (tcode-set-work-buffer tcode-bushu-reverse-buffer-name
			 tcode-bushu-reverse-dictionary-name
			 t)
  (goto-char (point-min))
  (while (search-forward-regexp "[ \t]*;.*$" nil t)
    (delete-region (match-beginning 0) (match-end 0)))
  (goto-char (point-min))
  (while (search-forward-regexp "\n\n+" nil t)
    (delete-region (1+ (match-beginning 0)) (match-end 0)))
  (goto-char (point-min))
  (while (search-forward-regexp "^\\([^{}\n]\\|{[^{}\n]*}\\)+\\([^{}\n]\\|{[^{}\n]*}\\)3" nil t)
    (let ((str (match-string 2)))
      (goto-char (match-beginning 2))
      (delete-region (point) (match-end 2))
      (insert str str str)))
  (let ((bushu-expand-buf (get-buffer-create tcode-bushu-expand-buffer-name))
	(coding-system (and (boundp 'buffer-file-coding-system)
			    buffer-file-coding-system))
	(noe (count-lines (point-min) (point-max)))
	(count 0))
    (with-current-buffer bushu-expand-buf
      (erase-buffer)
      (or (not (boundp 'buffer-file-coding-system))
	  (set-buffer-file-coding-system coding-system)))
    (goto-char (point-min))
    (setq tcode-bushu-list nil)
    (while (not (eobp))
      (when tcode-verbose-message
	(message "部首合成辞書を展開中(%d%%)..." (/ (* 100 count) noe)))
      (let ((entry (tcode-bushu-parse-entry)))
	(setq count (1+ count))
	(if entry
	    (tcode-bushu-expand-char (car entry) nil)))
      (forward-line 1))
    (tcode-save-buffer tcode-bushu-expand-buffer-name
		       tcode-bushu-expand-file-name t)
    (when tcode-verbose-message
      (message "部首合成辞書を展開中(100%%)...完了"))))

(defun tcode-bushu-load-dictionary (&optional force)
  "部首合成変換辞書を読み込む。
すでに読み込まれている場合は何もしない。
FORCEがnilでない場合は再読み込みする。"
  (interactive "P")
  (save-excursion
    ;; load dictionaries
    (tcode-set-work-buffer tcode-bushu-expand-buffer-name
			   tcode-bushu-expand-file-name)
    (if (file-newer-than-file-p (tcode-path-for-read
				 tcode-bushu-expand-file-name)
				(tcode-path-for-read
				 tcode-bushu-index2-file-name))
	(tcode-bushu-make-index2)
      (tcode-set-work-buffer tcode-bushu-index2-buffer-name
			     tcode-bushu-index2-file-name))
    ;; make tcode-bushu-list
    (unless tcode-bushu-list
      (tcode-set-work-buffer tcode-bushu-index2-buffer-name
			     tcode-bushu-index2-file-name)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.\\|{[^{}\n]*}\\) " nil t)
	(beginning-of-line)
	(let ((bushu (match-string 1)))
	  (setq tcode-bushu-list (cons bushu tcode-bushu-list))
	  (forward-line 1)))
      (setq tcode-bushu-list (nreverse tcode-bushu-list)))))

;;; obsolete
(defun tcode-bushu-convert-dic-to-rev ()
  "現在のバッファにあるdic形式部首合成辞書データをrev形式に変換する。"
  (interactive)
  (let ((buf (get-buffer-create "*tcode: dic to rev*")))
    (with-current-buffer buf
      (erase-buffer))
    (goto-char (point-min))
    (setq tcode-bushu-list nil)
    (message "部首合成辞書を変換中...")
    (while (not (eobp))
      (let ((entry (tcode-bushu-parse-entry)))
	(if entry
	    (save-excursion
	      (let ((str (mapconcat 'char-to-string
				    (list (car (cdr (cdr entry)))
					  (car entry)
					  (car (cdr entry)))
				    nil)))
		(set-buffer buf)
		(tcode-bushu-search str)
		(insert str ?\n)))))
      (forward-line 1))
    (message "部首合成辞書を変換中...完了")
    (pop-to-buffer buf)))

;;;
;;; 部首合成変換用基本演算
;;;

(defun tcode-delete-first-element (elt list)
  "Delete first ELT in LIST with side effect."
  (if list
      (if (equal elt (car list))
	  (cdr list)
	(let ((l list))
	  (catch 'found
	    (while l
	      (when (equal elt (car (cdr l)))
		(setcdr l (cdr (cdr l)))
		(throw 'found t))
	      (setq l (cdr l))))
	  list))))

(defun tcode-intersection (list1 list2)
  "LIST1とLIST2との集合積を返す。
同じ要素が複数ある場合は区別する。
返り値における要素の並び方はLIST1の方に基づく。"
  (let ((l2 (copy-sequence list2))
	intersection)
    (while (and list1 l2)
      (let ((elt (car list1)))
	(when (member elt l2)
	  (setq intersection (cons elt intersection)
		l2 (tcode-delete-first-element elt l2)))
	(setq list1 (cdr list1))))
    (nreverse intersection)))

(defun tcode-complement-intersection (list1 list2)
  (if list2
      (let ((l1 (copy-sequence list1))
	    (l2 (copy-sequence list2))
	    ci)
	(while (and l1 l2)
	  (let* ((e (car l1))
		 (c1 (tcode-count e l1))
		 (c2 (tcode-count e l2))
		 (diff (abs (- c1 c2))))
	    (if (> diff 0)
		(setq ci (nconc ci (make-list diff e))))
	    (setq l1 (delete e l1)
		  l2 (delete e l2))))
	(nconc ci l1 l2))
    list1))

(defun tcode-subtract-set (list1 list2)
  (if list2
      (let ((l1 (copy-sequence list1))
	    (l2 (copy-sequence list2))
	    ci)
	(while (and l1 l2)
	  (let* ((e (car l1))
		 (c1 (tcode-count e l1))
		 (c2 (tcode-count e l2))
		 (diff (- c1 c2)))
	    (if (> diff 0)
		(setq ci (nconc ci (make-list diff e))))
	    (setq l1 (delq e l1)
		  l2 (delq e l2))))
	(nconc l1 ci))
    list1))

(defun tcode-bushu-superset (bushu-list)
  "部首の部分集合がBUSHU-LISTである字の集合を求める。"
  ;; 長さ2以下の場合を特別扱いする。
  (cond ((null bushu-list) nil) ;; ?
	((null (cdr bushu-list))
	 (tcode-bushu-included-char-list (car bushu-list)))
	((null (nthcdr 2 bushu-list))
	 (tcode-bushu-lookup-index2-entry-2 (car bushu-list)
					    (nth 1 bushu-list)))
	(t
	 (let* ((bushu (car bushu-list))
		(n (tcode-count bushu bushu-list))
		(included
		 (if (> n 1)
		     (progn
		       (setq bushu-list (delete bushu bushu-list))
		       (tcode-bushu-included-char-list bushu n))
		   (setq bushu-list (cdr bushu-list))
		   (tcode-bushu-lookup-index2-entry-2
		    bushu (nth 1 bushu-list))))
		(ret nil) l)
	   (while included
	     (if (tcode-bushu-included-set-p
		  bushu-list (tcode-bushu-for-char (car included)))
		 (setq ret (cons (car included) ret)))
	     (setq included (cdr included)))
	   (nreverse ret)))))

(defun tcode-bushu-higher-priority-p (bushu1 bushu2 ref default)
  "REFを基準として、BUSHU1の方がBUSHU2よりも並び方が基準に近いかどうか。
判断できなかったり、する必要がない場合はDEFAULTを返す。"
  (if tcode-bushu-sequence-sensitive
      (catch 'done
	(while (and ref bushu1 bushu2)
	  (let ((b1 (car bushu1))
		(b2 (car bushu2))
		(r (car ref)))
	    (cond ((and (equal r b1)
			(not (equal r b2)))
		   (throw 'done t))
		  ((and (not (equal r b1))
			(equal r b2))
		   (throw 'done nil))
		  ((and (not (equal r b1))
			(not (equal r b2)))
		   (throw 'done default)))
	    (setq bushu1 (cdr bushu1)
		  bushu2 (cdr bushu2)
		  ref (cdr ref))))
	default)
    default))

(defun tcode-bushu-priority-level (char)
  "CHARが変数`tcode-bushu-prioritized-chars'の何番目にあるかを返す。
なければ nil を返す。"
  (if (and tcode-bushu-prioritized-chars
	   (not (stringp char)))
      (let* ((priority-list
	      (string-to-list tcode-bushu-prioritized-chars))
	     (char-list (memq char priority-list)))
	(if char-list
	    (- (length priority-list) (length char-list) -1)))))

(defun tcode-easier-stroke-p (s1 s2)
  (if (= (length s1) (length s2))
      ;; とりあえず段だけ考慮。
      ;; ホームポジションや打ちやすさなど考慮するべき。
      (let ((evfunc (lambda (a)
		      (let ((v (/ a 10)))
			(if (>= v 3)
			    1
			  (ash v 1))))))
	(> (apply '+ (mapcar evfunc s1))
	   (apply '+ (mapcar evfunc s2))))
    (< (length s1) (length s2))))

(defvar bushu-list)
(defun tcode-bushu-less-p (char1 char2 &optional many)
  "CHAR1がCHAR2より優先度が高いか?
自由変数BUSHU-LISTで指定された部首リストを基準とする。
MANYがnilの場合、同じ優先度では、BUSHU-LISTに含まれない
部首の数が少ない方が優先される。
nilでない場合は多い方が優先される。"
  (let* ((bushu1 (tcode-bushu-for-char char1))
	 (bushu2 (tcode-bushu-for-char char2))
	 (i1 (tcode-intersection bushu1 bushu-list))
	 (i2 (tcode-intersection bushu2 bushu-list))
	 (l1 (- (length bushu1) (length i1)))
	 (l2 (- (length bushu2) (length i2))))
    (if (= (length i1) (length i2))
	(if (= l1 l2)
	    (let ((p1 (tcode-bushu-priority-level char1))
		  (p2 (tcode-bushu-priority-level char2)))
	      (cond (p1
		     (if p2
			 (< p1 p2)
		       t))
		    (p2
		     nil)
		    (t
		     (let ((val (tcode-bushu-higher-priority-p
				 i1 i2 (tcode-intersection
					bushu-list (nconc bushu1 bushu2))
				 'default)))
		       (if (not (eq val 'default))
			   val
			 (let ((s1 (if (stringp char1)
				       nil
				     (tcode-encode char1)))
			       (s2 (if (stringp char2)
				       nil
				     (tcode-encode char2))))
			   (cond ((and s1 s2)
				  (tcode-easier-stroke-p s1 s2))
				 (s1
				  t)
				 (s2
				  nil)
				 (t
				  (tcode-bushu-< char1 char2)))))))))
	  (if many
	      (> l1 l2)
	    (< l1 l2)))
      (> (length i1) (length i2)))))

(defun tcode-bushu-complete-compose-set (char-list)
  (let ((bushu-list (apply 'nconc (mapcar 'tcode-bushu-for-char char-list))))
    (sort (tcode-subtract-set (tcode-char-list-for-bushu bushu-list)
			      char-list)
	  'tcode-bushu-less-against-seqence-p)))

(defun tcode-bushu-strong-compose-set (char-list)
  (let* ((bushu-list (apply 'nconc (mapcar 'tcode-bushu-for-char char-list)))
	 (r (tcode-bushu-superset bushu-list)))
    (catch 'not-found
      (mapc (lambda (c)
	      (unless (setq r (delete c r))
		(throw 'not-found nil)))
	    char-list)
      (sort r 'tcode-bushu-less-p))))

(defun tcode-bushu-less-against-seqence-p (char1 char2)
  (let ((p1 (tcode-bushu-priority-level char1))
	(p2 (tcode-bushu-priority-level char2)))
    (cond (p1
	   (if p2
	       (< p1 p2)
	     t))
	  (p2
	   nil)
	  (t
	   (tcode-bushu-higher-priority-p (tcode-bushu-for-char char1)
					  (tcode-bushu-for-char char2)
					  bushu-list
					  (tcode-bushu-< char1 char2))))))

(defun tcode-bushu-include-all-chars-bushu-p (char char-list)
  (let* ((bushu (tcode-bushu-for-char char))
	 (new-bushu bushu))
    (mapc (lambda (char)
	    (setq new-bushu 
		  (tcode-subtract-set new-bushu
				      (tcode-bushu-for-char char))))
	  char-list)
    (setq bushu (tcode-subtract-set bushu new-bushu))
    (catch 'false
      (mapc (lambda (char)
	      (or (tcode-subtract-set 
		   bushu
		   (apply 'nconc 
			  (mapcar 
			   'tcode-bushu-for-char
			   (tcode-subtract-set char-list (list char)))))
		  (throw 'false nil)))
	    char-list)
      t)))

(defun tcode-bushu-all-compose-set (char-list &optional bushu-list)
  (let* ((char (car char-list))
	 (rest (cdr char-list))
	 (all-list 
	  (tcode-uniq
	   (delete char
		 (apply 'nconc
			(mapcar
			 (if rest
			     (lambda (bushu)
			       (tcode-bushu-all-compose-set rest
							    (cons bushu
								  bushu-list)))
			   (lambda (bushu)
			     (tcode-bushu-superset (cons bushu bushu-list))))
			 (tcode-bushu-for-char char)))))))
    (delq nil
	  (mapcar (lambda (char)
		    (if (tcode-bushu-include-all-chars-bushu-p char char-list)
			char))
		  all-list))))

(defun tcode-bushu-weak-compose-set (char-list)
  (when (cdr char-list) ;; char-list が一文字だけの時は何もしない
    (let ((bushu-list (apply 'nconc (mapcar 'tcode-bushu-for-char char-list))))
      (sort (tcode-subtract-set (tcode-bushu-all-compose-set char-list)
				(tcode-bushu-strong-compose-set char-list))
	    'tcode-bushu-less-p))))

(defun tcode-bushu-subset (bushu-list)
  (delq nil
	(mapcar
	 (lambda (char)
	   (unless (tcode-subtract-set (tcode-bushu-for-char char) bushu-list)
	       char))
	 (tcode-uniq (apply 'nconc
			    (mapcar 'tcode-bushu-included-char-list
				    (tcode-uniq bushu-list)))))))

(defun tcode-bushu-less-or-many-p (char1 char2)
  (tcode-bushu-less-p char1 char2 t))

(defun tcode-bushu-strong-diff-set (char-list &optional bushu-list complete)
  (let* ((char (car char-list))
	 (rest (cdr char-list))
	 (bushu (tcode-bushu-for-char char))
	 (i (if bushu-list
		(tcode-intersection bushu bushu-list)
	      bushu)))
    (if i
	(let ((d1 (tcode-complement-intersection bushu i))
	      (d2 (tcode-complement-intersection bushu-list i)))
	  (if (or (and d1 d2)
		  (and (null d1)
		       (null d2)))
	      nil
	    (if rest
		(delete char
			(tcode-bushu-strong-diff-set rest (or d1 d2) complete))
	      (sort (delete char (if complete
				     (tcode-char-list-for-bushu (or d1 d2))
				   (tcode-bushu-subset (or d1 d2)))) 
		    'tcode-bushu-less-or-many-p))))
      nil)))

(defun tcode-bushu-complete-diff-set (char-list)
  (tcode-bushu-strong-diff-set char-list nil t))

(defun tcode-bushu-all-diff-set (char-list &optional bushu-list common-list)
  (let* ((char (car char-list))
	 (rest (cdr char-list))
	 (bushu (tcode-bushu-for-char char))
	 (new-common-list (if common-list
			      (tcode-intersection bushu common-list)
			    bushu)))
    (if new-common-list
	(let* ((new-bushu-list
		(if common-list
		    (nconc bushu-list
			   (tcode-complement-intersection bushu
							  new-common-list)
			   (tcode-complement-intersection common-list
							  new-common-list)))))
	  (if rest
	      (delete char (tcode-bushu-all-diff-set rest
						     new-bushu-list
						     new-common-list))
	    (tcode-uniq
	     (delete char (apply 'nconc
				 (mapcar
				  (lambda (bushu)
				    (let ((cl (copy-sequence new-common-list)))
				      (tcode-bushu-subset
				       (append new-bushu-list
					       (delete bushu cl)))))
				  new-common-list))))))
      nil)))

(defun tcode-bushu-weak-diff-set (char-list)
  (let* ((bushu-list (tcode-bushu-for-char (car char-list)))
	 (diff-set (tcode-subtract-set 
		    (tcode-bushu-all-diff-set char-list)
		    (tcode-bushu-strong-diff-set char-list)))
	 (true-diff-set 
	  (delq nil
		(mapcar 
		 (lambda (char)
		   (if (tcode-subtract-set (tcode-bushu-for-char char)
					   bushu-list)
		       nil
		     char))
		 diff-set))))
    (tcode-uniq (nconc (sort true-diff-set 'tcode-bushu-less-or-many-p)
		       (sort (tcode-subtract-set diff-set true-diff-set)
			     'tcode-bushu-less-or-many-p)))))

(defun tcode-bushu-common-set (char-list)
  (let ((bushu-list (tcode-bushu-for-char (car char-list))))
    (catch 'not-found
      (mapc
       (lambda (c)
	 (unless (setq bushu-list
		       (tcode-intersection bushu-list
					   (tcode-bushu-for-char c)))
	   (throw 'not-found nil)))
       (cdr char-list))
      (let ((kouho (tcode-bushu-subset bushu-list)))
	(mapc (lambda (c)
		(if (memq c kouho)
		    (setq kouho (delq c kouho))))
	      char-list)
	(sort kouho 'tcode-bushu-less-or-many-p)))))

(defun tcode-bushu-compose-explicitly (char-list)
  (if (or (cdr (cdr char-list))
	  (null (nth 1 char-list))
	  (stringp (car char-list))
	  (stringp (nth 1 char-list)))
      ;; only for 2-char composition
      nil
    (save-excursion
      (tcode-set-work-buffer tcode-bushu-help-buffer-name
			     tcode-bushu-help-dictionary-name
			     nil t)
      (goto-char (point-min))
      (if (let* ((case-fold-search nil)
		 (c1 (regexp-quote (char-to-string (car char-list))))
		 (c2 (regexp-quote (char-to-string (nth 1 char-list))))
		 (reg (concat "\\(^.\\| \\)\\("
			      c1 c2 "\\*?\\|" c2 c1
			      "\\*\\)\\( \\|$\\)")))
	    (re-search-forward reg nil t))
	  (progn
	    (beginning-of-line)
	    (list (tcode-following-char)))))))

;;;
;;; 部首合成変換用インタフェース
;;;

;;;###autoload
(defun tcode-bushu-compose-two-chars (char1 char2)
  "CHAR1とCHAR2を合成する。"
  (tcode-bushu-load-dictionary)
  (let* ((str (concat (char-to-string char1)
		      (char-to-string char2)))
	 (cache (get (intern-soft str tcode-stroke-table) 'compose)))
    (or (and tcode-bushu-use-cache
	     cache)
	(let ((selected-char (tcode-bushu-compose (list char1 char2))))
	  (when selected-char
	    (put (intern str tcode-stroke-table) 'compose selected-char)
	    selected-char)))))

(defun tcode-bushu-funcall (func char-list)
  (let ((r (funcall func char-list)))
    (if tcode-bushu-inhibited-output-regexp
	(delq nil (mapcar (lambda (x)
			    (if (string-match
				 tcode-bushu-inhibited-output-regexp
				 (tcode-bushu-b2s x))
				nil
			      x))
			  r))
      r)))

(defun tcode-bushu-compose (char-list)
  "Compose a character from characters in CHAR-LIST.
See also `tcode-bushu-functions'."
  (catch 'found
    (mapc
     (lambda (function)
       (let ((r (tcode-bushu-funcall function char-list)))
	 (if r
	     (throw 'found (car r)))))
     tcode-bushu-functions)
    nil))

(defun tcode-bushu-compose-interactively (char-list)
  "CHAR-LISTをもとに対話的に合成する。"
  (tcode-bushu-load-dictionary)
  (let ((kouho-list (apply 'nconc (mapcar (lambda (function)
					    (tcode-bushu-funcall
					     function char-list))
					  tcode-bushu-functions))))
    (if kouho-list
	(tcode-bushu-select (tcode-uniq kouho-list) char-list)
      (ding))))

(defun tcode-bushu-scan-backward (max)
  "現 point より先頭方向にある日本語列または英単語一つのリストを返す。
リストの要素は(POINT . \"文字列\")である。
`tcode-scan-backward' と違い、英文字でも一文字の文字列で返す。
リストの順番としては、バッファの先頭に近い文字列が先頭の側になる。
リストの長さは最大 MAX 文字である。"
  (save-excursion
    (let (ch context)
      (while (and (< (length context) max)
		  (tcode-skip-blank-backward)
		  (setq ch (tcode-preceding-char))
		  (not (bobp)))
	(progn
	  (tcode-forward-char -1)
	  (setq context (cons (cons (point) (char-to-string ch))
			      context))))
      context)))

(defun tcode-bushu-convert-preceding-chars (&optional arg)
  "ポイントの前の2文字を合成する。"
  (interactive "*P")
  (tcode-bushu-init 2)
  (let ((context (tcode-bushu-scan-backward 2)))
    (if (/= (length context) 2)
	(ding)
      (let* ((prev-char (tcode-2-to-1 (tcode-string-to-char
				       (cdr (car (cdr context))))))
	     (prev-prev-char (tcode-2-to-1 (tcode-string-to-char
					    (cdr (car context)))))
	     (kanji (if (or arg current-prefix-arg)
			(progn
			  (setq prefix-arg nil
				current-prefix-arg nil)
			  (tcode-bushu-compose-interactively
			   (list prev-prev-char prev-char)))
		      (tcode-bushu-compose-two-chars prev-prev-char
						     prev-char)))
	     (p2 (car (car context))))
	(if kanji
	    (progn
	      (setq tcode-bushu-occurrence (1+ tcode-bushu-occurrence))
	      (unless (stringp kanji)
		(setq kanji (char-to-string kanji)))
	      (delete-region p2 (point))
	      (tcode-insert kanji)
	      (and tcode-auto-help
		   (tcode-display-direct-stroke kanji)
		   (tcode-auto-remove-help-char))
	      (setq tcode-help-char kanji))
	  (ding))))))

(defun tcode-bushu-prompt (char-list kouho-list)
  (let* ((1st-kouho (car kouho-list))
	 (rest-kouho (cdr kouho-list))
	 (msg (format "%s => %s"
		      (mapconcat 'char-to-string char-list nil)
		      (if (stringp 1st-kouho)
			  1st-kouho
			(char-to-string 1st-kouho))))
	 (rest-kouho-str (mapconcat (lambda (e) 
				      (if (stringp e)
					  e
					(char-to-string e)))
				    rest-kouho
				    " "))
	 (w (- (window-width (minibuffer-window))
	       5 (string-width msg) (string-width rest-kouho-str))))
    (message 
     (if rest-kouho
	 (if (> w 0)
	     (format "%s [%s]" msg rest-kouho-str)
	   (format (format "%%s [%%.%ds..."
			   (- (window-width (minibuffer-window))
			      5 (string-width msg)))
		   msg rest-kouho-str))
       msg))))

(defun tcode-bushu-select (kouho-list char-list)
  (tcode-bushu-prompt char-list kouho-list)
  (let ((char (read-char))
	(nok (length kouho-list))
	(current-kouho 0))
    (catch 'done
      (while (cond ((>= (tcode-char-to-key char) 0)
		    ;; インクリメンタル変換
		    (let* ((next-chars (tcode-input-method char))
			   (kouho (tcode-bushu-compose-interactively
				   (append char-list
					   (mapcar 'tcode-2-to-1
						   next-chars)))))
		      (if kouho
			  (throw 'done kouho))
		      t))
		   ((= char ?\r)
		    ;; 確定
		    (throw 'done (nth current-kouho kouho-list)))
		   ((or (= char ?\C-?)
			(= char ?\C-h))
		    ;; 一つ戻る
		    (throw 'done nil))
		   ((or (= char ? )
			(= char ?>))
		    ;; 次候補
		    (setq current-kouho (% (1+ current-kouho) nok)))
		   ((= char ?<)
		    ;; 前候補
		    (setq current-kouho (1- (if (<= current-kouho 0)
						nok
					      current-kouho))))
		   (t
		    (ding)))
	(tcode-bushu-prompt char-list (nthcdr current-kouho kouho-list))
	(setq char (read-char))))))

;;;
;;; 前置型部首合成変換
;;;

(defvar tcode-bushu-prefix-list nil
  "前置型部首合成変換の開始地点の印のリスト。")
(make-variable-buffer-local 'tcode-bushu-prefix-list)

(defun tcode-bushu-put-prefix ()
  "前置型部首合成変換の開始地点として印を付ける。"
  (tcode-bushu-init 2)
  (unless (get-buffer tcode-bushu-expand-buffer-name)
    (error "Bushu dictionary not ready."))
  (setq tcode-bushu-prefix-list (cons (set-marker (make-marker) (point))
				      tcode-bushu-prefix-list))
  (insert "▲")
  (cancel-undo-boundary)
  (setq this-command 'self-insert-command
	tcode-self-insert-non-undo-count 0))

(defun tcode-bushu-delete-prefix ()
  "前置型部首合成変換用の一番内側の印を削除する。"
  (when tcode-bushu-prefix-list
    (let ((p (car tcode-bushu-prefix-list)))
      (setq tcode-bushu-prefix-list (cdr tcode-bushu-prefix-list))
      (save-excursion
	(goto-char p)
	(when (looking-at "▲")
	  (delete-char 1))))))

(defun tcode-bushu-clear-prefix ()
  "前置型部首合成変換用の印をすべて削除する。"
  (while tcode-bushu-prefix-list
    (tcode-bushu-delete-prefix)))

(add-hook 'tcode-clear-hook 'tcode-bushu-clear-prefix)

(defun tcode-bushu-prefix-convert (arg)
  (if (stringp arg)
      (let ((l (string-to-list arg))
	    tmp ret)
	(while l
	  (setq tmp (tcode-bushu-prefix-convert-char (car l)))
	  (if tmp (setq ret (cons tmp ret)))
	  (setq l (cdr l)))
	(mapconcat (lambda (x) (if (stringp x) x (char-to-string x)))
		   (nreverse ret) ""))
    (tcode-bushu-prefix-convert-char arg)))

(defun tcode-bushu-prefix-convert-char (char)
  (if (or (null tcode-bushu-prefix-list)
	  (null char))
      char
    (let* ((context (tcode-bushu-scan-backward 2))
	   (car-context (car context))
	   (cadr-context (car (cdr context)))
	   (prev-char (cdr cadr-context))
	   (prev-prev-char (cdr car-context))
	   (p (car car-context)))
      (if (and prev-char
	       prev-prev-char
	       (string= "▲" prev-prev-char)
	       (memq p (mapcar 'marker-position tcode-bushu-prefix-list))
	       (not (string= prev-char "▲")))
	  (let ((kanji (tcode-bushu-compose-two-chars
			(tcode-string-to-char prev-char)
			char)))
	    (if kanji
		(progn
		  ;; 部首合成変換して入力
		  (delete-region p (point))
		  (while (and tcode-bushu-prefix-list
			      (/= p (marker-position
				     (car tcode-bushu-prefix-list))))
		    (tcode-bushu-delete-prefix))
		  (tcode-bushu-delete-prefix)
		  (setq tcode-bushu-occurrence (1+ tcode-bushu-occurrence)
			tcode-help-char char)
		  (if (stringp kanji)
		      (if (= (length kanji) 1)
			  (setq kanji (string-to-char kanji))))
		  (and tcode-auto-help
		       (tcode-display-direct-stroke (if (stringp kanji)
							kanji
						      (char-to-string kanji))))
		  (tcode-bushu-prefix-convert-char kanji))
	      ;; 変換できない。
	      (ding)
	      nil))
	;; 部首合成変換の材料
	char))))

;;;
;;; 部首合成変換開始コマンド
;;;

;;;###autoload
(defun tcode-bushu-begin-conversion (&optional interactive)
  "部首合成変換を開始する。"
  (interactive "*P")
  (if interactive
      (tcode-bushu-convert-interactively)
    (if tcode-use-postfix-bushu-as-default
	(tcode-bushu-convert-preceding-chars)
      (tcode-bushu-put-prefix))))

;;;###autoload
(defun tcode-bushu-convert-interactively ()
  (interactive "*")
  (tcode-bushu-convert-preceding-chars t))

;;;###autoload
(defun tcode-bushu-begin-alternate-conversion (&optional interactive)
  "`tcode-use-postfix-bushu-as-default' とは逆の部首合成変換を開始する。"
  (interactive "*P")
  (if interactive
      (tcode-bushu-convert-interactively)
    (if tcode-use-postfix-bushu-as-default
	(tcode-bushu-put-prefix)
      (tcode-bushu-convert-preceding-chars))))

;;;###autoload
(defun tcode-bushu-convert-preceding-char-interactively ()
  "ポイントの前の1文字をもとに対話的に部首合成する。"
  (interactive "*")
  (tcode-bushu-init 2)
  (let ((context (tcode-bushu-scan-backward 1)))
    (if (/= (length context) 1)
	(ding)
      (let* ((prev-char (tcode-string-to-char (cdr (car context))))
	     (kanji (tcode-bushu-compose-interactively (list prev-char)))
	     (p2 (car (car context))))
	(if kanji
	    (progn
	      (setq tcode-bushu-occurrence (1+ tcode-bushu-occurrence))
	      (unless (stringp kanji)
		(setq kanji (char-to-string kanji)))
	      (delete-region p2 (point))
	      (tcode-insert kanji)
	      (and tcode-auto-help
		   (tcode-display-direct-stroke kanji)
		   (tcode-auto-remove-help-char))
	      (setq tcode-help-char kanji))
	  (ding))))))

(provide 'tc-bushu)

;;; tc-bushu.el ends here
