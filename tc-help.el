;;; tc-help.el --- help routines for T-Code

;; Copyright (C) 1997-2001 Kaoru Maeda, Yasushi Saito and Akira Kitajima.

;; Author: Kaoru Maeda <maeda@src.ricoh.co.jp>
;;	Yasushi Saito <yasushi@cs.washington.edu>
;;	Akira Kitajima <kitajima@isc.osakac.ac.jp>
;; Maintainer: Akira Kitajima
;; Created: 3 Apr 1997
;; Version: $Id: tc-help.el,v 1.36 2003/03/21 03:44:48 kitajima Exp $

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

(require 'tc-bushu)

;;;; 変数

(defvar tcode-strict-help t
  "* non-nil のとき、部首変換により入力ができるかどうかを詳しく調ベる。
nil の場合では見つからないような場合でも、non-nil にすれば見つかる場合が
ある。ただし、かなり時間がかかることもある。")

(defvar tcode-help-first-stroke "●"
  "* ヘルプ表で第1打鍵位置を表す文字")
(defvar tcode-help-second-stroke "○"
  "* ヘルプ表で第2打鍵位置を表す文字")
(defvar tcode-help-third-stroke "△"
  "* ヘルプ表で第3打鍵位置を表す文字")
(defvar tcode-help-forth-stroke "◇"
  "* ヘルプ表で第4打鍵位置を表す文字")
(defvar tcode-help-double-stroke "◎"
  "* ヘルプ表で複数回この位置を押すことを表す文字")
(defvar tcode-help-another-double-stroke "☆"
  "* ヘルプ表で複数回この位置を押すことを表す文字。
`tcode-help-double-stroke' を既に使用している場合に使用する。")

(defvar tcode-help-draw-top-keys t
  "* ストローク表で最上段のキーを表示するか")

(defvar tcode-last-help-char-list nil
  "最後にヘルプを表示した文字のリスト")

(defvar tcode-help-with-real-keys nil
  "* nilでない場合、表形式ではなく、キー入力順形式によるへルプ表示を行う。")

(defvar tcode-stroke-to-string-opener ""
  "*`tcode-stroke-to-string'で使われる開始文字列")
(defvar tcode-stroke-to-string-separator "-"
  "*`tcode-stroke-to-string'で使われる分割文字列")
(defvar tcode-stroke-to-string-closer ""
  "*`tcode-stroke-to-string'で使われる終了文字列")

(defvar tcode-stroke-to-string-option
  (cond ((and (tcode-mule-4-p)
	      (> emacs-major-version 20)
	      (display-images-p))
	 (autoload 'tc-image-stroke-to-string "tc-image")
	 'tc-image-stroke-to-string)
	(t nil))
  "*`tcode-stroke-to-string'を制御する変数")

(defvar tcode-stroke-to-string-function nil
  "*漢直パッケージ毎に`tcode-stroke-to-string'を乗っ取る関数")

(defcustom tcode-help-stroke-hook nil
  "ヘルプ表を表示する前に実行されるフック。"
  :type 'hook :group 'tcode)

;;;
;;; モードのヘルプ
;;;

;;;###autoload
(defun tcode-mode-help ()
  "Tコードモードのキー割り当てなどを表示する。"
  (interactive)
  (let ((buf (get-buffer-create " *T-Code Mode Help*")))
    (set-buffer buf)
    (erase-buffer)
    (if tcode-mode-help-string
	(insert (substitute-command-keys tcode-mode-help-string))
      (insert (documentation 'tcode-activate))
      (goto-char (point-min))
      (forward-line 1)
      (narrow-to-region (point) (point-max)))
    (tcode-display-help-buffer buf)
    (kill-buffer buf)))

;;;
;;; 部首合成変換のへルプ
;;;

(defun tcode-another-stroke (c)
  (if tcode-another-table
      (let ((i (1- (length tcode-another-table))))
	(catch 'found
	  (while (>= i 0)
	    (let ((c2 (aref tcode-another-table i)))
	      (if (null c2)
		  (when (eq i (tcode-char-to-key c))
		    (setq c2 c))
		(when (symbolp c2)
		  (setq c2 (symbol-value c2)))
		(when (stringp c2)
		  (setq c2 (string-to-list c2)))
		(when (and (consp c2) (null (cdr c2)))
		  (setq c2 (car c2))))
	      (if (eq c2 c)
		  (throw 'found i)))
	    (setq i (1- i)))))
    (when (and (<= 32 c) (<= c ?~))
      (let ((key (tcode-char-to-key c)))
	(if (memq key '(-2 -3))
	    nil
	  key)))))

(defun tcode-direct-input-p (c)
  (or (let ((l (tcode-encode c))) (and l (length l)))
      (and (tcode-another-stroke c) 2)))

(defun tcode-bushu-help-lookup (str &optional norecursive)
  (save-excursion
    (tcode-set-work-buffer tcode-bushu-help-buffer-name
			   tcode-bushu-help-dictionary-name
			   nil t)
    (goto-char (point-min))
    (when (tcode-bushu-search str)
      (forward-char)
      (let (s1 s2 l1 l2 lis)
	(while (looking-at "\\(.\\)\\(.\\)\\*?\\( \\|$\\)")
	  (setq s1 (buffer-substring (match-beginning 1) (match-end 1))
		s2 (buffer-substring (match-beginning 2) (match-end 2)))
	  (goto-char (match-end 0))
	  (when (and (setq l1 (tcode-direct-input-p (string-to-char s1)))
		     (setq l2 (tcode-direct-input-p (string-to-char s2))))
	    (setq lis (cons (cons (+ l1 l2) (cons s1 s2)) lis))))
	(if lis
	    (cdr (car (sort (nreverse lis) 'car-less-than-car)))
	  (unless norecursive
	    (beginning-of-line)
	    (forward-char)
	    (when (looking-at "\\(.\\)\\(.\\)\\*?\\( \\|$\\)")
	      (setq s1 (buffer-substring (match-beginning 1) (match-end 1))
		    s2 (buffer-substring (match-beginning 2) (match-end 2)))
	      (unless (tcode-direct-input-p (string-to-char s1))
		(setq s1 (tcode-bushu-help-lookup s1 t)))
	      (unless (tcode-direct-input-p (string-to-char s2))
		(setq s2 (tcode-bushu-help-lookup s2 t)))
	      (and s1 s2 (cons s1 s2))
	      )))))))

(defun tcode-bushu-composed-p (char bushu1 bushu2)
  "CHARがBUSHU1とBUSHU2で合成できるか。
正確には、BUSHU1とBUSHU2は両方とも直接入力できないといけない。"
  (and (or (null tcode-strict-help)
	   (and (not (stringp bushu1)) (tcode-encode bushu1)
		(not (stringp bushu2)) (tcode-encode bushu2)))
       (let ((composed (tcode-bushu-compose-two-chars bushu1 bushu2)))
	 (and composed
	      (= composed (tcode-string-to-char char))))))

(defun tcode-decompose-to-two-char (char)
  (let* ((str (char-to-string char))
	 (b1 (tcode-bushu-for-char char))
	 (b2 (list (car b1))))
    (catch 'found
      (while (setq b1 (cdr b1))
	(let ((cl1 (or (tcode-char-list-for-bushu b2)
		       (if (= (length b2) 1)
			   b2)))
	      (cl2 (or (tcode-char-list-for-bushu b1)
		       (if (= (length b1) 1)
			   b1))))
	  (mapc (lambda (c1)
		  (unless (stringp c1)
		    (mapcar (lambda (c2)
			      (unless (stringp c2)
				(if (= (tcode-bushu-compose-two-chars c1 c2)
				       char)
				    (throw 'found (cons c1 c2)))))
			    cl2)))
		cl1))
	(setq b2 (nconc b2 (list (car b1))))))))

(defun tcode-decompose-char (kanji &optional for-help)
  "KANJIを二つの部首に分解する。
返り値は二つの字を `cons' したセル。
分解できない時にはnilを返す。
FOR-HELPがnilでない場合は、直接入力できる字に分解する。"
  (tcode-bushu-init 2)
  (save-excursion
    (tcode-set-work-buffer tcode-bushu-help-buffer-name
			   tcode-bushu-help-dictionary-name
			   nil t))
  (let* ((char (tcode-string-to-char kanji))
	 (decomposed (tcode-decompose-to-two-char char)))
    (if decomposed
	(if for-help
	    (let* ((char1 (car decomposed))
		   (char2 (cdr decomposed))
		   (str1 (char-to-string char1))
		   (str2 (char-to-string char2)))
	      (cond ((tcode-encode char1)
		     (if (tcode-encode char2)
			 (cons str1 str2)
		       (let ((bushu-list (tcode-bushu-for-char
					  (cdr decomposed))))
			 (catch 'found
			   ;; 強合成集合を探す。
			   (mapc
			    (lambda (c)
			      (if (tcode-bushu-composed-p kanji
							  (car decomposed)
							  c)
				  (throw 'found (cons str1
						      (char-to-string c)))))
			    (sort (tcode-bushu-subset bushu-list)
				  'tcode-bushu-less-p))
			   ;; 弱合成集合を探す。
			   (mapc
			    (lambda (c)
			      (if (tcode-bushu-composed-p kanji
							  (car decomposed)
							  c)
				  (throw 'found (cons str1
						      (char-to-string c)))))
			    (sort (tcode-bushu-superset bushu-list)
				  'tcode-bushu-less-p))
			   ;; 再帰的に探す。
			   (let ((dec2 (tcode-decompose-char str2 t)))
			     (if dec2
				 (cons str1 dec2)
			       (cons str1 (cons dec2 nil))))))))
		    ((tcode-encode char2)
		     (let ((bushu-list (tcode-bushu-for-char
					(car decomposed))))
		       (catch 'found
			 ;; 強合成集合を探す。
			 (mapc
			  (lambda (c)
			    (if (tcode-bushu-composed-p kanji
							c
							(cdr decomposed))
				(throw 'found (cons (char-to-string c)
						    str2))))
			  (sort (tcode-bushu-subset bushu-list)
				'tcode-bushu-less-p))
			 ;; 弱合成集合を探す。
			 (mapc
			  (lambda (c)
			    (if (tcode-bushu-composed-p kanji
							c
							(cdr decomposed))
				(throw 'found (cons (char-to-string c)
						    str2))))
			  (sort (tcode-bushu-superset bushu-list)
				'tcode-bushu-less-p))
			 ;; 再帰的に探す。
			 (let ((dec1 (tcode-decompose-char str1 t)))
			   (if dec1
			       (cons dec1 str2)
			     (cons (cons str1 nil) str2))))))
		    (t
		     (let* ((bushu1 (tcode-bushu-for-char (car decomposed)))
			    (bushu2 (tcode-bushu-for-char (cdr decomposed)))
			    (bushu-list (append bushu1 bushu2))
			    (cl1 (tcode-uniq
				  (sort
				   (nconc (tcode-bushu-subset bushu1)
					  (tcode-bushu-superset bushu1))
				   'tcode-bushu-less-p)))
			    (cl2 (tcode-uniq
				  (sort
				   (nconc (tcode-bushu-subset bushu2)
					  (tcode-bushu-superset bushu2))
				   'tcode-bushu-less-p))))
		       (catch 'found
			 (mapc
			  (lambda (c1)
			    (mapcar
			     (lambda (c2)
			       (if (tcode-bushu-composed-p kanji c1 c2)
				   (throw 'found
					  (cons (char-to-string c1)
						(char-to-string c2)))))
			     cl2))
			  cl1)
			 (let ((d1 (tcode-decompose-char str1 t))
			       (d2 (tcode-decompose-char str2 t)))
			   (cons (or d1 (cons str1 nil))
				 (or d2 (cons str2 nil)))))))))
	  (cons (char-to-string (car decomposed))
		(char-to-string (cdr decomposed))))
      ;; 二つに分割できない場合
      (if for-help
	  ;; 強差合成集合を探す。
	  (let ((bushu-list (tcode-bushu-for-char char)))
	    (catch 'found
	      (mapc
	       (lambda (c)
		 (when (tcode-encode c)
		   (let ((diff (tcode-subtract-set (tcode-bushu-for-char c)
						   bushu-list)))
		     (mapcar (lambda (d)
			       (if (tcode-bushu-composed-p kanji c d)
				   (throw 'found
					  (cons (char-to-string c)
						(char-to-string d)))))
			     (tcode-bushu-subset diff)))))
	       (sort (tcode-bushu-superset bushu-list)
		     'tcode-bushu-less-p))
	      (cons kanji nil)))))))

(defun tcode-stroke-prefix-match (stroke)
  (let ((l tcode-special-prefix-alist)
	stroke1 stroke2 ret)
    (while l
      (setq stroke1 (car (car l))
	    stroke2 stroke)
      (while (and stroke1 stroke2
		  (eq (car stroke1) (car stroke2)))
	(setq stroke1 (cdr stroke1)
	      stroke2 (cdr stroke2)))
      (if (null stroke1)
	  (setq ret (cons (car l) stroke2)
		l nil)
	(setq l (cdr l))))
    ret))

(defun tcode-stroke-to-string (stroke)
  "STROKE を表す短い文字列を返す。
`tcode-stroke-to-string-opener'で始まり、
`tcode-stroke-to-string-separator'を区切りとし、
`tcode-stroke-to-string-closer'で終わる。

`tcode-stroke-to-string-option'によりストロークを表す文字列が変わる。
  nil の時は2桁の数字で表す。
  t の時は tcode-mode でない時に入力される文字で表す。
  vector の時は要素を利用する。
  関数の時はその関数を呼び出す。

ただし`tcode-stroke-to-string-function'が設定されている時は
その関数を直接呼び出すので上記が成立するとは限らない。"
  (cond ((functionp tcode-stroke-to-string-function)
	 (let ((f tcode-stroke-to-string-function)
	       (tcode-stroke-to-string-function nil))
	   (funcall f stroke)))
	((functionp tcode-stroke-to-string-option)
	 (let ((f tcode-stroke-to-string-option)
	       (tcode-stroke-to-string-option nil))
	   (funcall f stroke)))
	(t
	 (concat (let ((dat (tcode-stroke-prefix-match stroke)))
		   (when dat
		     (setq stroke (cdr dat))
		     (nth 2 (car dat))))
		 tcode-stroke-to-string-opener
		 (mapconcat
		  (cond ((eq t tcode-stroke-to-string-option)
			 (lambda (addr) (char-to-string
					 (tcode-key-to-char addr))))
			((vectorp tcode-stroke-to-string-option)
			 (lambda (addr) (aref tcode-stroke-to-string-option
					      addr)))
			(t (lambda (addr) (format "%02d" addr))))
		  stroke tcode-stroke-to-string-separator)
		 tcode-stroke-to-string-closer))))

(defun tcode-help-make-bushu-structure-string (str root)
  (cond ((null str)
	 nil)
	((stringp str)
	 (if tcode-help-with-real-keys
	     (let ((strokes (tcode-encode (tcode-string-to-char str))))
	       (if strokes
		   (tcode-stroke-to-string strokes)
		 str))
	   str))
	((listp str)
	 (let ((left (tcode-help-make-bushu-structure-string (car str) nil))
	       (right (tcode-help-make-bushu-structure-string (cdr str) nil)))
	   (if right
	       (format (if root "%s + %s" "(%s + %s)") left right)
	     (concat "[" left "]"))))))

(defun tcode-help-bushu-help-string (ch for-help)
  "文字CHの、部首合成変換による打ち方を表す文字列を返す。"
  (let ((decomposed (or (tcode-bushu-help-lookup ch)
			(tcode-decompose-char ch for-help))))
    (and decomposed
	 (concat (tcode-help-make-bushu-structure-string decomposed t)))))

;;;
;;; ヘルプ表
;;;

(defun tcode-make-drawing-data (stroke)
  "STROKE の打ち方を図示するためのデータを生成する。
生成するデータは、次のリストのリスト。
  キーの番地(0-39)  印(○など)  順番を示す文字列(1など)"
  (let ((1st (car stroke))
	(2nd (car (cdr stroke)))
	(3rd (car (cdr (cdr stroke))))
	(4th (car (cdr (cdr (cdr stroke)))))
	draw-data)
    (setq draw-data
	  (list (cons 1st
		      (if (memq 1st (cdr stroke))
			  (list tcode-help-double-stroke
				(cond ((and 4th 3rd
					    (= 1st 2nd)
					    (= 1st 3rd)
					    (= 1st 4th))
				       (setq 2nd nil 3rd nil 4th nil)
				       "1、第2、第3、第4")
				      ((and 3rd (= 1st 2nd) (= 1st 3rd))
				       (setq 2nd nil 3rd nil)
				       "1、第2、第3")
				      ((and 4th 3rd (= 1st 2nd) (= 1st 4th))
				       (setq 2nd nil 4th nil)
				       "1、第2、第4")
				      ((and 4th 3rd (= 1st 3rd) (= 1st 4th))
				       (setq 3rd nil 4th nil)
				       "1、第3、第4")
				      ((and 4th 3rd (= 1st 4th))
				       (setq 4th nil)
				       "1、第4")
				      ((and 3rd (= 1st 3rd))
				       (setq 3rd nil)
				       "1、第3")
				      ((= 1st 2nd) (setq 2nd nil)
				       "1、第2")))
			(list tcode-help-first-stroke "1")))))
    (and 2nd
	 (setq draw-data
	       (nconc draw-data
		      (list (cons 2nd
				  (if (memq 2nd (cdr (cdr stroke)))
				      (list
				       (if (and (= (length stroke) 4)
						(or (null 3rd) (null 4th)))
					   tcode-help-another-double-stroke
					 tcode-help-double-stroke)
				       (cond ((and 3rd 4th
						   (= 2nd 3rd) (= 2nd 4th))
					      (setq 3rd nil 4th nil)
					      "2、第3、第4")
					     ((and 3rd (= 2nd 3rd))
					      (setq 3rd nil)
					      "2、第3")
					     ((and 4th (= 2nd 4th))
					      (setq 4th nil)
					      "2、第4")))
				    (list tcode-help-second-stroke "2")))))))
    (and 3rd
	 (setq draw-data
	       (nconc draw-data
		      (list (cons 3rd
				  (if (and 4th (= 3rd 4th))
				      (list
				       (if (null 2nd)
					   tcode-help-another-double-stroke
					 tcode-help-double-stroke)
				       (progn
					 (setq 4th nil)
					 "3、第4"))
				    (list tcode-help-third-stroke "3")))))))
    (and 4th
	 (setq draw-data
	       (nconc draw-data
		      (list (list 4th tcode-help-forth-stroke "4")))))
    draw-data))

(defun tcode-help-stroke (loc ch)
  "subroutine of `tcode-draw-stroke-for-char'."
  (forward-line (- (1+ (car loc)) (line-number-at-pos)))
  (move-to-column (+ (* 2 (cdr loc)) (if (>= (cdr loc) 6) 0 -2)))
  (tcode-delete-char (if (= (char-width (tcode-following-char)) 2) 
			 1 
		       2))
  (insert ch))

(defun tcode-get-key-location (address)
  (cons (/ address 10) (1+ (% address 10))))

(defun tcode-key-to-help-string (key)
  (let ((c (tcode-key-to-char key)))
    (if (= c 32) "SPC" (char-to-string c))))

(defun tcode-draw-stroke-for-char (stroke)
  "STROKE の打ち方を表す図を描く。"
  (let* ((data (tcode-stroke-prefix-match stroke))
	 (draw-data
	  (if (null data)
	      (tcode-make-drawing-data stroke)
	    (let ((tcode-help-first-stroke
		   (or (nth 3 (car data)) tcode-help-first-stroke))
		  (tcode-help-second-stroke
		   (or (nth 4 (car data)) tcode-help-second-stroke))
		  (tcode-help-double-stroke
		   (or (nth 5 (car data)) tcode-help-double-stroke))
		  (tcode-help-third-stroke
		   (or (nth 6 (car data)) tcode-help-third-stroke))
		  (tcode-help-forth-stroke
		   (or (nth 7 (car data)) tcode-help-forth-stroke))
		  (tcode-help-another-double-stroke
		   (or (nth 8 (car data)) tcode-help-another-double-stroke)))
	      (tcode-make-drawing-data (cdr data)))))
	(i 0))
    (insert "\
                      \n\
・・・・      ・・・・
・・・・      ・・・・
・・・・      ・・・・")
    (while draw-data
      (let* ((datum (car draw-data))
	     (addr (car datum))
	     (char (car (cdr datum)))
	     (str (car (cdr (cdr datum)))))
	(if (and (<= 0 addr) (< addr 40))
	    (tcode-help-stroke (tcode-get-key-location addr) char)
	  (setq char (tcode-key-to-help-string addr)))
	(forward-line (- (if (= (mod i 2) 0) 3 4) (line-number-at-pos)))
	(end-of-line)
	(insert "     " char "…第" str "打鍵")
	(setq i (1+ i)
	      draw-data (cdr draw-data))))))

;;;###autoload
(defun tcode-display-key-for-char (ch &optional not-display)
  "CH (1文字)の打ち方をキーの入力順で表示する。"
  (interactive "sHelp:\nP")
  (if (or (null ch)
	  (string= ch ""))
      (progn
	(tcode-verbose-message "ヘルプの文字がありません。")
	(ding))
    (let* ((strokes (tcode-encode (tcode-string-to-char ch)))
	   (msg (if strokes
		    (format "%s = %s" ch (tcode-stroke-to-string strokes))
		  (let ((decomposed-char (tcode-help-bushu-help-string
					  ch
					  (and tcode-strict-help 
					       (not strokes)))))
		    (format (if decomposed-char
				(concat ch " = " decomposed-char)
			      ch))))))
      (setq tcode-last-help-char-list (list (tcode-string-to-char ch)))
      (if not-display
	  msg
	(message "%s" msg)))))

;;;###autoload
(defun tcode-display-stroke-sequence (char-list &optional append)
  (if tcode-help-with-real-keys
      (message "%s"
	       (mapconcat (lambda (ch)
			    (tcode-display-key-for-char (char-to-string ch) t))
			  char-list 
			  "  "))
    (mapcar (lambda (ch)
	      (tcode-display-stroke-for-char (char-to-string ch)
					     append
					     t)
	      (setq append t))
	    char-list)))

;;;###autoload
(defun tcode-display-direct-stroke (kakutei &optional yomi append)
  "KAKUTEI の中で、 YOMI に含まれず、かつ直接入力できる漢字を表示する。"
  (when (not (window-minibuffer-p (selected-window)))
    (let* ((kakutei-list (string-to-list kakutei))
	   (yomi-list (string-to-list yomi))
	   display-list)
      (while kakutei-list
	(let ((ch (car kakutei-list)))
	  (if (and (not (memq ch yomi-list))
		   (tcode-encode ch))
	      (setq display-list (cons ch display-list)))
	  (setq kakutei-list (delq ch kakutei-list))))
      (setq display-list (nreverse display-list))
      (tcode-display-stroke-sequence display-list append)
      (setq tcode-last-help-char-list display-list))))

;;;###autoload
(defun tcode-display-stroke-for-char (ch &optional append recursive)
  "CH (1文字)の打ち方を表示する。"
  (interactive "sHelp:\nP")
  (run-hooks 'tcode-help-stroke-hook)
  (if tcode-help-with-real-keys
      (tcode-display-key-for-char ch)
    (if (or (null ch)
	    (string= ch ""))
	(progn
	  (tcode-verbose-message "ヘルプの文字がありません。")
	  (ding))
      (let* ((strokes (tcode-encode (tcode-string-to-char ch)))
	     (buf (get-buffer-create " *tcode: stroke*"))
	     decomposed-string)
	(with-current-buffer buf
	  (erase-buffer)
	  (and strokes
	       (<= (length strokes) 4)
	       (tcode-draw-stroke-for-char strokes))
	  (tcode-bushu-init 2)
	  (setq decomposed-string
		(if strokes
		    (concat "{"
			    (mapconcat 'tcode-bushu-b2s
				       (tcode-bushu-for-char
					(tcode-string-to-char ch))
				       ", ")
			    "}")
		  (tcode-help-bushu-help-string ch tcode-strict-help)))
	  (let ((help-string (if decomposed-string
				 (concat ch " = " decomposed-string)
			       (concat ch
				       " = {"
				       (mapconcat 'char-to-string
						  (tcode-bushu-for-char
						   (tcode-string-to-char ch))
						  ", ")
				       "}"))))
	    (if (null strokes)
		(message help-string)
	      (goto-char (point-min))
	      (end-of-line)
	      (insert "     " help-string)
	      (tcode-display-help-buffer " *tcode: stroke*" t append))))
	(if (and decomposed-string
		 (not recursive)
		 (not strokes))
	  (tcode-display-direct-stroke decomposed-string "()[]{}, +" strokes))
	(setq tcode-last-help-char-list (list (tcode-string-to-char ch)))))))

;;;###autoload
(defun tcode-query-stroke (p)
  "位置Pにある文字の打ち方を表示する。"
  (interactive "d")
  (let ((ch (buffer-substring (save-excursion
				(goto-char p)
				(tcode-forward-char 1)
				(point))
			      p)))
    (tcode-display-stroke-for-char ch)))

(defun tcode-auto-remove-help-char ()
  "`tcode-last-help-char-list'に含まれる文字を探して消去する。"
  (and (eq tcode-auto-help 'delete-the-char)
       (let ((chars (reverse tcode-last-help-char-list)))
	 (while chars
	   (search-backward (char-to-string (car chars)))
	   (delete-region (match-beginning 0)
			  (match-end 0))
	   (setq chars (cdr chars)))
	 (ding))))

;;;
;;; ストローク表
;;;

;;;###autoload
(defun tcode-show-tables (first second &optional prefix)
  (interactive)
  (unless prefix
    (let ((dat (tcode-stroke-prefix-match
		(mapcar 'tcode-char-to-key tcode-this-command-keys))))
      (if dat (setq prefix (car (car dat))))))
  (let* ((buf (tcode-draw-tables first second nil prefix)))
    (if tcode-auto-zap-table
	(save-window-excursion
	  (tcode-display-help-buffer buf)
	  (sit-for 0)
	  (tcode-redo-command (read-char)))
      (let ((orig-buf (current-buffer)))
	(set-buffer buf)
	(goto-char (point-min))
	(set-buffer orig-buf))
      (tcode-display-help-buffer buf))))

(defun tcode-make-table-line (k1 k2 &optional prefix)
  "subroutine of `tcode-make-LR-block'."
  (let ((i1 k1) (j 0) c)
    (while (< j 5)
      (let ((a (cdr (tcode-decode (append prefix (list i1 k2))))))
	(setq c (if (null a)
		    "■"
		  (tcode-action-to-printable a)))
	(insert c)
	(if (= (char-width (tcode-string-to-char c)) 1) (insert " ")))
      (setq j (1+ j)
	    i1 (1+ i1)))))

(defun tcode-make-LR-block (k1-start k2-start &optional prefix)
  "subroutine of `tcode-insert-stroke-file'."
  (or tcode-help-draw-top-keys
      (setq k1-start (+ k1-start 10)
	    k2-start (+ k2-start 10)))
  (let ((k1 k1-start)
	(k2 k2-start)
	y x yy)
    (setq yy (if tcode-help-draw-top-keys 0 1))
    (while (< yy 4)
      (setq y (if tcode-help-draw-top-keys 0 1))
      (setq k1 k1-start)
      (while (< y 4)
	(setq k2 k2-start)
	(insert "\n  ")
	(setq x 0)
	(while (< x 5)
	  (tcode-make-table-line k1 k2 prefix)
	  (insert "  ")
	  (setq x (1+ x))
	  (setq k2 (1+ k2)))
	(setq y (1+ y))
	(setq k1 (+ 10 k1)))
      (setq k2-start (+ 10 k2-start))
      (insert "\n")
      (setq yy (1+ yy)))))

(defun tcode-insert-stroke-file (&optional force)
  "Make the stroke table and writeout the content into the file.
The file is updated if it is older than tcode table."
  (erase-buffer)
  (if (and (not force)
	   (file-exists-p tcode-stroke-file-name))
      (insert-file-contents tcode-stroke-file-name)
    (insert "\nLL\n")
    (tcode-make-LR-block 0 0)
    (insert "\nLR\n")
    (tcode-make-LR-block 0 5)
    (insert "\nRL\n")
    (tcode-make-LR-block 5 0)
    (insert "\nRR\n")
    (tcode-make-LR-block 5 5)
    (mapc
     (lambda (x)
       (insert "\n" (nth 2 x) "LL\n") (tcode-make-LR-block 0 0 (car x))
       (insert "\n" (nth 2 x) "LR\n") (tcode-make-LR-block 0 5 (car x))
       (insert "\n" (nth 2 x) "RL\n") (tcode-make-LR-block 5 0 (car x))
       (insert "\n" (nth 2 x) "RR\n") (tcode-make-LR-block 5 5 (car x)))
     tcode-special-prefix-alist)
    (if (file-writable-p tcode-stroke-file-name)
	(write-file tcode-stroke-file-name))))

(defun tcode-draw-tables (first second &optional force prefix)
  "Draw tcode stroke tables.
FIRST corresponds to the first stroke.  If nil, then left, else right.
SECOND to the second stroke.  If nil, then left, else right.
If FORCE is non-nil, make new table."
  (let* ((dat (tcode-stroke-prefix-match prefix))
	 (buf (get-buffer-create tcode-stroke-buffer-name))
	 (str (concat "^" (if dat (regexp-quote (nth 2 (car dat))))
		      (if first "R" "L") (if second "R" "L"))))
    (with-current-buffer buf
      (widen)
      (goto-char (point-min))
      (if (and (not force)
	       (re-search-forward str nil t))
	  (progn
	    (forward-line 0)
	    (let ((top (point))
		  (end
		   (progn
		     (forward-line 3)
		     (if (re-search-forward "^[RL]" nil t)
			 (progn(forward-line -1)(point))
		       (point-max)))))
	      (narrow-to-region top end)
	      buf))
	(tcode-insert-stroke-file force)
	(tcode-draw-tables first second nil prefix)))))

(provide 'tc-help)

;;; tc-help.el ends here
