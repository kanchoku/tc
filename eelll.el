;;; eelll.el --- EELLL (ELisp implemented CATTT)

;; Copyright (C) 1991--2002  Kaoru Maeda, Yasushi Saito and Akira Kitajima

;; Author: Kaoru Maeda <maeda@src.ricoh.co.jp>
;;	Yasushi Saito <yasushi@is.s.u-tokyo.ac.jp>
;;	Akira Kitajima <kitajima@isc.osakac.ac.jp>
;;      YAGI Tatsuya <ynyaaa@ybb.ne.jp>
;; Maintainer: Akira Kitajima

;; $Id: eelll.el,v 1.29 2003/05/18 08:37:08 kitajima Exp $

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

(require 'tc)
(require 'tc-bushu)
(require 'tc-help)

;;  Version
(defun eelll-version ()
  "EELLL のバージョンを表示する。"
  (interactive)
  (if (called-interactively-p 'interactive)
      (message "EELLL version %s" (eelll-version))
    (substring "$Revision: 1.29 $" 11 -2)))

;;;
;;;  Variable
;;;

(defgroup eelll nil
  "EELLL (A T-Code trainer)."
  :group 'tcode)

(defcustom eelll-expert nil
  "Tコードの上級者かどうか。"
  :type 'boolean :group 'eelll)

(defcustom eelll-display-help-threshold 10
  "ヘルプの表示/非表示を切り替えるエラー率のしきい値(%)。"
  :type 'integer :group 'eelll)

(defvar eelll-text "EELLLTXT"
  "EELLLの練習テキストファイル")

(defvar eelll-move-cursor nil
  "*non-nilにするとキー入力のたびにカーソルを進める。")

(defvar eelll-configuration-file-name (concat tcode-data-directory
					      "eelll-conf.el")
  "*EELLLの設定ファイル名。
このファイルは自動的に書き換えられるので、
ユーザーは内容を変更してはいけない。")

(defvar eelll-last-lesson nil
  "最後に練習したレッスン番号。")

(defvar eelll-last-lesson-alist nil
  "テキストのファイル名と最後に練習したレッスン番号のalist。")

(defconst eelll-buffer-name "*EELLL*"
  "EELLLで練習するバッファ名")

(defconst eelll-text-buffer " *eelll: text*"
  "EELLLの練習テキストを入れておくバッファ名")

(defconst eelll-current-text-buffer " *eelll: current text*"
  "EELLLで練習中のテキストを入れておくバッファ名")

(defvar eelll-help-remove-interval 25
  "*EELLLの説明を消すまでの時間(秒)")

(defconst eelll-help-buffer-name " *EELLL-Help*"
  "EELLLで打ち方を表示するバッファ名")

(defvar eelll-help-window-margin
  (if (and (>= emacs-major-version 21)
	   window-system)
      3
    1)
  "*EEELLの打ち方を表示するときのウィンドウの高さのマージン")

(defvar eelll-by-text-dummy-char "・"
  "*直接入力できない文字の代わりに入力させる文字列")

(defvar eelll-by-text-max-line 30)
(defvar eelll-by-text-fill-column 60)

(when (and (fboundp 'defface)
	   window-system)
    (defface eelll
      (if (featurep 'meadow)
	  '((t (:font "default"
		      :foreground "black"
		      :background "white")))
	'((t (:font "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-*-*"
		    :foreground "black"
		    :background "white"))))
      "*一行ヘルプを使った EELLL でのヘルプのフェイス"
      :group 'eelll))

(defvar eelll-previous-error-rate 0)

(defvar eelll-original-window-configuration nil)
(defvar eelll-window-configuration nil)

(defvar eelll-total-time 0)
(defvar eelll-total-stroke 0)
(defvar eelll-total-error 0)
(defvar eelll-lesson-no nil)
(defvar eelll-first-hand nil)
(defvar eelll-second-hand nil)
(defvar eelll-prefix-stroke nil)
(defvar eelll-upper-row nil)
(defvar eelll-lesson-chars nil)
(defvar eelll-text-line nil)
(defvar eelll-start-time nil)
(defvar eelll-strokes 0)
(defvar eelll-error-strokes 0)
(defvar eelll-lesson-string nil)
(defvar eelll-lesson-pattern-string nil)
(defvar eelll-key-table nil)

(defvar eelll-mode-hook nil
  "*EELLLを実行するときに実行されるフック。")

(defvar eelll-exit-hook (if (featurep 'frame)
			    'eelll-remove-frame))
;;; for `eelll-other-frame'
(defvar eelll-frame-parameters 
  (if (featurep 'meadow)
      '((name . "EELLL")
	(font . "default"))
    '((name . "EELLL")
      (font . "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-*-*")))
  "*`eelll-other-frame'で用いるフレームのパラメータ。")

(defvar eelll-bushu-stroke-alist
  '((tutcode  20 28 20)
    (tcode    26 23))
  "部首合成を表すストローク")

(defvar eelll-use-column-for-help
  (or (not window-system)
      (> 22 emacs-major-version))
  "一行ヘルプを表示する時に桁揃えをカラム単位で行うか否か。
この変数の値が non-nil で emacs22 を window-system で使っている場合に
ピクセル単位の計算をする。")

(defvar eelll-frame nil)

(defvar eelll-comment-line nil)
(defvar eelll-current-text nil)
(defvar eelll-random-mode nil)
(defvar eelll-random-max-line 4
  "*EELLLのランダムモードで1回に選択される行数の最大値。")

;;;
;;;  Text
;;;

(defun eelll-random-text (max-line)
  "現在のバッファの行をランダムに並べ替える。
最大MAX-LINEまで残す。"
  (let ((lines (count-lines (point-min) (point-max)))
	line-list)
    (goto-char (point-min))
    (while (search-forward-regexp "^;" nil t)
      (end-of-line)
      (insert "\r")
      (delete-char 1))
    (goto-char (point-min))
    (while (and (> max-line 0)
		(> lines 0))
      (goto-line (1+ (random lines)))
      (if (eolp)
	  (progn
	    (unless (eobp)
	      (delete-char 1))
	    (setq lines (1- lines)))
	(let ((point-eol (save-excursion (end-of-line) (point))))
	  (setq line-list (cons (buffer-substring (point) point-eol) line-list)
		max-line (1- max-line)
		lines (1- lines))
	  (delete-region (point) (1+ point-eol)))))
    (erase-buffer)
    (insert (mapconcat 'identity line-list "\n") "\n")
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (delete-char -1)
      (insert "\n"))
    ))

(defun eelll-prepare-text (num)
  "練習テキストNUMを用意する。NUMがnilならば次のレッスンを用意する。"
  (save-excursion
    (tcode-set-work-buffer eelll-text-buffer eelll-text)
    (widen)
    (goto-char (point-min))
    (or (if num
	    (re-search-forward
	     (concat "\f\nLesson \\(" (int-to-string num) "\\):") nil t)
	  (and (or (null eelll-last-lesson)
		   (re-search-forward (concat "\f\nLesson \\("
					      (int-to-string eelll-last-lesson)
					      "\\):")
				      nil t))
	       (re-search-forward "\f\nLesson \\([0-9]+\\):" nil t)))
	(error "練習テキスト%sはありません。" (if num (int-to-string num) "")))
    (setq eelll-lesson-string (buffer-substring (match-beginning 1)
						(match-end 1))
	  eelll-lesson-no (string-to-number eelll-lesson-string))
    (setq eelll-prefix-stroke
	  (let ((l tcode-special-prefix-alist) ret)
	    (while l
	      (if (not (looking-at (concat (regexp-quote (nth 2 (car l)))
					   "\\([RrLl]\\)\\([RrLl]\\)"
					   "\\(!?\\)$")))
		  (setq l (cdr l))
		(goto-char (match-beginning 1))
		(setq ret (car (car l))
		      l nil)
		))
	    ret))
    (setq eelll-first-hand (looking-at "[Rr]"))
    (setq eelll-second-hand (looking-at ".[Rr]"))
    (setq eelll-upper-row (looking-at "..!"))
    (forward-line 1)
    (if (looking-at "^Lesson-chars: ")
	(let ((eol (save-excursion (end-of-line 1) (point))))
	  (skip-chars-forward "^ " eol)
	  (setq eelll-lesson-chars (buffer-substring (1+ (point)) eol))
	  (forward-line 1)))
    (let ((text (buffer-substring (point)
				  (save-excursion
				    (forward-page 1)
				    (1- (point))))))
      (set-buffer (get-buffer-create eelll-current-text-buffer))
      (erase-buffer)
      (insert text)
      (eelll-decompose-chars)
      (if eelll-random-mode
	  (eelll-random-text eelll-random-max-line))
      (goto-char (point-min))))
  (setq eelll-last-lesson eelll-lesson-no))

(defun eelll-decompose-chars ()
  (goto-char (point-min))
  (let (c stroke cell (str "▲"))
    (while (not (eobp))
      (if (looking-at "^;\\|$")
	  (forward-line)
	(setq c (tcode-following-char)
	      stroke (or (eelll-tcode-encode c)
			 (eelll-tcode-another-stroke (char-to-string c))))
	(if stroke
	    (forward-char)
	  (delete-char 1)
	  (setq cell (or (tcode-bushu-help-lookup (char-to-string c))
			 (tcode-decompose-char (char-to-string c) t)))
	  (if (not (and (consp cell)
			(stringp (car cell))
			(stringp (cdr cell))))
	      (insert eelll-by-text-dummy-char)
	    (if tcode-use-postfix-bushu-as-default
		(insert (car cell) (cdr cell) str)
	      (insert str (car cell) (cdr cell)))))))))

(defun eelll-lesson-line ()
  "練習テキストの次の行をとってくる。終わりならnilを返す。
eelll-text-line:	印字イメージ"
  (with-current-buffer eelll-current-text-buffer
    (skip-chars-forward " \t\n\f" (point-max))
    (let ((p (point)))
      (while (looking-at "^;")
	(forward-line 1))
      (setq eelll-comment-line (buffer-substring p (point))))
    (and (not (eobp))
	 (let ((p (point)))
	   (forward-line 1)
	   (setq eelll-text-line (buffer-substring p (1- (point))))
	   eelll-text-line))))

(defun eelll-tcode-encode (ch)
  (if (eq ch (tcode-string-to-char "▲"))
      (or (cdr (assq tcode-input-method eelll-bushu-stroke-alist))
	  '(26 23))
    (tcode-encode ch)))

(defun eelll-tcode-another-stroke (ch)
  (let ((i (1- (length tcode-another-table))))
    (catch 'found
      (while (>= i 0)
	(let ((c (aref tcode-another-table i)))
	  (if (string= ch (if (and c (symbolp c))
			      (eval c)
			    c))
	      (throw 'found i)))
	(setq i (1- i))))))

(defun eelll-stroke-for-char (ch)
  "CH(全角一文字)の打ち方を返す。Tコードで入力できなければnilを返す。"
  (let* ((char (tcode-string-to-char ch))
	 (strokes (eelll-tcode-encode char))
	 (dat (tcode-stroke-prefix-match strokes)))
    (when eelll-prefix-stroke
      (if (equal eelll-prefix-stroke (car (car dat)))
	  (setq strokes (cdr dat))
	(setq strokes nil)))
    (if (and strokes
	     (= (length strokes) 2))
	strokes
      nil)))

(defun eelll-inputs-for-char (ch)
  "CH (全角1文字)の打ち方(文字列のリスト)を返す。
Tコードで入力できなければnilを返す。"
  (let ((strokes (eelll-tcode-encode (tcode-string-to-char ch))))
    (cond (strokes
	   (mapcar (lambda (key)
		     (aref eelll-key-table key))
		   strokes))
	  (tcode-another-table
	   (list (aref eelll-key-table (eelll-tcode-another-stroke ch))
		 ? ))
	  ((string= ch " ")
	   (list ? )))))

;;;
;;;  Stroke chart
;;;
(defun eelll-draw-chart ()
  "練習テキストの練習対象となる文字のストローク表を作る。"
  (with-current-buffer (get-buffer-create eelll-help-buffer-name)
    (widen)
    (erase-buffer)
    (goto-char (point-min))
    (let ((i 0)
	  j k
	  (c (if eelll-second-hand 1 4)))
      (while (< i 4)
	(setq i (1+ i) j 0)
	(while (< j 4)
	  (insert "    ")
	  (setq j (1+ j) k 0)
	  (while (< k 5)
	    (setq k (1+ k))
	    (insert "－－－－－  ")
	    (if (= k c) (insert "  ")))
	  (delete-horizontal-space)
	  (insert "\n"))
	(if (< i 4) (insert "\n"))))
    (mapcar
     (lambda (c)
       (let ((stroke (eelll-stroke-for-char (char-to-string c))))
	 (when (and stroke
		    (= (length stroke) 2))
	   (let* ((second (car (cdr stroke)))
		  (fc (% (car stroke) 5))
		  (fr (/ (car stroke) 10))
		  (sc (% second 5))
		  (sr (/ second 10)))
	     (goto-line (+ (* sr 5) fr 1))
	     (move-to-column (+ 4 (* 12 sc) (* 2 fc)
				(if (> sc (if eelll-second-hand 0 3)) 2 0)))
	     (tcode-delete-char 1)
	     (insert (char-to-string c))))))
     (string-to-list eelll-lesson-chars))
    (goto-char (point-min))
    (forward-line 1)
    (delete-region (point-min) (point))
    (unless eelll-upper-row
      (forward-line 5)
      (delete-region (point-min) (point))
      (forward-line 4)
      (delete-region (point)
		     (save-excursion (forward-line 1)
				     (point)))
      (forward-line 4)
      (delete-region (point)
		     (save-excursion (forward-line 1)
				     (point))))
    (setq eelll-lesson-pattern-string
	  (concat (when eelll-prefix-stroke
		    (nth 2 (car (tcode-stroke-prefix-match
				 eelll-prefix-stroke))))
		  (if eelll-first-hand "R" "L")
					      "->"
					      (if eelll-second-hand "R" "L")))
    (set-buffer-modified-p nil)))

;;;
;;;  String Matching
;;;
(defun eelll-subsequence (seq n)
  ;; return first N elements of SEQ
  (let ((seq (copy-sequence seq)))
    (if (> (length seq) n)
	(setcdr (nthcdr (1- n) seq) nil))
    (if (<= n 0)
	nil
      seq)))

(defun eelll-match (string quest)
  "QUESTをお手本として入力された文字列STRINGの採点をする。
2要素のリスト(RESULT ERROR)を返す。"
  (let* ( ;; template は ((文字 . ストローク数) . ストローク) のリスト
	 (template (delq nil
			 (mapcar
			  (lambda (c)
			    (let ((stroke (eelll-inputs-for-char
					   (char-to-string c))))
			      (and stroke
				   (cons (cons c (length stroke))
					 stroke))))
			  (string-to-list quest))))
	 ;; template-length は template の長さ
	 (template-length (length template))
	;; vtemplate は (文字 . ストローク数) のベクター
	 (vtemplate (vconcat (mapcar 'car template)))
	 ;; key-list は入力された文字のリスト
	 (key-list (string-to-list string))
	 (key-length (length string))
	 (link (list '(0 . 0)))
	 (last (1- (length string)))
	 (j 0))
    (while template
      (let ((correct (mapconcat 'char-to-string (cdr (car template)) nil))
	    (correct-length (cdr (car (car template))))
	    (inputted key-list))
	(while inputted
	  (when (string= correct
			 (mapconcat 'char-to-string
				    (eelll-subsequence inputted correct-length)
				    nil))
	    (let* ((l link)
		   (next-input (nthcdr correct-length inputted))
		   (next-i (- key-length (length next-input)))
		   (next-template (cdr template))
		   (next-j (- template-length (length next-template)))
		   pair)
	      (catch 'finish
		(while l
		  (if (and (>= (- key-length (length inputted))
			       (car (car l)))
			   (>= (- template-length (length template))
			       (cdr (car l))))
		      (throw 'finish nil)
		    (setq pair (car l)
			  l (cdr l)))))
	      (if pair
		  (let ((dif (+ (- (car pair) next-i) (- (cdr pair) next-j))))
		    (when (or (> dif 0)
			      (and (= dif 0)
				   (> next-i (car pair))))
		      (setcar pair next-i)
		      (setcdr pair next-j)))
		(setq link (cons (cons next-i next-j) link)))))
	  (setq inputted (cdr inputted))))
      (setq template (cdr template)))
    (let* ((res (vconcat (string-to-list string)))
	   (pi (length string))
	   (pj template-length)
	   (err 0)
	   (l link)
	   (i (car (car l))))
      (while (> i 0)
	(let* ((j (cdr (car l)))
	       (correct (aref vtemplate (1- j))))
	  (aset res (- i 1) (car correct))
 	  (let ((n (1- (cdr correct))))
 	    (while (>= n 1)
 	      (aset res (- i 1 n) nil)
 	      (setq n (1- n))))
	  (setq err (+ err (max (- pi i) (- pj j)) (- (cdr correct))))
	  (setq pi i
		pj j
		l (cdr l)
		i (car (car l)))))
      (list (mapconcat (lambda (c) (if c (char-to-string c)))
		       res nil)
	    (+ err (max pi pj))))))

;;;
;;;  Mode setup
;;;

(defvar eelll-mode-map nil
  "EELLL モードで使うキーマップ")
(if eelll-mode-map
    ()
  (setq eelll-mode-map (make-keymap))
  (substitute-key-definition nil 'eelll-undefined eelll-mode-map)
  (define-key eelll-mode-map "\177" 'eelll-delete-char)
  (define-key eelll-mode-map "\e" 'ESC-prefix)
  (define-key eelll-mode-map "\C-c" 'mode-specific-command-prefix)
  (define-key eelll-mode-map "\C-j" 'eelll-return)
  (define-key eelll-mode-map "\C-m" 'eelll-return)
  (define-key eelll-mode-map "\C-g" 'eelll-confirm-quit)
  (define-key eelll-mode-map "\C-l" 'eelll-redisplay)
  (define-key eelll-mode-map "\C-u" 'universal-argument)
  (define-key eelll-mode-map "\C-x" 'Control-X-prefix)
  (define-key eelll-mode-map "\C-]" 'abort-recursive-edit)
  (define-key eelll-mode-map "?" 'eelll-help)
  (define-key eelll-mode-map " " 'eelll-key))

(defun eelll-mode ()
  "EELLL は Emacs Lisp で実現されたTコード練習プログラムです。
画面に表示された文字列をそのまま入力してください。
1行の入力が終わったらリターンキーを打ってください。

画面の上半分には今のレッスンで習う文字のストローク表が表示されています。

EELLL 内ではほとんどのコマンドが禁止されています。
まず \\[switch-to-buffer] で他のバッファに移ってから\
コマンドを実行してください。
なお、\\[eelll-confirm-quit] で EELLL を中断します。"
  (use-local-map eelll-mode-map)
  (setq major-mode 'eelll-mode)
  (setq mode-name "EELLL")
  (setq mode-line-format '("-----EELLL"
			   (eelll-lesson-string
			    (": lesson " eelll-lesson-string))
			   (eelll-random-mode "(random)")
			   "%-"))
  (run-hooks 'eelll-mode-hook))

(defun eelll-help ()
  (interactive)
  (if tcode-help-with-real-keys
      (progn
	(forward-line -2)
	(while (looking-at "^;")
	  (forward-line -1))
	(insert "\n")
	(eelll-insert-line-help eelll-text-line))
    (setq eelll-upper-row t)
    (if (null eelll-second-hand)
	(setq eelll-second-hand t
	      eelll-first-hand eelll-first-hand)
      (setq eelll-second-hand nil
	    eelll-first-hand (not eelll-first-hand)))
    (let ((eelll-lesson-chars (eelll-select-chars eelll-text-line))
	  (cur (selected-window)))
      (eelll-draw-chart)
      (delete-other-windows)
      (split-window-vertically
       (with-current-buffer eelll-help-buffer-name
	 (setq mode-line-format
	       '("-----EELLL Help"
		 (eelll-lesson-string
		  (": lesson " eelll-lesson-string))
		 (eelll-lesson-pattern-string
		  ("  (" eelll-lesson-pattern-string ")"))
		 "%-"))
	 (+ (count-lines (point-min) (point-max)) 
	    eelll-help-window-margin)))
      (switch-to-buffer eelll-help-buffer-name)
      (other-window 1))
    (setq eelll-window-configuration (current-window-configuration)))
  (eelll-redisplay))

(defun eelll-select-chars (text)
  (let ((ret nil))
    (mapcar (lambda (c)
	      (let* ((stroke (eelll-stroke-for-char (char-to-string c)))
		     (1st (car stroke))
		     (2nd (car (cdr stroke)))
		     (lks '(0 1 2 3 4
			      10 11 12 13 14
			      20 21 22 23 24
			      30 31 32 33 34))
		     (rks '(5 6 7 8 9
			      15 16 17 18 19
			      25 26 27 28 29
			      35 36 37 38 39)))
		(and (memq 1st (if eelll-first-hand rks lks))
		     (memq 2nd (if eelll-second-hand rks lks))
		     (setq ret (cons c ret)))))
	    (string-to-list text))
    (if ret
	(mapconcat 'char-to-string ret nil)
      "")))

(defun eelll-undefined ()
  (interactive)
  (message (substitute-command-keys
	    "\\[switch-to-buffer] SOME-BUFFER first.")))

(defun eelll-delete-char ()
  (interactive)
  (message "間違いを気にせずどんどん入力してください。"))

;;;
;;; 専用の入力補完付きのcompleting-read
;;;
;;; "?"で一覧が見れます。

(defun eelll-completing-read ()
  "入力補完付きで、練習テキスト番号をミニバッファから入力する。
Emacs内部のcompletionの実装上の問題のため、「?」を
入力した時にしか一覧は見えない。"
  (unless (featurep 'tcode-ready)
    (tcode-use-package (or tcode-current-package
			   tcode-default-input-method))
    (tcode-inactivate))
  (load eelll-configuration-file-name t)
  (if current-prefix-arg
      (setq eelll-text
	    (completing-read "text for eelll: "
			     (mapcar 'list
				     (nconc (and tcode-data-directory
						 (directory-files
						  tcode-data-directory
						  nil "^EELLLTXT"))
					    (directory-files
					     tcode-site-data-directory
					     nil "^EELLLTXT")))
			     nil t (or eelll-text "EELLLTXT"))))
  (if (not (equal eelll-current-text eelll-text))
      (let ((config (assoc eelll-text eelll-last-lesson-alist)))
	(setq eelll-last-lesson (if config
				    (cdr config)
				  nil))))
  (let (lesson-alist key val orig-minibuffer-completion-help)
    (save-excursion
      (if (and (not (equal eelll-current-text eelll-text))
	       (get-buffer eelll-text-buffer))
	  (kill-buffer eelll-text-buffer))
      (tcode-set-work-buffer eelll-text-buffer eelll-text)
      (setq eelll-current-text eelll-text)
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "^Lesson \\([0-9]+\\):.*[rRlL]+!?$" nil t)
	(setq key (buffer-substring (match-beginning 1) (match-end 1)))
	(forward-line 2)
	(setq val (buffer-substring (point)
				    (save-excursion (end-of-line) (point))))
	(setq lesson-alist (cons (cons key val) lesson-alist))))
    (setq orig-minibuffer-completion-help
	  (symbol-function 'minibuffer-completion-help))
    (unwind-protect
	(progn
	  (fset 'minibuffer-completion-help
		(symbol-function 'eelll-minibuffer-completion-help))
	  (let* ((hist (mapcar 'car lesson-alist))
		 (pos (if eelll-last-lesson
			  (- (length hist)
			     (length (member (format "%d" eelll-last-lesson)
					     hist))
			     -1)))
		 (str (cond ((tcode-xemacs-p)
			     (completing-read
			      "練習テキスト[`?'で一覧] "
			      lesson-alist nil t
			      (if eelll-last-lesson
				  (format "%d" eelll-last-lesson)
				"")
			      (if pos (cons 'hist (list pos)) (list 'hist))))
			    (t
			     (completing-read 
			      "練習テキスト[`?'で一覧] "
			      lesson-alist nil t
			      (if eelll-last-lesson
				  (format "%d" eelll-last-lesson)
				"")
			      (if pos (cons 'hist pos) 'hist))))))
	    (if (string= str "")
		nil
	      (list (string-to-number str)))))
      (fset 'minibuffer-completion-help orig-minibuffer-completion-help))))

(defun eelll-minibuffer-completion-help ()
  "EELLL用の補完候補を表示する。
`minibuffer-completion-help'を置き換える。"
  (interactive)
  (with-output-to-temp-buffer "*Completions*"
    (eelll-display-completion-list
     (all-completions (save-excursion
			(buffer-substring (progn (beginning-of-line) (point))
					  (progn (end-of-line) (point))))
			minibuffer-completion-table))))

(defun eelll-display-completion-list (x)
  "練習テキスト一覧を表示する。
`display-completion-list'を置き換える。"
  (princ "    ---- 練習テキスト一覧 ----\n")
  (setq x (sort x (lambda (x y)
		    (< (string-to-number x) (string-to-number y)))))
  (while x
    (princ (car x))
    (princ ":")
    (princ (cdr (assoc (car x) lesson-alist)))
    (princ "\n")
    (setq x (cdr x))))

;;;; 一行ヘルプの表示

(defun eelll-insert-with-face (str)
  (let ((beg (point)))
    (insert str)
    (when (and (fboundp 'put-text-property)
	       window-system)
      (put-text-property beg (point) 'face 'eelll-face))))

(defun eelll-put-help-char (c)
  (if (or eelll-use-column-for-help
	  (not window-system)
	  (> 22 emacs-major-version))
      (eelll-put-help-char-by-column c)
    (eelll-put-help-char-by-pixel c)))

(defun eelll-put-help-char-by-column (c)
  (let* ((ch (char-to-string c))
	 (stroke (eelll-tcode-encode c))
	 (another (eelll-tcode-another-stroke ch))
	 (s (cond (stroke
		   (tcode-stroke-to-string stroke))
		  (another (concat (let ((tcode-stroke-to-string-closer
					  tcode-stroke-to-string-separator))
				     (tcode-stroke-to-string (list another)))
				   "SPC"))
		  (t "-- --")))
	 (ex-col (string-width s)))
    (if (<= (+ help-column (max ex-col 5) 1)
	    (* (frame-width) 0.80))
	(setq help-column (+ help-column (max ex-col 5) 1))
      (insert "\n\n")
      (setq help-column (1+ (max ex-col 5))))
    (forward-line -1)
    (end-of-line)
    (if (>= ex-col 5)
	(eelll-insert-with-face s)
      (setq ex-col (- 5 ex-col))
      (eelll-insert-with-face (concat (make-string (- ex-col (/ ex-col 2)) 32)
				      s
				      (make-string (/ ex-col 2) 32)))
      (setq ex-col 5))
    (eelll-insert-with-face "|")
    (forward-line)
    (end-of-line)
    (if (equal ch " ")
	(setq ch "SPC"))
    (setq ex-col (- ex-col (string-width ch)))
    (eelll-insert-with-face (concat (make-string (- ex-col (/ ex-col 2)) 32)
				    ch
				    (make-string (/ ex-col 2) 32)))
    (eelll-insert-with-face "|")
    ))

(defun eelll-put-help-char-by-pixel (c)
  (let* ((ch (char-to-string c))
	 (stroke (eelll-tcode-encode c))
	 (another (eelll-tcode-another-stroke ch))
	 (s (cond (stroke
		   (tcode-stroke-to-string stroke))
		  (another (concat (let ((tcode-stroke-to-string-closer
					  tcode-stroke-to-string-separator))
				     (tcode-stroke-to-string (list another)))
				   "SPC"))
		  (t "-- --")))
	 (p1 (progn (forward-line -1) (end-of-line) (point))) p1e p2 p2e
	 sw cw dw)
    (if (equal ch " ") (setq ch "SPC"))
    (narrow-to-region (point) (point))
    (set-window-start (selected-window) (point))
    (eelll-insert-with-face s)
    (eelll-insert-with-face "|")
    (setq p1e (point))
    (setq sw (car (pos-visible-in-window-p nil nil t)))
    (widen)
    (forward-line)
    (end-of-line)
    (setq p2 (point))
    (narrow-to-region (point) (point))
    (set-window-start (selected-window) (point))
    (eelll-insert-with-face ch)
    (eelll-insert-with-face "|")
    (setq p2e (point))
    (setq cw (car (pos-visible-in-window-p nil nil t)))
    (widen)
    (setq dw (max cw sw))
    (if (<= (+ help-column dw)
	    (* (let ((l (window-inside-pixel-edges))) (- (nth 2 l) (car l)))
	       0.90))
	(setq help-column (+ help-column dw))
      (let (s1 s2)
	(setq s2 (buffer-substring p2 (point)))
	(delete-region p2 (point))
	(setq s1 (buffer-substring p1 p1e))
	(delete-region p1 p1e)
	(goto-char p1)
	(forward-line 2)
	(setq p1 (point))
	(insert s1 "\n")
	(setq p1e (1- (point)))
	(setq p2 (point))
	(insert s2 "\n")
	(setq p2e (1- (point)))
	(forward-char -1)
	(setq help-column dw)))
    (let* ((w (abs (- cw sw)))
	   (padl (/ w 2))
	   (padr (- w padl)))
      (when (< 0 padr)
	(goto-char (1- (if (<= cw sw) p2e p1e)))
	(insert 32)
	(put-text-property (1- (point)) (point)
			   'display `(space :width (,padr))))
      (when (< 0 padl)
	(goto-char (if (<= cw sw) p2 p1))
	(insert 32)
	(put-text-property (1- (point)) (point) 
			   'display `(space :width (,padl))))
      )
    (goto-char p2e)
    (end-of-line)))

(defun eelll-insert-line-help (string)
  (or (bolp) (insert "\n"))
  (let ((chars (string-to-list string))
	(help-column 0)
	(beg (progn 
	       (insert "\n\n")
	       (forward-char -1)
	       (point))))
    (mapcar 'eelll-put-help-char chars)
    (forward-line)))

;;;
;;;  Main
;;;

(defun eelll-init ()
  (tcode-use-package (or tcode-current-package
			 tcode-default-input-method))
  (tcode-inactivate)
  (unless eelll-key-table
    ;; initialize `eelll-key-table' and `eelll-keymap-table'
    (let ((n (1- (length tcode-key-translation-rule-table))))
      (setq eelll-key-table (make-vector 80 nil))
      (while (>= n 0)
	(let ((key (aref tcode-key-translation-rule-table n)))
	  (when (>= key 0)
	    (aset eelll-key-table key (+ n ? ))
	    (define-key eelll-mode-map (char-to-string (+ n ? )) 'eelll-key))
	  (setq n (1- n))))))
  (setq eelll-original-window-configuration (current-window-configuration))
  (setq eelll-previous-error-rate (if eelll-expert 0 100))
  (setq eelll-current-text eelll-text)
  (if (not (stringp lesson))
      (eelll-prepare-text lesson)
    (setq eelll-current-text nil
	  eelll-lesson-string "Temporary"
	  eelll-lesson-no 0
	  eelll-lesson-chars "")
    (with-current-buffer (get-buffer-create eelll-current-text-buffer)
      (erase-buffer)
      (insert lesson)
      (if eelll-random-mode
	  (eelll-random-text eelll-random-max-line))
      (eelll-normalize-text-buffer)))
  (eelll-setup-lesson)
  (message (substitute-command-keys "\\[eelll-help] でヘルプ")))

(defun eelll-normalize-text-buffer ()
  ;; delete white space
  (goto-char (point-min))
  (while (re-search-forward "[ \t]+" nil t)
    (replace-match ""))
  ;; delete null line
  (goto-char (point-min))
  (while (re-search-forward "\n\n+" nil t)
    (replace-match "\n"))
  (goto-char (point-min))
  (let ((line 0) c stroke cell (str "▲")
	(bol-kinsoku
	 (string-to-list ")!?、。，．・：；？！゛゜´｀¨＾\
‾＿ヽヾゝゞ〃仝々〆〇ー—‐\
／＼〜‖｜…‥’”）〕］｝〉》」』】°′″℃\
ぁぃぅぇぉっゃゅょゎァィゥェォッャュョヮヵヶ"))
	(eol-kinsoku (string-to-list "(‘“（〔［｛〈《「『【°′″℃＠§")))
    (while (and (< line eelll-by-text-max-line)
		(not (eobp)))
      (setq c (tcode-following-char)
	    stroke (or (eelll-tcode-encode c)
		       (eelll-tcode-another-stroke c)))
      (when (and (bolp) (not (bobp))
		 (memq c bol-kinsoku))
	(delete-char -1))
      (if (or stroke (eolp))
	  (if (eolp) (delete-char 1) (forward-char))
	(delete-char 1)
	(setq cell (or (tcode-bushu-help-lookup (char-to-string c))
		       (tcode-decompose-char (char-to-string c) t)))
	(if (not (and (consp cell)
		      (stringp (car cell))
		      (stringp (cdr cell))))
	    (insert eelll-by-text-dummy-char)
	  (if tcode-use-postfix-bushu-as-default
	      (insert (car cell) (cdr cell) str)
	    (insert str (car cell) (cdr cell)))))
      (when (and (< eelll-by-text-fill-column (current-column))
		 (not (memq c eol-kinsoku)))
	(insert ?\n)
	(setq line (1+ line)))))
  (or (bolp) (insert ?\n))
  (delete-region (point) (point-max))
  (goto-char (point-min)))

;;;###autoload
(defun eelll (&optional lesson text)
  "EELLL を始める。詳しくは `eelll-mode' の解説を見よ。"
  (interactive (eelll-completing-read))
  (if text (setq eelll-text text))
  (setq eelll-random-mode nil)
  (eelll-init))

;;;###autoload
(defun eelll-random (&optional lesson text)
  "ランダムモードで EELLL を始める。
指定したテキストの中からランダムに
数行(`eelll-random-max-line'で指定した行数)選択される。"
  (interactive (eelll-completing-read))
  (if text (setq eelll-text text))
  (setq eelll-random-mode t)
  (eelll-init))

;;;###autoload
(defun eelll-region (beg end)
  "リージョン内のテキストで EELLL を始める。"
  (interactive "r")
  (unless (featurep 'tcode-ready)
    (tcode-use-package (or tcode-current-package
			   tcode-default-input-method))
    (tcode-inactivate))
  (load eelll-configuration-file-name t)
  (let ((lesson (buffer-substring beg end)))
    (setq eelll-random-mode nil)
    (eelll-init)))

(defun eelll-setup-lesson ()
  (eelll-draw-chart)
  (switch-to-buffer eelll-buffer-name)
  (buffer-disable-undo)
  (eelll-mode)
  (widen) 
  (erase-buffer)
  (delete-other-windows)
  (if tcode-help-with-real-keys
      nil
    (when (>= eelll-previous-error-rate eelll-display-help-threshold)
      (split-window-vertically
       (with-current-buffer eelll-help-buffer-name
	 (setq mode-line-format
	       '("-----EELLL Help"
		 (eelll-lesson-string
		  (": lesson " eelll-lesson-string))
		 (eelll-lesson-pattern-string
		  ("  (" eelll-lesson-pattern-string ")"))
		 "%-"))
	 (+ (count-lines (point-min) (point-max)) 
	    eelll-help-window-margin)))
      (switch-to-buffer eelll-help-buffer-name)
      (goto-char (point-min))
      (other-window 1)))
  (setq eelll-window-configuration (current-window-configuration))
  (setq eelll-start-time nil
	eelll-error-strokes 0
	eelll-strokes 0)
  (insert "リターンキーを打てば始まります。 "))

(defun eelll-key ()
  (interactive)
  (with-current-buffer " *eelll: strokes*"
    (insert (char-to-string last-command-event)))
  (if eelll-move-cursor
      (insert " ")))

(defun eelll-return ()
  (interactive)
  (if eelll-start-time
      (progn
	(delete-region (point) (progn (beginning-of-line 1) (point)))
	(let* ((str (with-current-buffer " *eelll: strokes*"
		      (buffer-string)))
	       (res (eelll-match str eelll-text-line))
	       (err (car (cdr res))))
	  (insert (car res) "\n")
	  (let ((wrong-chars (tcode-uniq 
			      (tcode-subtract-set
			       (string-to-list eelll-text-line)
			       (string-to-list (car res))))))
	    (when wrong-chars
	      (insert "\n[間違えた字]")
	      (if tcode-help-with-real-keys
		  (eelll-insert-line-help
		   (mapconcat 'char-to-string wrong-chars nil))
		(insert "=> " (mapconcat 'char-to-string wrong-chars nil)
			"\n"))))
	  (setq eelll-strokes (+ eelll-strokes (length str))
		eelll-error-strokes (+ eelll-error-strokes err))))
    (setq eelll-start-time (eelll-current-time)))
  (if (null (eelll-lesson-line))
      (eelll-end-lesson)
    (with-current-buffer (get-buffer-create " *eelll: strokes*")
      (widen) (erase-buffer))
    (when tcode-help-with-real-keys
      (insert "\n")
      (if (>= eelll-previous-error-rate eelll-display-help-threshold)
	  (eelll-insert-line-help eelll-text-line)))
    (insert "\n" eelll-comment-line eelll-text-line "\n")
    (eelll-redisplay)))

(defun eelll-redisplay ()
  "EELLL の画面を表示し直す。"
  (interactive)
  (set-window-configuration eelll-window-configuration)
  (goto-char (point-max))
  (recenter -1))

(defun eelll-current-time ()
  (let ((str (current-time-string)))
    (string-match "\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)" str)
    (+ (* 3600 (string-to-number (substring str
					 (match-beginning 1)
					 (match-end 1))))
       (* 60 (string-to-number (substring str (match-beginning 2) (match-end 2))))
       (string-to-number (substring str (match-beginning 3) (match-end 3))))))

(defun eelll-percentage (num den)
  (let ((res%  (min 9999 (/ num (max 1 den)))))
    (format "%d.%d" (/ res% 10) (% res% 10))))

(defun eelll-stroke-per-min (stroke sec)
  "1分あたりのストローク数を表した文字列を返す。"
  (let ((stroke-per-min (/ (* (float stroke) 60) sec)))
    (format (if (< stroke-per-min 10) "%.1f" "%.f") stroke-per-min)))

(defun eelll-time-per-stroke (time stroke)
  "1ストロークあたりの時間を表した文字列を返す。"
      (if (>= time stroke)
	  (format "%.1f" (/ (float time) stroke))
	(format "%.f ミリ" (/ (* (float time) 1000) stroke))))

(unless (fboundp 'float)
  ;; float の扱えない Emacs (NEmacs) の場合
  ;; オーバーフローしないように桁をずらす。

  (defun eelll-stroke-per-min (stroke sec)
    "1分あたりのストローク数を表した文字列を返す。"
    (while (>= stroke 10000)
      (setq sec (/ sec 2)
	    stroke (/ stroke 2)))
    (setq stroke (* stroke 600))
    (let ((10stroke-per-min (/ stroke sec)))
      (if (< 10stroke-per-min 100)
	  (format "%d.%1d" (/ 10stroke-per-min 10) (% 10stroke-per-min 10))
	(format "%d" (/ 10stroke-per-min 10)))))

  (defun eelll-time-per-stroke (time stroke)
    "1ストロークあたりの時間を表した文字列を返す。"
    (if (>= time stroke)
	(let ((10time-per-stroke (/ (* time 10) stroke)))
	  (format "%d.%d " (/ 10time-per-stroke 10) (% 10time-per-stroke 10)))
      (while (>= time 8000)
	(setq time (/ time 2)
	      stroke (/ stroke 2)))
      (let ((1000time-per-stroke (/ (* time 1000) stroke)))
	(format "%d ミリ" 1000time-per-stroke)))))

(defun eelll-display-score (time stroke err)
  (if (> stroke 0)
      (let ((cnt (- stroke err)))
	(set-buffer eelll-buffer-name)
	(insert "\n\n(総打鍵成績)毎打鍵 "
		(eelll-time-per-stroke time stroke) "秒  "
		"毎分 " (eelll-stroke-per-min stroke time) " 打鍵\n")
	(if (> cnt 0)
	    (insert "(実打鍵成績)毎打鍵 "
		    (eelll-time-per-stroke time cnt) "秒  "
		    "毎分 " (eelll-stroke-per-min cnt time) " 打鍵\n"))
	(setq eelll-previous-error-rate (if (> stroke 0)
					    (/ (* 100 err) stroke)
					  100))
	(insert "              エラーレート "
		(eelll-percentage (* 1000 err) stroke)
		"%\n\n"))))

(defun eelll-end-lesson ()
  (let ((time (% (- (eelll-current-time) eelll-start-time -86399) 86400)))
    (setq eelll-total-time (+ eelll-total-time time)
	  eelll-total-stroke (+ eelll-total-stroke eelll-strokes)
	  eelll-total-error (+ eelll-total-error eelll-error-strokes))
    (eelll-display-score time eelll-strokes eelll-error-strokes)
    (delete-other-windows)
    (recenter -1)
    (if (equal eelll-lesson-string "Temporary")
	(if (y-or-n-p "もう一度トライしますか? ")
	    (progn
	      (with-current-buffer eelll-current-text-buffer
		(goto-char (point-min)))
	      (eelll-setup-lesson))
	  (eelll-end))
    (if (y-or-n-p "もう一度トライしますか? ")
	(progn
	  (eelll-prepare-text eelll-last-lesson)
	  (eelll-setup-lesson))
      (if (y-or-n-p "次のレッスンに進みますか? ")
	  (progn (eelll-prepare-text nil)
		 (setq eelll-previous-error-rate (if eelll-expert 0 100))
		 (eelll-setup-lesson))
	(eelll-end))))
    )
  (message ""))

(defun eelll-save-configuration ()
  "`eelll-last-lesson' の値を設定ファイルに保存する。
設定ファイル名は `eelll-configuration-file-name' で指定する。"
  ;; `eelll-last-lesson-alist'を更新する。
  (let ((config (assoc eelll-text eelll-last-lesson-alist)))
    (if config
	(setcdr config eelll-last-lesson)
      (setq eelll-last-lesson-alist (cons (cons eelll-text eelll-last-lesson)
					  eelll-last-lesson-alist))))
  ;; `eelll-last-lesson-alist'の内容を`eelll-configuration-file-name'に
  ;; 保存する。
  (and tcode-data-directory
       eelll-configuration-file-name
       (file-writable-p eelll-configuration-file-name)
       (save-excursion
	 (kill-buffer
	  (prog1
	      (set-buffer (get-buffer-create " *eelll: temp*"))
	    (insert ";; このファイルは自動的に更新されます。"
		    "編集しないでください。\n")
	    (insert (format "(setq eelll-last-lesson-alist '%s)\n" 
			    (prin1-to-string eelll-last-lesson-alist)))
	    (let ((backup-inhibited t))
	      (write-file eelll-configuration-file-name)))))))

(defun eelll-confirm-quit ()
  "EELLL を終了するかどうか確認し、終了する。"
  (interactive)
  (and (y-or-n-p "EELLL を終了しますか? ")
       (eelll-end t)))

(defun eelll-end (&optional abort)
  (if abort
      ()
    (set-buffer eelll-buffer-name)
    (widen)
    (erase-buffer)
    (insert "総合成績\n\n")
    (eelll-display-score eelll-total-time eelll-total-stroke eelll-total-error)
    (insert "\n    入力打鍵数 " (int-to-string eelll-total-stroke) " 打鍵. ")
    (insert "所要時間 " (int-to-string eelll-total-time) " 秒.\n")
    (recenter -1)
    (if eelll-current-text
	(eelll-save-configuration))
    (message "おつかれさまでした。どれかキーをおしてください。")
    (condition-case ()
	(read-char)
      (error)))
  (kill-buffer eelll-help-buffer-name)
  (set-window-configuration eelll-original-window-configuration)
  (run-hooks 'eelll-exit-hook)
  (message ""))

(defun eelll-remove-frame ()
  "EELLL用のフレームがあれば削除する。"
  (interactive)
  (if (and eelll-frame
	   (frame-live-p eelll-frame))
      (make-frame-invisible eelll-frame)))

;;;###autoload
(defun eelll-other-frame ()
  "フレームを作成してEELLLを始める。"
  (interactive)
  (if (or (null eelll-frame)
	  (not (frame-live-p eelll-frame)))
      (setq eelll-frame (make-frame eelll-frame-parameters)))
  (unless (frame-visible-p eelll-frame)
    (make-frame-visible eelll-frame))
  (select-frame eelll-frame)
  (raise-frame eelll-frame)
  (call-interactively 'eelll))

;;;
;;; 練習テキストデータの整形
;;;

(defun tcode-key-address-right-p (address)
  (let ((location (tcode-get-key-location address)))
    (>= (cdr location) 6)))

(defun tcode-key-address-left-p (address)
  (let ((location (tcode-get-key-location address)))
    (< (cdr location) 6)))

(defun eelll-compile-text (recompile-all)
  "練習テキストのLesson-chars: 行を作る。
RECOMPILE-ALL が non-nil の場合には、
既存のLesson-chars:をすべて削除してから新たに作り直す。"
  (interactive "P")
  (find-file (tcode-path-for-write eelll-text))
  (let ((reached-eob nil) (eelll-buffer (current-buffer)))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (with-current-buffer (get-buffer-create " *eelll: lessons*")
	(delete-region (point-min) (point-max)))
      (while (and (not reached-eob) (not (eobp)))
	(let ((point (point)))
	  (narrow-to-page 1)
	  (if (= point (point)) (setq reached-eob t)))
	(goto-char (point-min))
	(skip-chars-forward "^0-9")
	(message (buffer-substring (point)
				   (save-excursion (end-of-line 1) (point))))
	(skip-chars-forward "^:" (save-excursion (end-of-line 1) (point)))
	(or (eolp) (forward-char))
	(setq eelll-prefix-stroke
	      (let ((l tcode-special-prefix-alist) ret)
		(while l
		  (if (not (looking-at (concat (regexp-quote (nth 2 (car l)))
					       "\\([RrLl]\\)\\([RrLl]\\)"
					       "\\(!?\\)$")))
		      (setq l (cdr l))
		    (goto-char (match-beginning 1))
		    (setq ret (car (car l))
			  l nil)
		    ))
		ret))
	(setq eelll-first-hand (looking-at "[Rr]"))
	(setq eelll-second-hand (looking-at ".[Rr]"))
	(forward-line 1)
	(if (and recompile-all (looking-at "^Lesson-chars: "))
	    (delete-region (point) (save-excursion (forward-line) (point))))
	(if (not (looking-at "^Lesson-chars: "))
	    (let ((where (point))
		  (chars nil)
		  (upper nil)
		  c last)
	      (while (not (eobp))
		(skip-chars-forward " \t\n\f" (point-max))
		(while (not (eolp))
		  (setq chars (cons (buffer-substring (point)
						      (progn (forward-char 1)
							     (point)))
				    chars))))
	      (setq chars (sort chars 'string<))
	      (goto-char where)
	      (if chars (insert "Lesson-chars: "))
	      (while chars
		(catch 'next
		  (setq c (car chars) chars (cdr chars))
		  (if (string= c last) (throw 'next nil))
		  (setq last c)
		  (setq c (eelll-stroke-for-char c))
		  (if (and c
			   (eq eelll-first-hand
			       (tcode-key-address-right-p (car c)))
			   (eq eelll-second-hand
			       (tcode-key-address-right-p (car (cdr c)))))
		      (progn
			(if (or (< (car c) 10) (< (car (cdr c)) 10))
			    (setq upper t))
			(insert last)))))
	      (insert "\n")
	      (if upper
		  (progn
		    (goto-char (point-min))
		    (end-of-line 1)
		    (or (= (preceding-char) ?!)
			(insert "!"))))
	      (sit-for 0)
	      (if (input-pending-p) (ding)))))
      (message "Compilation end.")
      (let ((buffer (get-buffer eelll-text-buffer)))
	(and buffer
	     (kill-buffer buffer))))))

(provide 'eelll)

;;; eelll.el ends here
