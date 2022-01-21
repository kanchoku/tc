;;; tc-util.el --- utilities for T-Code

;; Copyright (C) 1996-2001 KITAJIMA Akira

;; Author: KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;; Created: 7 May 1996
;; Version: $Id: tc-util.el,v 1.22 2003/03/03 04:04:50 kitajima Exp $
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
(require 'tc-sysdep)

(defgroup tcode-utility nil
  "Tコードの補助機能"
  :group 'tcode)

;;;###autoload
(defun tcode-inactivate-and-self-insert (n)
  "Inactivate tcode-mode and self-insert."
  (interactive "*p")
  (if (tcode-on-p)
      (toggle-input-method))
  (self-insert-command n))

;;
;; 拡張コマンド (moved from tc.el)
;;

;;;###autoload
(defun tcode-insert-register (reg arg)
  "`insert-register' と同じ。ただし、ポイントとマークの位置が逆。"
  (interactive "cInsert register: \nP")
  (insert-register reg (not arg)))

;;;###autoload
(defun tcode-transpose-strokes (arg)
  "ポイント位置の文字のストロークを入れかえる。"
  (interactive "*P")
  (if (not (tcode-on-p))
      (transpose-chars arg)
    (if (eolp) (tcode-forward-char -1))
    (let* ((ch (buffer-substring (point)
				 (save-excursion (tcode-forward-char 1)
						 (point))))
	   (strokes (tcode-encode (tcode-string-to-char ch))))
      (when (and (= (length strokes) 2)
		 (setq ch (tcode-action-to-printable
			   (cdr (tcode-decode (reverse strokes))))))
	(tcode-delete-char 1)
	(insert ch)))))

;;;; キーの設定

;;;###autoload
(defun set-tcode-mode-key (key func &optional type)
  "obsolete; use `tcode-set-key'."
  (interactive (list (progn
		       (message "Tコードモードで設定を行うキーは? ")
		       (setq key (read-char)))
		     (read-command (format "「%c」に割り当てるコマンドは? "
					   key))
		     prefix-arg))
  (tcode-set-key key func type))

;;;; カーソルの色

(when (or (tcode-mule-2-p)
	  (tcode-mule-3-p)
	  (tcode-mule-4-p)
	  (tcode-xemacs-p))
  (or (fboundp 'set-cursor-color)
      ;; for XEmacs
      (defun set-cursor-color (color)
	(set-frame-property (selected-frame) 'cursor-color color)))

  (defcustom tcode-mode-off-cursor-color
    (and window-system
	 (or (cdr (assq 'cursor-color (frame-parameters (selected-frame))))
	     (face-background-name (get-face 'text-cursor)))) ; XEmacs
    "* Tコードモードでないときのカーソルの色。"
    :type 'string :group 'tcode-utility)
  (defcustom tcode-mode-on-cursor-color "GreenYellow"
    "* Tコードモードのときのカーソルの色。"
    :type 'string :group 'tcode-utility)
  (defcustom tcode-katakana-mode-cursor-color "Green"
    "* Tコードモードでカタカナモードのときのカーソルの色。"
    :type 'string :group 'tcode-utility)

  (defun tcode-change-cursor-color ()
    (set-cursor-color
     (if (tcode-on-p)
	 (if tcode-katakana-mode
	     tcode-katakana-mode-cursor-color
	   tcode-mode-on-cursor-color)
       tcode-mode-off-cursor-color)))

  (defun tcode-enable-cursor-to-change-color (&optional arg)
    "引数がなければ、カーソルの色がモードにより変わるようにする。
nil でない引数があれば、カーソルの色がモードにより変わらないようにする。"
    (interactive "P")
    (if (null arg)
	(progn
	  (add-hook 'tcode-toggle-hook 'tcode-change-cursor-color)
	  (add-hook 'post-command-hook 'tcode-change-cursor-color)
	  (add-hook 'minibuffer-setup-hook 'tcode-change-cursor-color))
      (remove-hook 'tcode-toggle-hook 'tcode-change-cursor-color)
      (remove-hook 'post-command-hook 'tcode-change-cursor-color)
      (remove-hook 'minibuffer-setup-hook 'tcode-change-cursor-color))))

;;;; 交ぜ書き辞書から項目を一括削除

(defvar tcode-mazegaki-delete-log-buffer "*Mazegaki Delete Log*"
  "* 一括削除の一覧を表示するバッファ名")

(autoload 'tcode-mazegaki-switch-to-dictionary "tc-mazegaki" nil t)

(defun tcode-mazegaki-write-to-delete-log (str)
  (with-current-buffer tcode-mazegaki-delete-log-buffer
    (goto-char (point-max))
    (insert str)))

(defun tcode-mazegaki-make-entry-list (kanji)
  "漢字を含む項目の一覧を作成する。
ただし、作成されるのは `tcode-mazegaki-delete-kanji-from-dictionary' で
削除される項目だけ。"
  (and (string= kanji "")
       (error "Quit"))
  (let ((nod 0)
	(yomi-pattern (concat "[^ ]*" kanji))
	str)
    (with-current-buffer (get-buffer-create tcode-mazegaki-delete-log-buffer)
      (tcode-mazegaki-switch-to-dictionary)
      (goto-char (point-min))
      (message "検索中(%s)..." kanji)
      (while (search-forward kanji nil t)
	(beginning-of-line)
	(if (looking-at yomi-pattern)
	    (next-line 1)
	  (setq str (buffer-substring (point) (progn (next-line 1) (point))))
	  (tcode-mazegaki-write-to-delete-log str)
	  (setq nod (1+ nod))))
      (if (> nod 0)
	  (tcode-mazegaki-write-to-delete-log
	   (format "\n\t%s  %s\n" kanji (format "%d 項目" nod)))
	(message "概当なし"))
      (> nod 0))))

;;;###autoload
(defun tcode-mazegaki-delete-kanji-from-dictionary (kanji)
  "漢字を含む項目を削除する。
ただし、削除されるのは読みにその漢字が含まれていない項目だけ。"
  (interactive
   (let ((minibuffer-setup-hook
	  (unless (tcode-mule-1-p)
	    (cons 'tcode-activate minibuffer-setup-hook))))
     (list (read-from-minibuffer "削除する漢字 "))))
  (and (or (string= kanji "")
	   (string= kanji "/"))
       (error "Quit"))
  (and (tcode-mazegaki-make-entry-list kanji)
       (if (save-excursion
	     (pop-to-buffer tcode-mazegaki-delete-log-buffer)
	     (goto-char (point-max))
	     (y-or-n-p "削除しますか"))
	   (let ((nod 0)
		 (yomi-pattern (concat "[^ ]*" kanji))
		 (pattern (concat "/[^/\n]*" kanji "[^/\n]*/")))
	     (save-excursion
	       (tcode-mazegaki-switch-to-dictionary)
	       (goto-char (point-min))
	       (message "削除中(%s)..." kanji)
	       (while (search-forward kanji nil t)
		 (beginning-of-line)
		 (if (looking-at yomi-pattern)
		     (next-line 1)
		   (narrow-to-region (point)
				     (save-excursion (next-line 1) (point)))
		   (while (re-search-forward pattern nil t)
		     (replace-match "/")
		     (backward-char)
		     (setq nod (1+ nod)))
		   (widen)
		   (if (and (= (preceding-char) ? )
			    (= (char-after (1+ (point))) ?\n))
		       (progn
			 (beginning-of-line)
			 (kill-line 1))
		     (end-of-line)
		     (forward-char))))
	       (message "削除中(%s)...終了 (%s)" kanji
			(format "%d語削除" nod))
	       (tcode-mazegaki-write-to-delete-log
		(format "%d語削除\n\f\n" nod))))
	 (tcode-mazegaki-write-to-delete-log "削除中止\n\f\n"))))

;;;; 交ぜ書き辞書の中の最長の読みを一つ見つける

;;;###autoload
(defun tcode-mazegaki-get-yomi-max ()
  "辞書中の項目の中で読みが最長のもの(およびその長さ)を一つ見つける。
その長さを返す。"
  (interactive)
  (let ((max 0)	n
	line (l 0)
	maxstr str)
    (tcode-mazegaki-switch-to-dictionary)
    (goto-char (point-min))
    (while (not (eobp))
      (setq n (if (= (char-width (tcode-following-char)) 1)
		  1			; alphabet
		(length
		 (string-to-list
		  (setq str (buffer-substring
			     (point)
			     (prog2
				 (looking-at "^\\([^/]+\\) /")
				 (match-end 1))))))))
      (and (> n max)
	   (setq max n
		 line l
		 maxstr str))
      (forward-line 1)
      (setq l (1+ l)))
    (and (called-interactively-p 'interactive)
	 (message "%d文字 (%s) %d行目" max maxstr line))
    max))

;;;; コントロールキーを伴わない Tコードモードの切り替え

(defvar tcode-electric-switching-command-list
  '(self-insert-command
    egg-self-insert-command
    tcode-mazegaki-finish
    delete-backward-char
    backward-delete-char
    backward-delete-char-untabify
    tcode-insert-ya-outset
    tcode-transpose-strokes-or-chars)
  "* 直後の `tcode-electric-space' でモードが切り替わるコマンドのリスト。
Tコードモードのときに、このリストのコマンドが呼ばれた後に
`tcode-electric-space' を実行すると Tコードモードを切り替える。")

(defvar tcode-electric-switching-chars '(?,)
  "* `tcode-electric-space' の直後にモードが切り替わる文字のリスト。
ここで定義された文字を通常のアルファベット入力モードで
`tcode-electric-space' の直後に入力すると Tコードモードに切り替わる。")

(defvar tcode-space-chars-list '(?  ?\n ?\t)
  "* 空白として扱う文字のリスト")

(defvar tcode-electric-deleting-and-switching-chars '(?\t)
  "* `tcode-electric-space' の直後にモードが切り替わる文字のリスト。
ただし、`tcode-electric-switching-chars' と異なり、
直前に `tcode-electric-space' で挿入した文字を消す。")

(defvar tcode-electric-space-without-inserting nil
  "* nil なら切り替えるときに文字(空白)を入力する。")

(defvar tcode-no-following-space-chars "({[‘“（〔［｛〈《「『【、。，．"
  "この文字列中のどの文字の後にも直後に空白を挿入しない。")

;;; 交ぜ書き変換が用意されていない場合は、self-insert しか行わない。
(or (boundp 'tcode-mazegaki-self-insert-or-convert)
    (defun tcode-mazegaki-self-insert-or-convert (arg)
      (interactive "*p")
      (self-insert-command arg)))

;;;###autoload
(defun tcode-electric-space (arg)
  "空白を入力することにより Tコードモードを切り替える。
`tcode-electric-switching-command-list' にあるコマンドが呼ばれた直後に
このコマンドが呼ばれると、Tコードモードを切り替える。
そうでないときは、単に空白を挿入する。"
  (interactive "p")
  (cond (buffer-read-only
	 (toggle-input-method))
	((tcode-on-p)
	 (or (tcode-mazegaki-self-insert-or-convert arg)
	     (if (memq last-command tcode-electric-switching-command-list)
		 ;; 空白をそのままにして切り替え
		 (progn
		   (delete-backward-char 1)
		   (toggle-input-method)
		   (or tcode-electric-space-without-inserting
		       (and (not (bobp))
			    (let ((prev-char (char-to-string
					      (tcode-preceding-char))))
			      (string-match (regexp-quote prev-char)
					    tcode-no-following-space-chars)))
		       (tcode-redo-command last-command-event)))
	       (condition-case nil
		   (let* ((echo-keystrokes 0)
			  (ch (read-char)))
		     (if (memq ch tcode-electric-deleting-and-switching-chars)
			 ;; 直前の空白を消して切り替え
			 (progn
			   (delete-backward-char 1)
			   (toggle-input-method))
		       ;; 切り替えない
		       (tcode-redo-command ch)))
		 (t
		  (setq unread-command-events
			(nconc unread-command-events
			       (list last-input-event))))))))
	(t
	 ;; OFF から ON への切り替え
	 (self-insert-command arg)
	 (condition-case nil
	     (let* ((echo-keystrokes 0)
		    (ch (read-char)))
	       (cond ((memq ch tcode-electric-switching-chars)
		      (and tcode-electric-space-without-inserting
			   (delete-backward-char 1))
		      (toggle-input-method))
		     ((memq ch tcode-electric-deleting-and-switching-chars)
		      (delete-backward-char 1)
		      (toggle-input-method))
		     (t
		      (tcode-redo-command ch))))
	   (t
	    (setq unread-command-events (nconc unread-command-events
					       (list last-input-event))))))))

;;;###autoload
(defun tcode-electric-comma (arg)
  "空白などの後で「,」を入力することにより、Tコードモードに切り替える。
切り替わるのは、非 Tコードモードで、かつ `tcode-space-chars-list' 中の
いずれかの文字の直後で「,」を入力したときのみ。"
  (interactive "p")
  (if (and (not (tcode-on-p))
	   (or (bolp)
	       (memq (tcode-preceding-char) tcode-space-chars-list)))
      (toggle-input-method)
    (self-insert-command arg)))

;;;; もう一つの外字入力

(defvar tcode-ya-outset-map-list
  '(["¥" "†" "‡" "¶"  "▼"     "《"  "》" "【" "】" "“"

     "★" "◆" "■" "●"  "▲"     "〈"  "〉" "〔" "〕" "〃"
     "☆" "◇" "□" "○"  "△"     "←"  "↓" "↑" "→" "§"
     "※" "‥" "…" "◎"  "▽"     "♪"  "〒" "♂" "♀" "‐"]

    ["∵" "∴" "≪" "≫"  "≡"     "≦"  "≧" "∝" "∽" "⊥"

     "⊇" "⊆" "∃" "∀"  "⇔"     "＜"  "＞" "≠" "＝" "∠"
     "∋" "¬" "∨" "∧"  "⇒"     "＋"  "×" "÷" "−" "±"
     "∈" "⊂" "∪" "∩"  "⊃"     "∞"  "≒" "∫" "∬" "⌒"]

    ["■" "Ξ" "Φ" "Ψ"  "■"     "■"  "Υ" "Λ" "■" "■"

     "Γ" "Σ" "Π" "π"  "ψ"     "φ"  "γ" "Δ" "ρ" "λ"
     "α" "ο" "ε" "υ"  "ι"     "δ"  "χ" "τ" "ν" "σ"
     "Θ" "θ" "η" "κ"  "ξ"     "β"  "μ" "ω" "Ω" "ζ"])
  "* 外字のマップのリスト")

;;;###autoload
(defun tcode-insert-ya-outset (level)
  "一文字読み込み、 `tcode-ya-outset-map-list' の表に基づき文字を挿入する。
LEVEL 番目の表が対象となる。"
  (interactive "*p")
  (tcode-cancel-undo-boundary)
  (let* ((map-num (length tcode-ya-outset-map-list))
	 (map-index (1- (let ((i level))
			  (while (> i map-num)
			    (setq i (- i map-num)))
			  i)))
	 (outset-map (nth map-index tcode-ya-outset-map-list))
	 (show-table (sit-for 1)))
    (and show-table
	 (tcode-display-help-buffer
	  (tcode-draw-table outset-map (1+ map-index) map-num) t))
    (unwind-protect
	(let* ((ch (read-char))
	       (addr (tcode-char-to-key ch))
	       (elm (and (>= addr 0)
			 (< addr (length outset-map))
			 (aref outset-map addr))))
	  (cond (elm
		 (let (current-prefix-arg)
		   (tcode-insert elm)))
		((= ch last-command-event)
		 (tcode-insert-ya-outset (1+ level)))
		((= ch ? )
		 (self-insert-command level))
		(t
		 (self-insert-command level)
		 (setq prefix-arg level)
		 (tcode-redo-command ch))))
      (and show-table
	   (tcode-auto-remove-help t)))))

;;;; 文字またはストロークの入れ替え

(defvar tcode-transpose-strokes-enable-commands
  '(tcode-self-insert-command
    egg-self-insert-command
    self-insert-command
    tcode-transpose-strokes-or-chars)
  "*次の動作でストロークを入れ替えることができるコマンドのリスト。
この変数で指定されたコマンドを実行した直後に
`tcode-transpose-strokes-or-chars' を実行すると、
ストロークを入れ替える。")

;;;###autoload
(defun tcode-transpose-strokes-or-chars (&optional arg)
  "Tコードモードのときには、ポイントのストロークを入れ替える。"
  (interactive "*P")
  (if (and (not (bobp))
	   (memq last-command tcode-transpose-strokes-enable-commands)
	   (= (char-width (tcode-preceding-char)) 2))
      (progn
	;; ストロークを入れ替える
	(or (eolp)
	    (tcode-backward-char 1))
	(tcode-transpose-strokes arg))
    ;; 文字を入れ替える
    (if (memq last-command tcode-transpose-strokes-enable-commands)
	(progn
	  (backward-char 1))
      (setq this-command 'transpose-chars)
      (and (eolp)
	   (backward-char 1)))
    (transpose-chars arg)))

;;;; 漢字に対する交ぜ書き辞書からの読みの表示

;;;###autoload
(defun tcode-mazegaki-show-yomi-region (begin end &optional prefix)
  "リージョンで指定された文字列の読みを交ぜ書き辞書から探して表示する。
PREFIX が nil でなければリージョン中の文字列で始まる文字列を探す。"
  (interactive "r\nP")
  (let* ((kanji (buffer-substring begin end))
	 (pattern (concat "/" kanji (if prefix "" "/")))
	 list)
    (save-excursion
      (tcode-mazegaki-switch-to-dictionary)
      (goto-char (point-min))
      (while (search-forward pattern nil t)
	(beginning-of-line)
	(looking-at "^\\([^/]+\\) /")
	(setq list (nconc list
			  (list (buffer-substring (match-beginning 1)
						  (match-end 1)))))
	(forward-line 1))
      (if list
	  (message (mapconcat 'identity list ", "))
	(error "「%s」の読みは見つかりませんでした。" kanji)))))

;;;; ひらがなからカタカナへの変換

(unless (fboundp 'japanese-katakana-region)
  (if (fboundp 'katakana-region)
      (defun japanese-katakana-region (start end)
	(katakana-region start end))
    (defun japanese-katakana-region (start end)
      "リージョン中のひらがなをカタカナにする。"
      (interactive "r")
      (let* ((str (buffer-substring start end))
	     (katakana (mapconcat (lambda (char) 
				    (char-to-string (japanese-katakana char)))
				  (string-to-list str)
				  nil)))
	(unless (string= str katakana)
	  (delete-region start end)
	  (insert katakana))))))

;;;###autoload
(defun tcode-katakana-previous-char (n)
  "現ポイントより n 文字前までのひらがなをカタカナにする。"
  (interactive "*p")
  (let ((prev-char (tcode-preceding-char)))
    (japanese-katakana-region (save-excursion (tcode-backward-char n) (point))
		     (point))
    (and tcode-auto-help
	 (/= prev-char (tcode-preceding-char))
	 (= n 1)
	 (tcode-display-direct-stroke
	  (char-to-string (tcode-preceding-char)))
	 (tcode-auto-remove-help-char))))

(defvar tcode-katakana-begin-point (point-min)
  "直前の後置型カタカナ変換の開始位置")
(defvar tcode-katakana-end-point (point-min)
  "直前の後置型カタカナ変換の終了位置")

(defun tcode-katakana-previous-chars (&optional hiracnt)
  "現ポイント以前で連続するひらがなや「ー」をカタカナに変換する。
ひらがなとして残す文字数を HIRACNT で指定可能。
ただし、交ぜ書き変換の読みがあれば、読みをカタカナとして確定する。"
  (interactive "*p")
  (if tcode-mazegaki-prefix
      (let ((beg tcode-mazegaki-prefix))
	(tcode-mazegaki-restore-yomi-and-quit)
	(japanese-katakana-region beg (point)))
    (save-excursion
      (let* ((end (point))
	     (beg0 (re-search-backward "[^ぁ-んー]" nil t))
	     (beg (+ (if beg0
			 ;; 「キーとばりゅー」に対して1文字残してカタカナ変換で
			 ;; 「キーとバリュー」になるように始まりの「ー」は除く
			 (if (and (looking-at ".ー")
				  (re-search-forward "[ぁ-ん]" end t))
			     (match-beginning 0)
			   (+ beg0 1))
		       (point-min))
		     (or hiracnt 0))))
	(when (< beg end)
	  (setq tcode-katakana-begin-point beg)
	  (setq tcode-katakana-end-point end)
	  (japanese-katakana-region beg end))))))

(defun tcode-katakana-shrink (&optional cnt)
  "直前の後置型カタカナ変換を縮める"
  ;; 単にカタカナを縮めるのではなく、直前の後置型カタカナ変換を縮める。
  ;; 「キーとばりゅー」後置型カタカナ変換→「キートバリュー」
  ;; 直前の後置型カタカナ変換を1文字縮める→「キーとバリュー」
  ;; (もし単にカタカナを縮めると→「きートバリュー」)
  (interactive "*p")
  (when (and (= (point) tcode-katakana-end-point)
	     (> tcode-katakana-end-point tcode-katakana-begin-point))
    (let ((hiraend (min (+ tcode-katakana-begin-point (or cnt 1))
			tcode-katakana-end-point)))
      (save-excursion
	(japanese-hiragana-region tcode-katakana-begin-point hiraend))
      (setq tcode-katakana-begin-point hiraend))))

;;;; 区点・JIS コードによる漢字入力

;;;###autoload
(defun tcode-insert-kanji-by-kuten-code (code)
  "区点コード CODE の漢字を挿入する。"
  (interactive "*s区点コード(10進数4桁)? ")
  (let* ((declist (mapcar (lambda (n)
			    (if (and (>= n ?0)
				     (<= n ?9))
				(- n ?0)
			      0))
			  (string-to-list code)))
	 (kuten (cons (+ (* (car declist) 10)
			 (car (setq declist (cdr declist))))
		      (+ (* (car (setq declist (cdr declist))) 10)
			 (car (cdr declist))))))
    (and (or (> (cdr kuten) 94)
	     (= (cdr kuten) 0)
	     (memq (car kuten) '(0 14 15)))
	 (error "コード(%s)が間違っています。" code))
    (tcode-insert-kanji-by-jis-code (format "%x%x"
					    (+ (car kuten) 32)
					    (+ (cdr kuten) 32)))))

;;;###autoload
(defun tcode-insert-kanji-by-jis-code (code)
  "JISコード CODE の漢字を挿入する。"
  (interactive "*sJIS コード(16進数)? ")
  (let ((hexlist (mapcar (lambda (n)
			   (cond ((and (>= n ?0)
				       (<= n ?9))
				  (- n ?0))
				 ((and (>= (setq n (downcase n)) ?a)
				       (<= n ?f))
				  (+ (- n ?a) 10))
				 (t
				  0)))
			 (string-to-list code)))
	bytelist)
    (while hexlist
      (setq bytelist (nconc bytelist
			    (list (+ (* (car hexlist) 16)
				     (car (cdr hexlist)))))
	    hexlist (nthcdr 2 hexlist)))
    (let ((kanji (make-char tcode-jisx0208
			    (+ (car bytelist) 128)
			    (+ (car (cdr bytelist)) 128))))
      (tcode-insert kanji)
      (and tcode-auto-help
	   (tcode-display-direct-stroke (char-to-string kanji))
	   (tcode-auto-remove-help-char)))))

;;;; バッファの内容に応じた句読点自動切り替え

(defvar tcode-kutouten-regexp-alist
  (list '("[、。]" . 1)
	'("\\cj[,.]" . 2))
  "* 句読点を判定するための正規表現の alist。
リストの各要素は、
その句読点が使われていることを判定するための正規表現と、
それにマッチした時に選ばれる `tcode-switch-table-list' の組の
番号(何番目の組か)。")

(defvar tcode-auto-identify-kutouten-mode-list '(text-mode)
  "* 句読点の自動判定を行う主モードのリスト。")

(defun tcode-identify-kutouten-type ()
  "バッファの内容から使用されている句読点を判別する。
`tcode-kutouten-regexp-alist' の正規表規を順に探し、マッチすれば
その番号を返す。どれにもあてはまらない場合は 0 を返す。"
  (catch 'found
    (let* ((list tcode-kutouten-regexp-alist)
	   regexp)
      (while list
	(setq regexp (car (car list)))
	(save-excursion
	  (goto-char (point-min))
	  (and (re-search-forward regexp nil t)
	       (throw 'found (cdr (car list)))))
	(setq list (cdr list)))
      0)))

;;;###autoload
(defun tcode-auto-switch-kutouten (&optional force)
  "バッファの内容から自動的に句読点を切り替える。
FORCE が nil の場合は、
`tcode-auto-identify-kutouten-mode-list' 中のモードで、
かつそのバッファが read-only でない場合にのみ動作する。
句読点の選択は関数 `tcode-identify-kutouten-type' で行う。"
  (interactive "P")
  (and (or force
	   (and (memq major-mode tcode-auto-identify-kutouten-mode-list)
		(not buffer-read-only)))
       (tcode-switch-variable (tcode-identify-kutouten-type))))

;;;; 直前の文字列を順にカタカナに変換

(unless (fboundp 'japanese-hiragana-region)
  (if (fboundp 'hiragana-region)
      (defun japanese-hiragana-region (start end)
	(hiragana-region start end))
    (defun japanese-hiragana-region (start end)
      "リージョン中のカタカナをひらがなにする。"
      (interactive "r")
      (let* ((str (buffer-substring start end))
	     (hiragana (mapconcat (lambda (char) 
				    (char-to-string (japanese-hiragana char)))
				  (string-to-list str)
				  nil)))
	(unless (string= str hiragana)
	  (delete-region start end)
	  (insert hiragana))))))

;;;###autoload
(defun tcode-katakana-preceding-chars (arg)
  "直前の文字列を順にカタカナに変換する。
コマンドのキーを何回か押すと、その回数だけ直前のひらがながカタカナになる。
Backspace で最後にカタカナになった文字をひらがなに戻す。
RET で終了。
その他のキーはそのキーの動作を行う。"
  (interactive "*p")
  (let ((point (point)))
    (cond ((> arg 0)
	   (tcode-forward-char (- arg))
	   (japanese-katakana-region (point) point))
	  ((< arg 0)
	   (tcode-forward-char arg)
	   (japanese-hiragana-region (point) (progn (tcode-forward-char 1)
						    (point)))
	   (setq arg (1- (- arg)))))
    (goto-char point)
    (unwind-protect
	(let* ((echo-keystrokes 0)
	       (ch (read-char)))
	  (cond ((= ch last-command-event)
		 (tcode-katakana-preceding-chars (1+ arg)))
		((= ch ?\C-?)
		 (tcode-katakana-preceding-chars (- arg)))
		((= ch ?\C-m))
		(t
		 (tcode-redo-command ch))))
      (goto-char point))))

;;;; KKC を用いた変換
;;; contributed by Masayuki Ataka / 安宅 正之

(defvar tcode-kkc-toroku t
  "* コマンド `tcode-kkc-region' で、変換後の文字を登録するかどうかを表す。
t で変換した文字を交ぜ書き辞書へ登録する。
nil では登録しない。1 を選択すると、登録文字の読みを修正できる。")

(defun tcode-kkc-region (beg end)
  "リージョンで囲まれた平仮名の列を漢字に変換する。

内部で leim パッケージを使っているので、
leim パッケージが入っていなければ使うことはできない。
使用している辞書は leim パッケージ付属の辞書。

変数 `tcode-kkc-toroku' を使って、
変換した文字の交ぜ書き辞書への登録を
するかしないかを制御することができる。"
  (interactive "r")
  (let ((default current-input-method)
	(yomi (buffer-substring beg end)))
    (unwind-protect
	(progn
	  (activate-input-method "japanese")
	  (kkc-region beg end)
	  (tcode-kkc-mazegaki-toroku beg yomi))
      (activate-input-method default))))

(defun tcode-kkc-mazegaki-toroku (beg yomi)
  (cond
   ;; 変換した文字を交ぜ書き辞書に登録
   ((equal tcode-kkc-toroku t)
    (tcode-mazegaki-make-entry yomi (buffer-substring beg (point))))
   ;; 読みを修正してから、辞書に登録
   ((equal tcode-kkc-toroku 1)
    (let ((minibuffer-setup-hook (list 'tcode-activate)))
      (tcode-mazegaki-make-entry
       (read-string
	(format "\"%s\"の読み: "
		(buffer-substring beg (point))) yomi)
       (buffer-substring beg (point)))))
   ;; 交ぜ書き辞書への登録はしない。
   (t )))

;;;; zap-to-char の拡張

;;;###autoload
(defun tcode-zap-to-char (arg char)
  "`zap-to-char'の拡張で、TコードモードのときはTコードで入力する。"
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (let ((key (and (message (if (tcode-on-p)
						  "Zap to char [TC]: "
						"Zap to char: "))
				     (read-char))))
		       (if (tcode-on-p)
			   (let ((keys (tcode-input-method key)))
			     (if (= (length keys) 1)
				 (car keys)))
			 key))))
  (if char
      (zap-to-char arg char)))

;;;; トグルではなく、ON/OFFキーを使用

;;;###autoload
(defun tcode-activate-input-method ()
  (interactive)
  (activate-input-method default-input-method))
;;;###autoload
(defun tcode-inactivate-input-method ()
  (interactive)
  (inactivate-input-method))
;;;###autoload
(defun tcode-inactivate-input-method-recenter ()
  (interactive)
  (inactivate-input-method)
  (recenter-top-bottom))


(provide 'tc-util)

;;; tc-util.el ends here
