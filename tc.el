;;; tc.el --- a Japanese input method with T-Code on Emacs

;; Copyright (C) 1989--2002 Kaoru Maeda, Yasushi Saito and KITAJIMA Akira.

;; Author: Kaoru Maeda <maeda@src.ricoh.co.jp>
;;	Yasushi Saito <yasushi@cs.washington.edu>
;;      KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;; Maintainer: KITAJIMA Akira
;; Keyword: input method, japanese

;; $Id: tc.el,v 1.60 2003/03/29 03:42:27 kitajima Exp $

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

(require 'tc-pre)
(require 'tc-sysdep)

;;
;; Variables for customization
;;

(defcustom tcode-bushu-ready-hook nil
  "部首合成変換の初期化直後に実行されるフック。"
  :type 'hook :group 'tcode)

(defvar tcode-bushu-on-demand 2
  "部首合成変換辞書をいつ初期化するか。
	0 : tc.el ロード時
	1 : 初めてTコードモードに入ったとき
	2 : 部首合成変換を開始したとき、
            または文字のヘルプを見ようとしたとき")

(defcustom tcode-use-postfix-bushu-as-default nil
  "* nil でないとき、jfで後置型部首合成変換を、77で前置型部首合成変換を行う。
nilの時にはその逆。" :type 'boolean :group 'tcode)

(defcustom tcode-use-prefix-mazegaki nil
  "* 前置型(従来型)の交ぜ書き変換の時にt, 後置型(新型)の交ぜ書き変換の時にnil。

前置型では、fjを入力すると、読み入力モードに入り、その中で ' 'を入力すると
変換を行う。後置型では、fjを入力すると、ポイント前にある文字列を使って
変換を行う。"
  :type 'boolean :group 'tcode)

(defvar tcode-kuten "。" "* 句点")
(make-variable-buffer-local 'tcode-kuten)
(defvar tcode-touten "、" "* 読点")
(make-variable-buffer-local 'tcode-touten)

(defvar tcode-switch-table-list
  '(((tcode-touten . "、")
     (tcode-kuten . "。"))

    ((tcode-touten . ", ")
     (tcode-kuten . ". ")))
  "テーブル中の変数値を切り替えるための表。")

(defcustom tcode-record-file-name "~/.tc-record"
  "* non-nil のとき、Tコードの統計をこのファイルに記録する。"
  :type 'string :group 'tcode)

(defcustom tcode-ready-hook nil
  "Emacsを立ち上げてから最初にtcode-modeに入ったとき実行されるフック。"
  :type 'hook :group 'tcode)

(defcustom tcode-mode-hook nil
  "そのバッファで初めてtcode-modeに入ったとき実行されるフック。"
  :type 'hook :group 'tcode)

(defcustom tcode-toggle-hook nil
  "tcode-modeをトグルする度に実行されるフック。"
  :type 'hook :group 'tcode)

(defcustom tcode-before-load-table-hook nil
  "`tcode-load-table' によりテーブルを読み込む直前に実行されるフック。"
  :type 'hook :group 'tcode)

(defcustom tcode-after-load-table-hook nil
  "`tcode-load-table' によりテーブルを読み込む度に実行されるフック。"
  :type 'hook :group 'tcode)

(defcustom tcode-before-read-stroke-hook nil
  "2ストローク目以降のキーストロークを読む前に実行されるフック。"
  :type 'hook :group 'tcode)

(defcustom tcode-clear-hook nil
  "デフォールト状態に戻すときに実行されるフック。"
  :type 'hook :group 'tcode)

(defvar tcode-auto-zap-table nil
  "* non-nil のとき、ストローク表を次の打鍵で自動的に消す。")

(defcustom tcode-auto-help t
  "* non-nil のとき、入力した文字のヘルプ表を自動的に表示する。
ただし、表示するのは、交ぜ書き変換や部首合成変換によって直接入力できる漢字を
挿入した場合のみ。
また、値がシンボル delete-the-char のときは、ヘルプを表示し、
さらに最後にヘルプの対象となった文字を消去する。

ただし、`input-method-verbose-flag' が nil でない場合は t となる
ので注意。"
  :group 'tcode)

(defvar tcode-auto-remove-help-count nil
  "* ヘルプ表を画面から無くすまでに呼ばれる `tcode-auto-remove-help' の回数。
関数 `tcode-auto-remove-help' は、この変数の回数だけ呼ばれると、
ヘルプ表を自動的に削除する。nil の場合は自動削除を行わない。")

(defcustom tcode-adjust-window-for-help nil
  "* non-nil のとき、ヘルプを表示するウィンドウの大きさを自動的に調整する。"
  :type 'boolean :group 'tcode)

(defcustom tcode-adjust-window-additional-height
  (if (and (tcode-mule-4-p)
	   (= emacs-major-version 21))
      1
    0)
  "* ヘルプを表示するウィンドウの高さに追加する高さ。"
  :type 'integer :group 'tcode)

(defcustom tcode-display-help-delay 2
  "* 直前の文字を入力してから仮想鍵盤を表示するまでの時間(秒)。

ただし、`input-method-verbose-flag' が nil でない場合は 2 となる
ので注意。"
  :group 'tcode)

(defcustom tcode-verbose-message t
  "* non-nil のとき、より多くのメッセージを表示する。

ただし、変数 `input-method-verbose-flag' が nil でない場合は、
この変数の値によらず、t が設定されている場合と同じ動作になるので注意。"
  :type 'boolean :group 'tcode)

(defvar tcode-shift-lowercase nil
  "Tコード入力時のシフトの場合に、大文字ではなく小文字を入力する。")

(defvar tcode-no-wait-display-help-command-list nil
  "スペースの入力でヘルプ表のバッファを消す機能を使わないコマンドのリスト。
このリスト中のコマンドでは、 `tcode-display-help-buffer' により
ヘルプが表示された場合に、続けてスペースを入力したときでも
ヘルプ表は消えない。")

(defvar tcode-help-window-height-max 21
  "* ヘルプを表示するためのウィンドウの高さの最大値")

(defvar tcode-cancel-stroke-list '(?\C-\? ?\C-h)
  "文字入力を明示的に取り消すキーのリスト")
(defvar tcode-verbose-stroke-list '(? )
  "文字入力の途中で、今までの入力をすべてそのまま挿入するキーのリスト")


;;
;; Global Variables
;;

(defvar tcode-mode-map nil
  "tcode-mode のとき Tコードキー以外のキーのためのキーマップ。
このキーマップに登録するときは、`tcode-set-key'を用いる。
`tcode-key-translation-rule-table' を参照。")

(defvar tcode-key-translation-rule-table
;;   0  1  2  3  4  5  6  7  8  9
;;  10 11 12 13 14 15 16 17 18 19
;;  20 21 22 23 24 25 26 27 28 29
;;  30 31 32 33 34 35 36 37 38 39

;;   1  2  3  4  5  6  7  8  9  0
;;   q  w  e  r  t  y  u  i  o  p
;;   a  s  d  f  g  h  j  k  l  ;
;;   z  x  c  v  b  n  m  ,  .  /

;;      !  \"   #   $   %   &   '   (   )   *   +   ,   -   .   /
;;  0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?
;;  @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O
;;  P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _
;;  `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o
;;  p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~
  [
   -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  37  -1  38  39
   09  00  01  02  03  04  05  06  07  08  -1  29  -1  -1  -1  -1
   -1  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2
   -2  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2  -1  -1  -1  -1  -1
   -1  20  34  32  22  12  23  24  25  17  26  27  28  36  35  18
   19  10  13  21  14  16  33  11  31  15  30  -1  -1  -1  -1
   ]
  "Tコードキー変換用テーブル。その1文字を入力したときの意味を表す。
0..39:	Tコードキー。
-1:	その文字。
-2:	対応する英小文字。
-3:	`tcode-mode-map' にしたがったコマンド。
< -3:	- (文字コード)。")

(defvar tcode-key-num 40
  "Tコードで使うキーの数。標準で40。")

(defvar tcode-ext-keys nil
  "Tコードキー40以上に対応するキーのリスト")

(defvar tcode-this-command-keys nil
  "Tコードでコマンド呼び出しに使ったキーシーケンス(実際のキー)")

;;
;; These variables are set in a table file, e.g. tc-tbl.el.
;;
(defvar tcode-input-method nil "現在選択されている入力法")

(defvar tcode-transparent-mode-indicator nil)
(defvar tcode-tcode-mode-indicator nil)
(defvar tcode-alnum-2byte-tcode-mode-indicator nil)
(defvar tcode-hiragana-mode-indicator nil)
(defvar tcode-katakana-mode-indicator nil)

;;; 以下の変数は、tc-tbl.el などで値が設定され、
;;; 内部テーブル `tcode-table' にその内容が登録される。
(defvar tcode-tbl nil)
(defvar tcode-non-2-stroke-char-list nil)
(defvar tcode-another-table nil
  "`tcode-verbose-stroke-list' のキーが押されたときに挿入する文字のテーブル。
値が nil ならばその文字を、vector で指定されていればキーに応じ
概当する文字を挿入する。")
(defvar tcode-special-commands-alist nil)

(defvar tcode-mode-help-string nil
  "`tcode-mode-help' によって表示される文字列。
nil のときは `tcode-mode' から得る。")
(defvar tcode-stroke-file-name nil "ストローク表のパス名")

(defvar tcode-special-prefix-alist nil
  "ヘルプで特別に扱うプレフィクスストロークのalist
要素は (stroke table-file stroke-to-string-prefix @draw-table-data)")

;;
;; misc. global variables
;;

;;; 入力コードの情報を保持するテーブル
(defvar tcode-table nil "変換に用いる表")
(defvar tcode-stroke-table nil)
(defvar tcode-stroke-table-size 511)

;;; 統計記録用の変数
(defvar tcode-input-chars 0)
(defvar tcode-number-strokes 0)
(defvar tcode-bushu-occurrence 0)
(defvar tcode-mazegaki-occurrence 0)
(defvar tcode-special-occurrence 0)

(defvar tcode-help-char nil "ヘルプの対象文字")
(defvar tcode-auto-remove-help-current-count 0)
(defvar tcode-window-configuration-before-help nil)

(defvar tcode-mode-in-minibuffer nil)

(defvar tcode-dictionaries nil)
;; (バッファ名 . ファイル名)のリスト。

(defconst tcode-stroke-buffer-name " *tcode: stroke*")
(defconst tcode-help-buffer-name "*T-Code Help*")

;;; input-method-verbose-flag 用
(defvar tcode-input-method-verbose-flag nil)
(defvar tcode-orig-auto-help nil)
(defvar tcode-orig-verbose-message nil)
(defvar tcode-orig-display-help-delay nil)

(defvar tcode-use-input-method nil)	; ***experimental***

(defvar tcode-input-filter-functions
  '(((or tcode-katakana-mode tcode-shift-state) . japanese-katakana)
    ((and (boundp 'tcode-bushu-prefix-list)
	  tcode-bushu-prefix-list) 
     . tcode-bushu-prefix-convert)
    (tcode-alnum-2-byte . tcode-1-to-2)))

(defvar tcode-shift-state nil)

(defvar tcode-input-command-list
  '(asm-colon
    asm-comment
    c-electric-semi&comma
    c-electric-slash
    canna-self-insert-command
    digit-argument
    egg-self-insert-command
    electric-c-brace
    electric-c-semi
    electric-c-terminator
    electric-perl-terminator
    org-self-insert-command
    perl-electric-terminator
    self-insert-command
    sgml-slash
    sql-magic-semicolon
    sql-magic-go
    tcode-electric-comma
    tcode-self-insert-command
    tcode-mazegaki-self-insert-or-convert)
  "*TコードモードのときにはTコード入力に用いるコマンドのリスト。")

;;
;; Buffer Local Variables
;;
(defvar tcode-mode nil "Tコードモードのときt。")
(make-variable-buffer-local 'tcode-mode)

(defvar tcode-ready-in-this-buffer nil "このバッファ内でTコードの準備がOK")
(make-variable-buffer-local 'tcode-ready-in-this-buffer)

(defvar tcode-current-switch-table 0)
(make-variable-buffer-local 'tcode-current-switch-table)

(defvar tcode-alnum-2-byte nil
  "英数字のバイト長切り換えフラグ。t では2バイト系、nil で1バイト系。")
(make-variable-buffer-local 'tcode-alnum-2-byte)

(defvar tcode-katakana-mode nil "現在カタカナモードかどうか")
(make-variable-buffer-local 'tcode-katakana-mode)

;;
;; キー配置・キーマップの設定
;;
(defvar tcode-key-layout "qwerty"
  "*Tコードを用いるキーの配置。
`tcode-key-layout-list' に登録されている名前を設定する。")

(defvar tcode-key-layout-list
  '(("qwerty" . (?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
		    ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
		    ?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;
		    ?z ?x ?c ?v ?b ?n ?m ?, ?. ?/))
    ("qwerty-jis-shift" . (?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
			      ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
			      ?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;
			      ?z ?x ?c ?v ?b ?n ?m ?, ?. ?/
			      ?! ?\" ?# ?$ ?% ?& ?' ?\( ?\) ?~
			      ?Q ?W ?E ?R ?T ?Y ?U ?I ?O ?P
			      ?A ?S ?D ?F ?G ?H ?J ?K ?L ?+
			      ?Z ?X ?C ?V ?B ?N ?M ?< ?> ??))
    ("qwerty-us-shift" . (?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
			     ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
			     ?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;
			     ?z ?x ?c ?v ?b ?n ?m ?, ?. ?/
			     ?! ?\@ ?# ?$ ?% ?^ ?& ?* ?\( ?\)
			     ?Q ?W ?E ?R ?T ?Y ?U ?I ?O ?P
			     ?A ?S ?D ?F ?G ?H ?J ?K ?L ?:
			     ?Z ?X ?C ?V ?B ?N ?M ?< ?> ??))
    ("dvorak" . (?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
		    ?' ?, ?. ?p ?y ?f ?g ?c ?r ?l
		    ?a ?o ?e ?u ?i ?d ?h ?t ?n ?s
		    ?\; ?q ?j ?k ?x ?b ?m ?w ?v ?z))
    ("dvorak-shift" . (?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
			  ?' ?, ?. ?p ?y ?f ?g ?c ?r ?l
			  ?a ?o ?e ?u ?i ?d ?h ?t ?n ?s
			  ?\; ?q ?j ?k ?x ?b ?m ?w ?v ?z
			  ?! ?\@ ?# ?$ ?% ?^ ?& ?* ?\( ?\)
			  ?\" ?< ?> ?P ?Y ?F ?G ?C ?R ?L
			  ?A ?O ?E ?U ?I ?D ?H ?T ?N ?S
			  ?: ?Q ?J ?K ?X ?B ?M ?W ?V ?Z)))
  "*Tコードを用いるキー配置用のデータ。
具体的には、配列名とキー並びの組のリスト。
キー並びでは、左上から順に、入力される文字を40個または80個並べる。
80個並べた場合、後半の40個は、前半の40個のシフト時の文字として扱う。

コマンド`tcode-set-key-layout'で用いる。")

(defun tcode-set-key-layout (layout)
  "Tコードで用いるキー配置を設定する。"
  (interactive (let ((layout (completing-read
			      (format
			       "キー配置を選択してください。[%s] "
			       (car (car tcode-key-layout-list)))
			      tcode-key-layout-list)))
		 (list layout)))
  (let ((table (make-vector (1+ (- ?~ ? )) -1))
	(list (assoc layout tcode-key-layout-list)))
    (unless list
      (if (or (null layout)
	      (= (length layout) 0))
	  (unless (setq list (car tcode-key-layout-list))
	    (error "キー配置(tcode-key-layout-list)が定義されていません。"))
	(let ((i 0))
	  (while (< i 40)
	    (if (= i 0)
		(message "キーボードのキーを左上から順に押してください。")
	      (message "%d行%d列目>" (/ i 40) (% i 40)))
	    (setq list (cons (read-char) list)
		  i (1+ i)))
	  (setq list (cons layout (nreverse list))))))
    (let ((i 0))
      (mapc (lambda (char)
	      (cond ((null char)
		     (aset table (- char ? ) -1))
		    ((and tcode-shift-lowercase
			  (/= (upcase char) char))
		     (aset table (- char ? ) i)
		     (aset table (- (upcase char) ? ) -2))
		    (t
		     (aset table (- char ? ) i)))
	      (setq i (1+ i))
	      (if (= i 40) (setq i 100)))
	    (cdr list)))
    (let ((char ? ))
      (while (<= char ?~)
	(if (lookup-key tcode-mode-map (char-to-string char))
	    (aset table (- char ? ) -3))
	(setq char (1+ char))))
    (let ((l tcode-ext-keys)
	  (i 40))
      (while l
	(if (car l)
	    (aset table (- (car l) ? ) i))
	(setq i (1+ i)
	      l (cdr l))))
    (setq tcode-key-layout layout)
    (setq tcode-key-translation-rule-table table)
    list))

(defun tcode-set-key (key func &optional type)
  "Tコードモードのキーを設定する。
引数は次のとおり。

KEY  ... 設定するキー。string、char、vector のいずれか。
FUNC ... TYPE がコマンドの場合の関数名。 TYPE が 'command または -3 以外の
         場合は意味を持たない。(nil にしておけばよい)
TYPE ... 機能の種類。(変数 `tcode-key-translation-rule-table' 参照)
         省略時は、FUNC が nil のとき 'literal で、そうでないときは
         'command。
         以下のシンボルも使える。

         literal      その文字。
         lowercase    対応する英小文字。
         command      tcode-mode-map にしたがったコマンド。"
  (interactive (list (progn
		       (message "Tコードモードで設定を行うキーは? ")
		       (setq key (read-char)))
		     (read-command (format "「%c」に割り当てるコマンドは? "
					   key))
		     prefix-arg))
  ;; type
  (cond ((null type)
	 (setq type (if (if (called-interactively-p 'interactive) (commandp func) func)
			-3 -1)))
	((integerp type))
	((eq type 'literal)
	 (setq type -1))
	((eq type 'lowercase)
	 (setq type -2))
	((eq type 'command)
	 (setq type -3))
	(t
	 (error "TYPE がおかしい。")))
  ;; key
  (cond ((char-or-string-p key)
	 (or (stringp key)
	     (setq key (char-to-string key))))
	((vectorp key)
	 (setq key (char-to-string (aref key 0))))
	(t
	 (error "KEY がおかしい。")))
  ;; set keymap address
  (let ((addr (string-to-char key)))
    (if (and (>= addr ? )
	     (<= addr ?~))
	(when (> 0 (aref tcode-key-translation-rule-table (- addr ? )))
	  (aset tcode-key-translation-rule-table (- addr ? ) type))
      (error "「%s」には割り当てられません。" key)))
  ;; set key binding
  (define-key tcode-mode-map key (and (= type -3)
				      func)))

(unless tcode-mode-map
  (setq tcode-mode-map (make-sparse-keymap))
  (tcode-set-key "?" 'tcode-mode-help)
  ;; tcode-mode のときに、「|」で登録・変換
  (tcode-set-key "|" 'tcode-mazegaki-make-entry-and-finish)
  ;; tcode-mode のときに、「!」で削除
  (tcode-set-key "!" 'tcode-mazegaki-delete-entry-by-last-yomi)
  ;; tcode-mode のときに、「=」で補完・確定
  (tcode-set-key "=" 'tcode-mazegaki-complete-and-convert))

(defun tcode-substitute-command-keys (string)
  "`substitute-command-keys' を `tcode-mode-map' のもとで適用する。"
  (let ((orig-map (current-local-map)))
    (prog2
	(use-local-map tcode-mode-map)
	(substitute-command-keys string)
      (use-local-map orig-map))))

(defun tcode-key-to-char (key)
  "キーの番号から対応する文字を得る。"
  (let ((max (length tcode-key-translation-rule-table))
	(i 0))
    (catch 'found
      (while (< i max)
	(if (= key (aref tcode-key-translation-rule-table i))
	    (throw 'found t))
	(setq i (1+ i))))
    (+ i ? )))

(defun tcode-char-to-key (c)
  "Return virtual key code of character C.
See also `tcode-key-translation-rule-table'."
  (if (or (< c ? ) (> c ?~))
      -1
    (aref tcode-key-translation-rule-table (- c ? ))))

;;
;; T-Code main
;;

(defun tcode-on-p ()
  "Tコードモードがオンになっているかどうかを返す。"
  (if (window-minibuffer-p (selected-window))
      tcode-mode-in-minibuffer
    tcode-mode))

(defun tcode-decode (strokes &optional table trace)
  "Decode STROKES with TABLE.
TABLE defaults `tcode-table'.
TRACE is a sub-stroke-sequence that has already been processed for decoding.

Return (status . value) where:

STATUS             VALUE
complete           decoded value
incomplete         sub-table
out-of-table       sub-strokes on table"
  (let* ((key (car strokes))
	 (table (or table
		    tcode-table))
	 (val (cond ((null table)
		     nil)
		    ((vectorp table)
		     (and (< key (length table))
			  (aref table key)))
		    ((and (consp table)
			  (not (eq (car table) 'lambda)))
		     (if (integerp (car table))
			 (if (= key (car table))
			     (cdr table))
		       (cdr (assq key table))))
		    (t
		     nil))))
    (if (null val)
	(cons 'out-of-table (nreverse trace))
      (let ((cont-strokes (cdr strokes)))
	(if cont-strokes
	    (tcode-decode cont-strokes val (cons key trace))
	  (cons (if (or (vectorp val)
			(and (consp val)
			     (not (eq (car val) 'lambda))))
		    'incomplete
		  'complete)
		(if (and (symbolp val)
			 (boundp val))
		    (eval val)
		  val)))))))

(defun tcode-default-key-binding (key)
  "Return the binding for command KEY in non-tcode-mode."
  (let (tcode-mode)
    (key-binding key)))

(defun tcode-decode-chars (c &optional table trace)
  "Decode C with TABLE.
Read characters if need.
TABLE defaults `tcode-table'.
TRACE is a sub-character-sequence that has already been processed for decoding.

Return value:
cons of (decoded value . character sequence)

where decoded value is one of:
char or string or function (symbol or lambda) ... code
nil ... no corresponding code
t ... cancel"
  (let ((key (tcode-char-to-key c)))
    (if (< key 0)
	;; C is not a key on TABLE
	(cons
	 (cond ((and trace
		     (memq c tcode-cancel-stroke-list))
		t)
	       ((and trace
		     (memq c tcode-verbose-stroke-list))
		(let ((rval (and tcode-another-table
				 (tcode-decode (mapcar 'tcode-char-to-key
						       (nreverse trace))
					       tcode-another-table))))
		  (or (cdr rval)
		      (mapconcat 'char-to-string (nreverse trace) nil))))
	       ((= key -1)
		c)
	       ((= key -2)
		(downcase c))
	       ((= key -3)
		(lookup-key tcode-mode-map (char-to-string c)))
	       (t
		(- key)))
	 (nreverse (cons c trace)))
      ;; C is a key on TABLE
      (if (>= key 100)
	  (setq tcode-shift-state t
		key (mod key 100)))
      (let* ((rval (tcode-decode (list key) table))
	     (status (car rval))
	     (val (cdr rval)))
	(cond ((eq status 'complete)
	       (cons val (nreverse (cons c trace))))
	      ((eq status 'incomplete)
	       (setq tcode-number-strokes (1+ tcode-number-strokes))
	       (run-hooks 'tcode-before-read-stroke-hook)
	       (let ((show-table (sit-for tcode-display-help-delay)))
		 (unwind-protect
		     (let ((echo-keystrokes 0))
		       (if show-table
			   (tcode-display-help-buffer
			    (tcode-draw-current-table val)
			    t))
		       (condition-case nil
			   (tcode-decode-chars (read-char) val (cons c trace))
			 (t (cons t (nreverse trace)))))
		   (if show-table
		       (tcode-auto-remove-help t)))))
	      (t
	       nil))))))

(defun tcode-eval-action (action)
  (cond ((null action)
	 (ding)
	 nil)
	((eq action t)
	 (setq this-command 'keyboard-quit) ; dummy
	 nil)
	((stringp action)
	 (string-to-list action))
	((char-or-string-p action)
	 (list action))
	(t
	 (undo-boundary)
	 (if (commandp action)
	     (progn
	       ;; コマンドの実行
	       (setq prefix-arg current-prefix-arg
		     this-command action)
	       (command-execute action))
	   ;; コマンドでない関数の実行
	   (funcall action)
	   (setq this-command 'lambda))	; dummy
	 (setq tcode-special-occurrence
	       (1+ tcode-special-occurrence))
	 nil)))

(defun tcode-apply-filters (list)
  (mapc (lambda (alist)
	  (if (eval (car alist))
	      (let ((v (mapcar (lambda (c) (funcall (cdr alist) c))
			       list)))
		(setq list (apply 'nconc
				  (mapcar (lambda (e) (if (stringp e)
						          (string-to-list e)
						        (list e)))
					  v))))))
	tcode-input-filter-functions)
  list)

(defvar tcode-debug nil
  "nil (デフォルト)の場合、tcode-input-method 内のエラーは condition-case で
キャッチして捨ててしまう。デバッグ時にエラーの backtrace を取得したい場合は、
この変数を t にセットする。")

(defun tcode-input-method (ch)
  "The input method function for T-Code."
  (let ((events (if tcode-debug
		    (tcode--input-method-core ch)
		  (tcode--input-method-guard ch))))
    (or events
	(when tcode-use-input-method
	  ;; input method が nil を返すと、(read-event) やキーボードマク
	  ;; ロが終了しない。
	  '(tcode-ignore)))))
(global-set-key [tcode-ignore] 'ignore)

(defun tcode--input-method-guard (ch)
  ;; isearch 中に input method がエラーを起こすとユーザーの意図しない
  ;; モードに入り、修復困難になることがある(isearch の色付けが消えない
  ;; など)。それを回避。
  ;;
  ;; tcode-set-key などで、ユーザー指定の任意のコマンドが input method
  ;; 内から実行されうるので、エラー発生を前提とする必要がある。
  (condition-case err
      (tcode--input-method-core ch)
    (error
     (ding)
     (minibuffer-message "Error in input method: %S" err)
     nil)))

(defun tcode--input-method-core (ch)
  (setq last-command 'self-insert-command
	last-command-event ch)
  (if input-method-verbose-flag
      (unless tcode-input-method-verbose-flag
	;; push some variables' values
	(setq tcode-orig-auto-help tcode-auto-help
	      tcode-orig-verbose-message tcode-verbose-message
	      tcode-orig-display-help-delay tcode-display-help-delay)
	;; set new values
	(setq tcode-auto-help t
	      tcode-verbose-message t
	      tcode-display-help-delay 2
	      tcode-input-method-verbose-flag t))
    (if tcode-input-method-verbose-flag
	;; pop pushed values
	(setq tcode-auto-help tcode-orig-auto-help
	      tcode-verbose-message tcode-orig-verbose-message
	      tcode-display-help-delay tcode-orig-display-help-delay
	      tcode-input-method-verbose-flag nil)))
  (let ((command (tcode-default-key-binding (char-to-string ch))))
    (if (or (and (boundp 'overriding-terminal-local-map)
		 overriding-terminal-local-map)
	    (and (boundp 'overriding-local-map)
		 overriding-local-map)
	    (eq this-command 'quoted-insert)
	    (not (memq command tcode-input-command-list))
	    (not (tcode-on-p)))
	;; pass input-method
	(list ch)
      ;; prosess as input-method
      (setq tcode-number-strokes (1+ tcode-number-strokes))
      (catch 'cancel
	(setq tcode-shift-state nil)
	(let ((result (let ((input-method-function) ; disable
			    decoded evaled)
			(setq decoded (tcode-decode-chars ch)
			      tcode-this-command-keys (cdr decoded)
			      evaled (tcode-eval-action (car decoded)))
			(if evaled
			   (tcode-apply-filters evaled)
			  (throw 'cancel nil)))))
	  (if (and result
		   (car result))
	      (setq tcode-input-chars (+ tcode-input-chars (length result))
		    tcode-help-char (char-to-string (car (reverse result)))))
	  (if (not (tcode-on-p)) ; if T-Code is disabled with a command
					; execution
	      (setq input-method-function nil))
	  result)))))

(defun tcode-insert (ch)
  "CHをバッファに挿入する。"
  (unless (stringp ch)
    (setq ch (char-to-string ch)))
  (let* ((p (point))
	 (arg (prefix-numeric-value current-prefix-arg))
	 (n (if (consp current-prefix-arg)
		(/ (car current-prefix-arg) 2)
	      arg)))
    (while (> n 0)
      (insert ch)
      (setq n (1- n)))
    (if overwrite-mode
	(let ((str (buffer-substring p (point))))
	  (delete-text-in-column nil (+ (current-column)
					(string-width str)))))
    (if (and (boundp 'self-insert-after-hook)
	     self-insert-after-hook)
	(funcall self-insert-after-hook p (point)))
    (tcode-do-auto-fill)
    (run-hooks 'input-method-after-insert-chunk-hook)))

;;
;; 2バイト英数字
;;

(defvar tcode-alnum-1-to-2-table
  (concat "　！”＃＄％＆’（）＊＋，−．／０１２３４５６７８９：；＜＝＞？"
	  "＠ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ［¥］＾＿"
	  "‘ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ｛｜｝‾")
  "1バイト英数字 ' '..'~' を2バイト英数字へ変換/逆変換するためのテーブル")

(defun tcode-toggle-alnum-mode ()
  (interactive)
  (setq tcode-alnum-2-byte (not tcode-alnum-2-byte))
  (tcode-mode-line-redisplay))

(defun tcode-check-alnum-1-to-2-table ()
  (if (stringp tcode-alnum-1-to-2-table)
      (setq tcode-alnum-1-to-2-table
	    (vconcat (string-to-list tcode-alnum-1-to-2-table)))))

(defun tcode-1-to-2-region (beg end)
  "リージョン内の1バイト英数字を2バイトに変換する。"
  (interactive "*r")
  (tcode-check-alnum-1-to-2-table)
  (save-excursion
    (save-restriction
      (goto-char beg)
      (narrow-to-region beg end)
      (let (char)
	(while (progn (skip-chars-forward "^!-~" (point-max))
		      (< (point) (point-max)))
	  (setq char (following-char))
	  (tcode-delete-char 1)
	  (insert (char-to-string (tcode-1-to-2 char))))))))

(defun tcode-1-to-2 (char)
  "1バイト英数字CHARを2バイトに変換する。"
  (tcode-check-alnum-1-to-2-table)
  (if (and (<= ?! char) (<= char ?~))
      (aref tcode-alnum-1-to-2-table (- char ? ))
    char))

(defun tcode-2-to-1-region (beg end)
  "リージョン内の2バイト英数字を1バイトに変換する。"
  (interactive "*r")
  (tcode-check-alnum-1-to-2-table)
  (save-excursion
    (save-restriction
      (goto-char beg)
      (narrow-to-region beg end)
      (let ((alnum-2byte-regexp (concat "["
					(mapconcat 'char-to-string
						   tcode-alnum-1-to-2-table
						   nil)
					"]+"))
	    str)
	(while (re-search-forward alnum-2byte-regexp nil t)
	  (setq str (buffer-substring (match-beginning 0) (match-end 0)))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert (mapconcat (lambda (c)
			       (char-to-string (tcode-2-to-1 c)))
			     (string-to-list str)
			     nil)))))))

(defun tcode-2-to-1 (char)
  "2バイト英数字CHARを1バイトに変換する。"
  (tcode-check-alnum-1-to-2-table)
  (let ((ch 0))
    (catch 'found
      (while (< ch 95)
	(if (= (aref tcode-alnum-1-to-2-table ch) char)
	    (throw 'found (+ ch 32)))
	(setq ch (1+ ch)))
      char)))

;;
;; コード表
;;

(defun tcode-switch-variable (&optional arg)
  "(`tcode-table' 中の) 変数の値を切り替える。
切り替える変数とその値は `tcode-switch-table-list' で指定する。
ARG が nil でないとき、ARG 番目の組に切り替える。"
  (interactive "P")
  (message
   (mapconcat
    'identity
    (mapcar
     (lambda (elm)
       (set (car elm) (cdr elm)))
     (let ((table (nth (setq tcode-current-switch-table
			     (if arg
				 (1- (prefix-numeric-value arg))
			       (1+ tcode-current-switch-table)))
		       tcode-switch-table-list)))
       (unless table
	 (setq tcode-current-switch-table 0
	       table (car tcode-switch-table-list)))
       table))
    nil)))

(defun tcode-function-p (obj)
  (cond ((or (null obj)
	     (arrayp obj))
	 nil)
	((symbolp obj)
	 (fboundp obj))
	((consp obj)
	 (eq (car obj) 'lambda))
	(t
	 nil)))

(defun tcode-put-stroke-property (value strokes &optional without-overwrite)
  (cond ((char-or-string-p value)
	 (let ((str (if (stringp value)
			value
		      (char-to-string value))))
	   (when strokes
	     (if (not without-overwrite)
		 ;; remove old property
		 (tcode-put-stroke-property
		  (cdr (tcode-decode (append strokes nil)))
		  nil
		  without-overwrite)))
	   (put (intern str tcode-stroke-table) 'stroke strokes)))
	((and (symbolp value)
	      (boundp value))
	 (let ((value (eval value)))
	   (if (char-or-string-p value)
	       (tcode-put-stroke-property value strokes without-overwrite))))))

(defun tcode-set-stroke-property (table strokes &optional
					without-overwrite prefix)
  (cond ((or (null table)
	     (tcode-function-p table)))
	((char-or-string-p table)
	 (tcode-put-stroke-property table (vconcat prefix strokes)
				    without-overwrite))
	((consp table)
	 (mapcar (lambda (ent)
		   (tcode-set-stroke-property (cdr ent)
					      (append strokes
						      (list (car ent)))
					      without-overwrite prefix))
		 table))
	((vectorp table)
	 (let ((i 0))
	   (while (< i (length table))
	     (tcode-set-stroke-property (aref table i)
					(append strokes (list i))
					without-overwrite prefix)
	     (setq i (1+ i)))))
	((and (symbolp table)
	      (boundp table))
	 (tcode-set-stroke-property (eval table) strokes
				    without-overwrite prefix))))

;; pointer := symbol | (vector . pos) | (key . value)
(defun tcode-pointer-ref (pointer)
  (cond ((symbolp pointer)
	 (symbol-value pointer))
	((and (consp pointer) (vectorp (car pointer)))
	 (aref (car pointer) (cdr pointer)))
	(t (cdr pointer))))

(defun tcode-pointer-set (pointer value)
  (cond ((symbolp pointer)
	 (set pointer value))
	((and (consp pointer) (vectorp (car pointer)))
	 (aset (car pointer) (cdr pointer) value))
	(t (setcdr pointer value))))

(defun tcode-set-action (table-pointer strokes value)
  "TABLE-POINTER 中の、キー入力 STROKES に相当する所に VALUE を設定する。"
  (let ((table (tcode-pointer-ref table-pointer))
	(key (car strokes)))
    (unless (consp strokes)
      (error "STROKES must be a non-null list: %s" strokes))
    (unless (natnump key)
      (error "KEY must be a non-negative integer: %s" key))
    (unless (< key  tcode-key-num)
      (error "KEY must be less than %s: %s" tcode-key-num key))
	     (if (vectorp table)
		 (progn
	  (unless (< key (length table))
	    (setq table (vconcat table (make-vector (- key -1 (length table))
						    nil)))
	    (tcode-pointer-set table-pointer table))
	  (if (null (cdr strokes))
		   (aset table key value)
	    (tcode-set-action (cons table key) (cdr strokes) value)))
      (let ((cell (assq key table)))
	(unless cell
	  (setq cell (list key))
	  (tcode-pointer-set table-pointer (cons cell table)))
	(if (null (cdr strokes))
	    (setcdr cell value)
	  (tcode-set-action cell (cdr strokes) value))
	))))

(defun tcode-set-action-to-table (strokes value)
  "コード入力用の内部テーブルに入力列 STROKES に対する VALUE を設定する。
動作(VALUE)として指定できるのは以下のとおり。
    - コマンド (symbol)		そのコマンドを実行する。
    - 関数 (symbol, lambda式)	その関数を引数なしで呼ぶ。
    - 変数 (symbol)		評価した結果の動作を行う。
    - 表 (vector)		更にその表に従った動作を行う。
    - リスト (list)		更にそのリストに従った動作を行う。
    - 文字列 (string)		その文字列を挿入する。
    - 文字 (char)		その文字を挿入する。

  入力列はキーの番地のリストまたはキーの番地。
キーの番地を指定すると、最後に SPC を押したときの動作を設定する。"
  (cond ((consp strokes)
	 (tcode-set-stroke-property value strokes)
	 (tcode-set-action 'tcode-table strokes value))
	((and (char-or-string-p strokes)
	      (not (stringp strokes)))
	 (unless tcode-another-table
	   (setq tcode-another-table (make-vector (max 40 strokes) nil)))
	 (aset tcode-another-table strokes value))
	((null strokes)
	 (setq tcode-table value))
	(t
	 (error "入力列の指定が無効です。"))))

(defun tcode-load-table-1 (filename &optional prefix)
  (let ((k1 0) k2 newval char new-table)
    (load filename)
    (setq new-table (make-vector 40 nil))
    (while (< k1 40)
      (aset new-table k1 (make-vector 40 nil))
      (setq k1 (1+ k1)))
    (setq k1 0)
    (while (< k1 40)
      (let ((v (aref tcode-tbl k1)))
	(if (null v)
	    ()
	  (setq newval (vconcat (delq ?  (string-to-list v))))
	  (unless (= (length newval) 40)
	    (error "Table corrupted at line %d." (1+ k1)))
	  (setq k2 0)
	  (while (< k2 40)
	    (unless (memq (setq char (aref newval k2))
			  tcode-non-2-stroke-char-list)
	      (aset (aref new-table k2) k1 char))
	    (setq k2 (1+ k2)))))
      (setq k1 (1+ k1)))
    (setq tcode-tbl nil)		; free
    ;; 'stroke property を入れる。
    (tcode-set-action-to-table prefix new-table)
    (tcode-set-stroke-property new-table nil t prefix)
    ;; コマンドをテーブルに登録する。
    (mapc (lambda (elm)
	    (tcode-set-action-to-table (append prefix (car elm)) (cdr elm)))
	  tcode-special-commands-alist)
    (setq tcode-special-commands-alist nil) ; free
    ))

(defun tcode-load-table (filename)
  (setq tcode-key-num 40
	tcode-ext-keys nil)
  (setq tcode-special-prefix-alist nil)
  (setq tcode-stroke-to-string-function nil)
  (setq eelll-key-table nil)
  (tcode-set-key-layout tcode-key-layout)
  (run-hooks 'tcode-before-load-table-hook)
  (setq tcode-stroke-table (and (> tcode-stroke-table-size 0)
				(make-vector tcode-stroke-table-size 0)))
  (tcode-load-table-1 filename)
  (mapc (lambda (x) (tcode-load-table-1 (nth 1 x)
					(car x)))
	tcode-special-prefix-alist)
  (let ((l tcode-ext-keys)
	(i 40))
    (while l
      (if (car l)
	  (aset tcode-key-translation-rule-table (- (car l) ? ) i))
      (setq i (1+ i)
	    l (cdr l))))
    (if (get-buffer tcode-stroke-buffer-name)
	(kill-buffer tcode-stroke-buffer-name))
    (run-hooks 'tcode-after-load-table-hook)
    (tcode-bushu-init 1)
    (tcode-clear)
  (tcode-mode-line-redisplay))

(defun tcode-encode (char)
  "文字CHARを打ち方(キーのリスト)に変換する。直接入力できなければnilを返す。"
  (let ((ch (char-to-string char)))
    (or (append (get (intern-soft ch tcode-stroke-table) 'stroke) nil)
	(let ((ch (char-to-string (tcode-2-to-1 char))))
	  (append (get (intern-soft ch tcode-stroke-table) 'stroke) nil)))))

;;
;; 初期設定
;;

(defun tcode-bushu-init (level)
  "部首合成変換の初期化を行う。
引数LEVELが変数`tcode-bushu-on-demand'より小さかったら行わない。"
  (interactive (list 999))
  (when (>= level tcode-bushu-on-demand)
    (require 'tc-bushu)
    (tcode-bushu-load-dictionary)
    (run-hooks 'tcode-bushu-ready-hook)))

(defun tcode-initialize ()
  "コード表などの初期化を行う。
Emacsが起動されてから最初のtcode-modeで実行される。
`tcode-ready-hook' を呼ぶ。"
  (when (and tcode-init-file-name
	     (not (file-exists-p tcode-init-file-name)))
    (if (y-or-n-p (format "設定ファイル%sがありません。作成しますか?"
			  tcode-init-file-name))
	(tcode-install)
      (call-interactively 'tcode-set-key-layout)))
  (if (and tcode-data-directory
	   (not (file-exists-p tcode-data-directory)))
      (error "`tcode-data-directory'(%s)が存在しません。"
	     tcode-data-directory))
  (tcode--load-isearch)
  (add-hook 'minibuffer-exit-hook 'tcode-exit-minibuffer)
  (when (and (fboundp 'inactivate-input-method)
	     (fboundp 'defadvice))
    (defadvice inactivate-input-method (after
					turn-off-current-input-method-title
					activate)
      "Turn off `current-input-method-title' for mode line."
      (and (string= (car input-method-history)
		    tcode-current-package)
	   (setq current-input-method-title
		 tcode-transparent-mode-indicator)))
    (defadvice activate-input-method (after
				      turn-on-current-input-method-title
				      activate)
      "Turn on `current-input-method-title' for mode line."
      (and (string= (car input-method-history)
		    tcode-current-package)
	   (tcode-mode-line-redisplay))))
  (provide 'tcode-ready)
  (run-hooks 'tcode-ready-hook)
  (tcode-verbose-message
   (tcode-substitute-command-keys
    "Tコードモードでは「\\[tcode-mode-help]」でヘルプが表示されます。")))

;;
;; モードの切り替え
;;

(defun tcode-mode-line-redisplay ()
  (setq current-input-method-title
	(if (tcode-on-p)
	    (concat (if tcode-alnum-2-byte
			tcode-alnum-2byte-tcode-mode-indicator
		      tcode-tcode-mode-indicator)
		    (if tcode-katakana-mode
			tcode-katakana-mode-indicator
		      tcode-hiragana-mode-indicator))
	  tcode-transparent-mode-indicator))
  (and (window-minibuffer-p (selected-window))
       (boundp 'minibuffer-preprompt)
       (setq minibuffer-preprompt
	     (list "[" (eval tcode-mode-indicator) "]")))
  (set-buffer-modified-p (buffer-modified-p)))

(defun tcode-clear ()
  "ややこしいモードに入っているのを全部クリアする。
ヘルプ用ウィンドウも消去する。"
  (interactive)
  (tcode-auto-remove-help t)
  (run-hooks 'tcode-clear-hook))

;;;###autoload
(defun tcode-activate (&optional arg)
  "Tコードモードを有効にする。ARGが負の整数のときは無効にする。

Tコードモードについては、\\[tcode-mode-help] で表示されるヘルプを参照。"
  (if (and arg
	   (< (prefix-numeric-value arg) 0))
      ;; inactivate T-Code mode
      (unwind-protect
	  (progn
	    (if (window-minibuffer-p (selected-window))
		(setq tcode-mode-in-minibuffer nil))
	    (setq tcode-mode nil
		  tcode-self-insert-non-undo-count 1)
	    (tcode-clear)
	    (run-hooks 'input-method-deactivate-hook))
	(setq input-method-function nil))
    ;; activate T-Code mode
    (if (window-minibuffer-p (selected-window))
	(setq tcode-mode-in-minibuffer t))
    (setq tcode-mode t
	  tcode-self-insert-non-undo-count 1)
    (unless (featurep 'tcode-ready)
      (tcode-initialize))
    (unless tcode-ready-in-this-buffer
      ;; バッファごとのTコードの初期化を行う。`tcode-mode-hook'を呼ぶ。
      (setq tcode-ready-in-this-buffer t)
      (run-hooks 'tcode-mode-hook))
    (run-hooks 'input-method-activate-hook)
    (when tcode-use-input-method
      (set (make-local-variable 'input-method-function)
	   'tcode-input-method)))
  (run-hooks 'tcode-toggle-hook)
  (tcode-mode-line-redisplay))

(defun tcode-inactivate ()
  "Tコードモードを無効にする。"
  (tcode-activate -1))

(defun tcode-exit-minibuffer ()
  (tcode-inactivate)
  (setq current-input-method nil)
  (if (boundp 'minibuffer-preprompt)
      (setq minibuffer-preprompt nil)))

(defun tcode-toggle-katakana-mode (arg)
  "カタカナモードを切り替える。"
  (interactive "P")
  (setq tcode-katakana-mode (if (null arg)
				(not tcode-katakana-mode)
			      (>= (prefix-numeric-value arg) 0)))
  (tcode-mode-line-redisplay))

;;
;; input method interface
;;
(defvar tcode-current-package nil)

(defvar tcode-transparent-mode-indicator "--")

;;;###autoload
(defun tcode-use-package (package-name &rest libraries)
  "Start using T-Code package PACKAGE-NAME.
The remaining arguments are libraries to be loaded before using the package."
  (if (equal tcode-current-package package-name)
      ()
    (let ((table-name (cdr (assoc package-name tcode-package-name-alist))))
      (or table-name
	  (error "No package named %s" package-name))
      (while libraries
	(load (car libraries))
	(setq libraries (cdr libraries)))
      (tcode-load-table table-name))
    (setq tcode-current-package package-name))
  (setq deactivate-current-input-method-function 'tcode-inactivate
	describe-current-input-method-function 'tcode-mode-help)
  (setq current-input-method-title 'tcode-mode-indicator)
  (tcode-activate))

;;
;; input method emulation
;;

(defvar tcode-self-insert-non-undo-count 0)
(defvar tcode-cancel-undo-boundary-commands '(self-insert-command))

(defun tcode-cancel-undo-boundary ()
  "文字を挿入したときに、まとめて undo できるように調整する。"
  (if (or (not (memq last-command
		     (if (memq this-command
			       tcode-cancel-undo-boundary-commands)
			 tcode-cancel-undo-boundary-commands
		       (setq tcode-cancel-undo-boundary-commands
			     (cons this-command
				   tcode-cancel-undo-boundary-commands)))))
	  (>= tcode-self-insert-non-undo-count (if (tcode-on-p) 10 20)))
      (progn
	(undo-boundary)
	(setq tcode-self-insert-non-undo-count 1))
    (cancel-undo-boundary)
    (setq tcode-self-insert-non-undo-count
	  (1+ tcode-self-insert-non-undo-count))))

(defun tcode-self-insert-command (&optional arg)
  "Encode Tcode character and insert."
  (interactive "P")
  (tcode-cancel-undo-boundary)
  (let ((events (tcode-input-method last-command-event)))
    (while events
      (let* ((ch (car events))
	     (command (tcode-default-key-binding (char-to-string ch))))
	(if (not (commandp command))
	    (setq command 'self-insert-command))
	(setq prefix-arg current-prefix-arg
	      this-command command
	      last-command-event ch) ; for self-insert-command
	(command-execute command))
      (setq events (cdr events)))))

(unless tcode-use-input-method
  ;; マイナーモード
  (unless (assq 'tcode-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons (list 'tcode-mode nil) minor-mode-alist)))

  (defvar tcode-minor-mode-map nil)

  (if (boundp 'minor-mode-map-alist)
      (progn
	(unless tcode-minor-mode-map
	  (setq tcode-minor-mode-map (make-keymap))
	  (let ((i ? ))
	    (while (< i ?~)
	      (define-key tcode-minor-mode-map
		(char-to-string i)
		'tcode-self-insert-command)
	      (setq i (1+ i)))))
	;; 自動的にマイナーモードキーマップに変更
	(or (assq 'tcode-mode minor-mode-map-alist)
	    (setq minor-mode-map-alist
		  (cons (cons 'tcode-mode tcode-minor-mode-map)
			minor-mode-map-alist)))

	(defadvice tcode-activate (after
				   raise-minor-mode-map
				   activate)
	  "Raise minor mode keymap."
	  ;; detach and re-attach tcode-minor-mode-map
	  (setq minor-mode-map-alist 
		(let ((map (or (assq 'tcode-mode minor-mode-map-alist)
			       (cons 'tcode-mode tcode-minor-mode-map))))
		  (cons map (delq map minor-mode-map-alist)))))

	(defun tcode-mode (&optional arg)
	  "T-Code mode.
Type \\[tcode-mode-help] for more detail."
	  (interactive "P")
	  (tcode-activate (or arg
			      (if tcode-mode -1 1)))
	  (force-mode-line-update)))
    ;; 自前でマイナーモードキーマップに変更
    (make-variable-buffer-local 'tcode-minor-mode-map)
    (defvar tcode-original-local-map nil)
    (make-variable-buffer-local 'tcode-original-local-map)

    ;; override
    (defun tcode-default-key-binding (key)
      "Return the binding for command KEY in non-tcode-mode."
      (or (lookup-key tcode-original-local-map key)
	  (lookup-key global-map key)))

    (unless (fboundp 'tcode:tcode-activate)
      (fset 'tcode:tcode-activate
	    (symbol-function 'tcode-activate))
      (defun tcode-activate (&optional arg)
	(or tcode-mode
	    (setq tcode-original-local-map (or (current-local-map)
					       (current-global-map))))
	(unless tcode-minor-mode-map
	  (setq tcode-minor-mode-map (if tcode-original-local-map
					 (copy-keymap tcode-original-local-map)
				       (make-sparse-keymap)))
	  (let ((i ? ))
	    (while (< i ?~)
	      (define-key tcode-minor-mode-map
		(char-to-string i)
		'tcode-self-insert-command)
	      (setq i (1+ i)))))
	(tcode:tcode-activate arg)
	(use-local-map (if tcode-mode
			   tcode-minor-mode-map
			 (or tcode-original-local-map
			     (current-local-map)
			     (current-global-map))))))))

;;
;; Help
;;

(defun tcode-action-to-printable (action)
  (cond ((or (null action)
	     (stringp action))
	 action)
	((char-or-string-p action)
	 (char-to-string action))
	((and (symbolp action)
	      (boundp action))
	 (tcode-action-to-printable (eval action)))
	(t
	 "*")))

(defun tcode-draw-current-table (table)
  "TABLE から、次の入力により挿入される文字等を表す一覧表を描く。"
  (tcode-draw-table
   (if (vectorp table)
       (let ((draw-table (copy-sequence table))
	     (i 0))
	 (while (< i 40)
	   (aset draw-table i (tcode-action-to-printable (aref draw-table i)))
	   (setq i (1+ i)))
	 draw-table)
     ;; table はリスト
     (let ((draw-table (make-vector 40 nil)))
       (mapc (lambda (elm)
	       (aset draw-table
		     (car elm)
		     (tcode-action-to-printable (cdr elm))))
	     table)
       draw-table))
   1 1))

(defun tcode-verbose-message (message &optional non-verbose-message)
  "変数 `tcode-verbose-message' が non-nil の場合には、 MESSAGE を表示する。
そうでないとき、 NON-VERBOSE-MESSAGE があれば、これを表示する。"
  (if (or tcode-verbose-message
	  non-verbose-message)
      (message (if tcode-verbose-message message non-verbose-message))))

(defun tcode-draw-table (table page whole-page)
  "一覧表を TABLE に基づき描く。表示はしない。"
  (let ((buf (get-buffer-create " *tcode: table*"))
        (sep0 [" " " " " " " " "  " "  " "  " " " " " " " ""])
        (sep1 ["[" " " " " " " "] " "  " " [" " " " " " " "]"])
        (none-str "-"))
    (with-current-buffer buf
      (erase-buffer)
      (let ((fv (make-vector 10 nil))
	    (i 0)
	    (offset 0))
        (while (< i 10)
          (aset fv i (format "%%%s%ds"
                             (if (>= i 5)
				 ""
			       "-")
                             (apply 'max 4
                                    (mapcar
                                     (lambda (x)
                                       (string-width
                                        (or (aref table (+ x i)) none-str)))
                                     '(0 10 20 30)))))
          (setq i (1+ i)))
        (while (< offset 40)
          (setq i 0)
          (while (< i 10)
            (insert (aref (if (= offset 0) sep0 sep1) i)
                    (format (aref fv i) (or (aref table (+ offset i))
                                            none-str)))
            (setq i (1+ i)))
          (insert (aref (if (= offset 0) sep0 sep1) 10) "\n")
          (setq offset (+ offset 10))))
      (unless (= whole-page 1)
        (backward-char 1)
        (insert (format "     (%d/%d)" page whole-page))))
    buf))

(defun tcode-display-help-buffer (buffer &optional display-only append)
  "\"*T-Code Help*\" というバッファに BUFFER の内容を表示する。
表示した直後に空白が入力されると、 DISPLAY-ONLY が nil ならばこのバッファ
を消去する。 APPEND が nil でないときは、前の内容に追加して表示する。"
  ;; ウィンドウ構成の保存
  (unless (get-buffer-window tcode-help-buffer-name)
    (setq tcode-window-configuration-before-help
	  (if (one-window-p)
	      nil
	    (current-window-configuration))))
  ;; 表示する内容を作成し、表示する
  (let (previous-contents)
    (if append
	(let ((buf (get-buffer tcode-help-buffer-name)))
	  (setq previous-contents (and buf
				       (with-current-buffer buf
					 (buffer-string))))))
    (with-output-to-temp-buffer tcode-help-buffer-name
      (when previous-contents
	(princ previous-contents)
	(princ "\n"))
      (princ (with-current-buffer buffer (buffer-string))))
    (if (fboundp 'help-mode)
	(with-current-buffer (get-buffer tcode-help-buffer-name)
	  (help-mode))))
  ;; ウィンドウの大きさの調整
  (let ((orig-win (selected-window))
	(new-win (get-buffer-window tcode-help-buffer-name))
	(window-min-height 2))
    (when new-win
      (select-window new-win)
      (if (and (or (not tcode-window-configuration-before-help)
		   tcode-adjust-window-for-help)
	       (= (frame-width) (window-width)))
	  (enlarge-window (- (1+ (min (count-lines (point-min) (point-max))
				      tcode-help-window-height-max))
			     (- (window-height)
				tcode-adjust-window-additional-height))))
      (set-window-start (selected-window) (point-min))
      (unless (one-window-p)
	(select-window orig-win))))
  ;; 表示後の処理
  (setq tcode-auto-remove-help-current-count 0)
  (unless (or display-only
	      (memq this-command tcode-no-wait-display-help-command-list))
    (tcode-verbose-message "スペースキーでヘルプが消えます。" " ")
    (let ((ch (read-char)))
      (if (/= ch ? )
	  (tcode-redo-command ch)
	(tcode-auto-remove-help t))
      (message ""))))

(defun tcode-auto-remove-help (&optional immediate)
  "ヘルプを自動的に消去する。
消去されるのは、ヘルプが表示されてから
この関数が `tcode-auto-remove-help-count' 回呼ばれたとき。"
  (when (or immediate
	    (and tcode-auto-remove-help-count
		 (>= (setq tcode-auto-remove-help-current-count
			   (1+ tcode-auto-remove-help-current-count))
		     tcode-auto-remove-help-count)))
    (let ((help-buf (get-buffer tcode-help-buffer-name))
	  help-win)
      (and help-buf
	   (not (eq help-buf (current-buffer)))
	   (setq help-win (get-buffer-window help-buf))
	   (cond (tcode-window-configuration-before-help
                  (save-excursion
		    (if tcode-adjust-window-for-help
			(set-window-configuration
                         tcode-window-configuration-before-help))))
		 ((not (one-window-p))
		  (delete-window help-win))))
      (and help-buf
	   (or immediate
	       (not (eq help-buf (current-buffer))))
	   (kill-buffer help-buf)))))

;;;
;;; 各モジュールで使用する汎用関数
;;;

(defun tcode-removable-fill-prefix-p ()
  "取り除いてもよい fill-prefix か。
取り除いてもよい fill-prefix とは、
行頭から point までが fill-prefix であり、かつ
その前の行も fill-prefix で始まっている場合をいう。"
  (and fill-prefix
       (and (string= fill-prefix
		     (buffer-substring (save-excursion
					 (beginning-of-line)
					 (point))
				       (point)))
	    (save-excursion
	      (and (= (forward-line -1) 0)
		   (looking-at (regexp-quote fill-prefix)))))))

(defun tcode-skip-blank-backward ()
  "行頭から point までが空白なら point を前の行の行末に移動する。
`fill-prefix'が設定されているときは、その文字列も無視する。"
  (let ((p (point))
	(fill-prefix-end (and fill-prefix
			      (save-excursion
				(beginning-of-line)
				(and (looking-at (regexp-quote fill-prefix))
				     (match-end 0))))))
    (cond ((bobp)
	   nil)
	  ((bolp)
	   ;; 前の行へ
	   (forward-line -1)
	   (end-of-line)
	   (if (bobp)
	       nil
	     (point)))
	  ((null fill-prefix-end) ; fill-prefix がない場合。
	   (if (save-excursion
		 (beginning-of-line)
		 (or (not (re-search-forward "^\\s +" p t))
		     (/= (point) p)))
	       p
	     ;; 行頭からの空白をとばす
	     (forward-line -1)
	     (end-of-line)
	     (if (bobp)
		 nil
	       (point))))
	  ((not (save-excursion
		  (goto-char fill-prefix-end)
		  (tcode-removable-fill-prefix-p)))
	   ;; 直前の行が fill-prefix で始まっていない場合。
	   (if (= fill-prefix-end p)
	       nil    ; 取り除いてはいけない fill-prefix
	     p))
	  ((<= p fill-prefix-end) ; fill-prefix の中にいる場合。
	   (forward-line -1)
	   (end-of-line)
	   (if (bobp)
	       nil
	     (point)))
	  (t		  ; fill-prefix + 文字列の場合。
	   (if (save-excursion
		 (beginning-of-line)
		 (or (not (re-search-forward
			   (concat "^" (regexp-quote fill-prefix) "\\s +")
			   p t))
		     (/= (point) p)))
	       p
	     ;; fill-prefix + 空白をとばす。
	     (forward-line -1)
	     (end-of-line)
	     (point))))))

(defun tcode-scan-backward (max &optional terminate-char-list)
  "現 point より先頭方向にある日本語列または英単語一つのリストを返す。
リストの要素は(POINT . \"文字列\")である。
ここで、「文字列」は、日本語文字の場合1文字、英文字の場合は1単語である。
リストの順番としては、バッファの先頭に近い文字列が先頭の側になる。
リストの長さは最大 MAX 文字である。"
  (save-excursion
    (let (ch context)
      (while (and (< (length context) max)
		  (tcode-skip-blank-backward)
		  (or (not (memq (setq ch (tcode-preceding-char))
				 terminate-char-list))
		      (null context))
		  (not (bobp)))
	(if (<= ch 255)
  	    ;; アルファベットの場合
	    (let ((end (point))
		  (beg (progn
			 (if (= (char-syntax ch) ?w)
			     ;; 語
			     (while (and (not (bobp))
					 (= (char-syntax
					     (setq ch (tcode-preceding-char)))
					    ?w)
					 (<= ch 255))
			       (tcode-forward-char -1))
			   ;; 記号
			   (tcode-forward-char -1))
			 (point))))
	      (setq context (cons
			     (cons beg
				   (buffer-substring-no-properties beg end))
			     context)))
	  ;; 日本語文字の場合(1字得る)
	  (tcode-forward-char -1)
	  (setq context (cons (cons (point) (char-to-string ch))
			      context))))
      context)))

(defun tcode-path-for-read (file)
  (let ((user-file (expand-file-name file tcode-data-directory)))
    (if (file-exists-p user-file)
	user-file
      (expand-file-name file tcode-site-data-directory))))

(defun tcode-path-for-write (file)
  (expand-file-name file tcode-data-directory))

(defun tcode-set-work-buffer (bufname filename &optional force noerror)
  "編集対象バッファを、FILENAME の内容を持つバッファ BUFNAME にする。
ファイル FILENAME がまだ読み込まれていない場合には読み込む。
返り値として、設定されたバッファを返す。

FORCE が nil でないときは、再読み込みを行う。
NOERROR が nil でないときは、FILENAME が指定されていない場合や
ファイルが存在しない場合でも、空のバッファを作成する。"
  (let ((buffer (get-buffer bufname))
	(file (tcode-path-for-read filename)))
    (if (and buffer
	     (not force))
	(set-buffer buffer)
      (if (and file (file-exists-p file))
	  (prog2
	      (when tcode-verbose-message
		(message "ファイル %s 読み込み中..." file))
	      (set-buffer (get-buffer-create bufname))
	    (erase-buffer)
	    (insert-file-contents file)
	    (set-buffer-modified-p nil)
	    (when tcode-verbose-message
	      (message "ファイル %s 読み込み中...完了" file)))
	(if noerror
	    (set-buffer (get-buffer-create bufname))
	  (error "ファイル %s が存在しません。" file))))))

(defun tcode-save-buffer (bufname filename &optional backup-inhibited)
  "BUFNAMEのバッファが変更されていればFILENAMEのファイルに保存する。
BACKUP-INHIBITED が nil でない場合は、バックアップファイルの作成を
行わない。"
  (let ((buffer (get-buffer bufname))
	(file-path (tcode-path-for-write filename)))
    (when (and tcode-data-directory
	       buffer
	       (buffer-modified-p buffer)
	       (file-writable-p file-path))
      (with-current-buffer buffer
	(unless (or backup-inhibited
		    (not (file-exists-p file-path)))
	  (rename-file file-path (make-backup-file-name file-path) t))
	(write-region (point-min) (point-max) file-path)
	(set-buffer-modified-p nil)))))

;;
;; Emacs終了時のデータ保存
;;

(defun tcode-save-dictionaries (&optional backup-inhibited)
  "Tコードで用いる辞書類を、変更されていれば保存する。"
  (interactive)
  (mapcar (lambda (dic)
	    (let ((bufname (car dic))
		  (filename (cdr dic)))
	      (tcode-save-buffer bufname filename
				 backup-inhibited)))
	  tcode-dictionaries))

(defun tcode-kill-emacs-function ()
  (tcode-save-dictionaries)
  (tcode-record))

(add-hook 'kill-emacs-hook 'tcode-kill-emacs-function)

(defun tcode-record ()
  (when (and tcode-record-file-name
	     (> tcode-number-strokes 0))
    (let ((bufname " *tcode: record*"))
      (save-excursion
	(tcode-set-work-buffer bufname tcode-record-file-name nil t)
	(goto-char (point-max))
	(insert
	 (format (concat "%s  文字: %4d  部首: %3d(%d%%)  "
			 "交ぜ書き: %3d(%d%%)  機能: %3d(%d%%)\n")
		 (let ((time (current-time-string)))
		   (if (not (string-match "^... \\(.*:.*\\):" time))
		       ""
		     (substring time (match-beginning 1) (match-end 1))))
		 tcode-input-chars
		 tcode-bushu-occurrence
		 (/ (* 100 tcode-bushu-occurrence) tcode-number-strokes)
		 tcode-mazegaki-occurrence
		 (/ (* 100 tcode-mazegaki-occurrence) tcode-number-strokes)
		 tcode-special-occurrence
		 (/ (* 100 tcode-special-occurrence) tcode-number-strokes)))
	(tcode-save-buffer bufname tcode-record-file-name t)))))

(provide 'tc)

;; 部首合成変換の初期設定
(tcode-bushu-init 0)

;;; tc.el ends here
