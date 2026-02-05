;;; tctest.el --- T-Code input method tests. -*- lexical-binding: nil -*-

;;  ## 実行方法:
;;    - emacs -Q で起動する。
;;    - (FIXME: .tc や ~/tcode/ の設定はどうする? ひとまず、できるだけ
;;      初期状態のものを使うとする。)
;;    - load-path や (setq tcode-use-isearch ...) など、tcode の初期設
;;      定を行なう。toggle-input-method で、japanese-T-Code が起動する
;;      こと。
;;    - (require 'tc-setup)
;;    - (require 'tctest)
;;    - M-x ert RET t RET
;;       * 「t」は全てのテストを実行する。詳しくは ert の info 参照。
;;
;;  ## 結果確認方法
;;    - 最初の統計情報のところに unexpected と書かれていないこと。
;;       例:  Failed 3 (1 unexpected)  ; unexpected とあるので失敗。
;;       例:  Failed 2                 ; unexpected がないので成功。
;;                                      (known bug のみ fail ということ。)
;;    - 時刻表示の下のプログレスバーが赤くなることでも失敗がわかる。
;;
;;  ## 個別実行方法
;;    - 次の式を評価した上で(テスト結果バッファを消さない設定)、
;;       (puthash :show-buf t tctest-cmp-default-params)
;;    - (tctest-cmp ...) の式を評価する。
;;
;;  ## テストプログラムの読み方
;;    - ert-deftest、should-not の意味については、ert の info 参照。
;;    - (tctest-cmp  "C-f C-f cdef"  ; このようにキーを押すと、
;;          :initial "ab"            ; もしバッファの初期内容がこうなら、
;;          :expect "abcdef<!>"      ; 最終的にバッファはこうなる。
;;                                     <!> はポイントの位置。
;;    - tctest-cmp はテスト成功時 nil を返すので、should-not と組み合
;;      わせる。失敗時は最終バッファ内容を返す。これが失敗リストに
;;      value: として表示される。
;;
;;  ## tctest-cmp 第1引数のキー入力文字列の自動変換
;;    - キー入力文字列には、kbd 関数の引数と同じ記法を用いる(C-x RET
;;      SPC など)。
;;    - 直接入力可能な日本語文字は、キー列を直接書く代わりに、日本語文
;;      字をそのまま書くことができる(テストを読みやすくするため、また、
;;      qwerty/dvorak の設定に依存しないテストコードにするため)。
;;      tctest-cmp 内部で T-Code 表を使って ASCII キー列に変換したもの
;;      が入力として用いられる(例: "あい" はqwerty 環境では "yfhd" が
;;      入力キー列となる)。
;;    - 以下のキーワードは、対応するキー列に変換される。
;;       [IMON]  : C-\ (toggle-input-method)
;;       [IMOFF] : 同上 (テストの意味がわかりやすいよう区別して書く。)
;;       [UNDO]  : C-x u (undo)
;;       [PREMAZE]   : 前置交ぜ書き変換
;;       [POSTMAZE]  : 後置交ぜ書き変換
;;       [PREBUSHU]  : 前置部首合成変換
;;       [POSTBUSHU] : 後置部首合成変換
;;       [ALNUM_ZEN] : 全角英数/半角英数切り換え
;;       [ALNUM_HAN] : 同上
;;       [KUTEN_J]   : 日本語句読点/ASCII句読点切り換え
;;       [KUTEN_A]   : 同上
;;    - 以下のキーワードは、特殊な関数を呼び出すキー列に変換される。
;;       [MATCH]     : isearch のマッチ範囲を[]で囲む。
;;       [MODE]      : モードラインの input method 状態表示部分を挿入する。
;;
;;  ## カタカナモードについて
;;    - TUT-Code 用の機能であるカタカナモードをテストするために、
;;      tctest-bind-katakana という関数が用意してある。これを
;;      tctest-cmp の引数 :setup-fun を使って呼び出すことで、テストの
;;      間だけ「#」がカタカナモード切り換えキーとなる。また、モードラ
;;      インの表示が [TC] から、[TCひ][TCカ] のようにカタカナモードの
;;      状態を表示するように切り換わる。

(require 'ert)
(require 'tctest-play)

(defvar tcode-use-isearch)
(defvar tcode-use-input-method)
(declare-function tcode-set-key      "tc")


;; 個別実行用の設定 (テスト結果バッファを消さない)
;;   (puthash :show-buf t tctest-cmp-default-params)

;;; *テスト環境の判別関数

(defun tctest-is-non-im ()
  "`im' 以外の isearch 実装を使っている。"
  (memq tcode-use-isearch '(overwrite advice)))

;;; *Test items

;;;
;;; *サンプルコード
;;;

(ert-deftest tctest-sample ()
  "テストの書き方の例。"
  (should-not (tctest-cmp "abcd" ; 入力キー列
    :initial "EFGH" ; バッファの初期内容
    :expect "abcd<!>EFGH" ; 最終バッファ内容。<!>は最終ポイント位置
    )))

(ert-deftest tctest-sample-keyword ()
  "テストの書き方の例。特に入力キー列の特殊キーワードについて。"
  ;; kbd 関数の受け取るキー表記法に加えて、いくつかのキーワードが使える。
  ;; [IMON]  : C-\ (toggle-input-method)
  ;; [IMOFF] : 同上 (テストの意味がわかりやすいよう区別して書く。)
  ;; 日本語文字は、T-Code 表を使って ASCII キー列に戻したものが使われる。
  ;; その他のキーワード:
  ;;   [POSTBUSHU] [PREBUSHU] [POSTMAZE] [PREMAZE] : 部首変換/交ぜ書き変換
  ;;   [ALNUM_ZEN] [ALNUM_HAN] : 全角英数/半角英数切り換え
  ;;   [KUTEN_J] [KUTEN_A]     : 日本語句読点/ASCII句読点切り換え
  (should-not (tctest-cmp "ab SPC cd-gh RET ij C-p M-f [IMON] あいう [IMOFF] ef"
    :expect "ab cdあいうef<!>-gh\nij")))

(ert-deftest tctest-sample-ding ()
  "テストの書き方の例。(ding)の鳴った場所に<DING>が書かれる。"
  ;; 実際は(ding)は鳴らない設定。keyboard macro を止めてしまうので。
  (should-not (tctest-cmp "[IMON] あ m8 い m8 う" ; m8 は文字割り当ての無い組
    :expect "あ<DING>い<DING>う<!>")))

(ert-deftest tctest-sample-match ()
  "特殊キーワード[MATCH]の例。"
  ;; [MATCH] : 直前のサーチのマッチ範囲を[]で囲むコマンド
  (should-not (tctest-cmp "C-s cde RET [MATCH]"
    :initial "... abcdefg"
    :expect "... ab[cde<!>]fg")))

(ert-deftest tctest-sample-ding-in-search ()
  "サーチ失敗の<DING>の例。実装の都合でマッチ開始場所に置かれるので注意。"
  ;; <DING>のふるまいがややこしい場合は、:no-ding t を指定する方がよいかも。
  (should-not (tctest-cmp "C-s abce [MATCH]"
    :initial "... a ab abc abcd"
    :expect "... a ab <DING>[abc<!>] abcd")))

;;;
;;; *基本機能
;;;

(ert-deftest tctest-prefix-maze ()
  "前置交ぜ書き変換ができる。"
  (should-not (tctest-cmp "[IMON] [PREMAZE] か手 SPC"
    :expect "歌手<!>")))

(ert-deftest tctest-prefix-bushu ()
  "前置部首変換ができる。"
  (should-not (tctest-cmp "[IMON] [PREBUSHU] 糸会"
    :expect "絵<!>")))

(ert-deftest tctest-prefix-bushu-nest-l ()
  "多段の前置部首変換ができる。(左結合型)"
  (should-not (tctest-cmp "[IMON] [PREBUSHU] [PREBUSHU] イヒサ"
    :expect "花<!>")))

(ert-deftest tctest-prefix-bushu-nest-r ()
  "多段の前置部首変換ができる。(右結合型)"
  (should-not (tctest-cmp "[IMON] [PREBUSHU] サ [PREBUSHU] イヒ"
    :expect "花<!>")))

(ert-deftest tctest-postfix-maze ()
  "後置交ぜ書き変換ができる。"
  (should-not (tctest-cmp "[IMON] か手 [POSTMAZE] RET"
    :expect "歌手<!>")))

(ert-deftest tctest-postfix-bushu ()
  "後置部首変換ができる。"
  (should-not (tctest-cmp "[IMON] 糸会 [POSTBUSHU]画"
    :expect "絵画<!>")))

(ert-deftest tctest-alnum2b ()
  "全角英数モードが使える。"
  (should-not (tctest-cmp
	       "[IMON] A b SPC [ALNUM_ZEN] A b SPC [ALNUM_HAN] A b SPC"
    :expect "AbＡｂAb<!>")))

(ert-deftest tctest-katakana ()
  "カタカナモードが使える。"
  (should-not (tctest-cmp "[IMON] あい # うえ # おか"
    :expect "あいウエおか<!>"
    :setup-fun #'tctest-bind-katakana)))

(ert-deftest tctest-kuten ()
  "句読点を変更できる。"
  (should-not (tctest-cmp "[IMON] 。、 [KUTEN_A] 。、 [KUTEN_J] 。、"
    :expect "。、. , 。、<!>")))

(ert-deftest tctest-katakana-in-prefix-maze ()
  "前置交ぜ書き変換中にカタカナモードに変更できる。"
  (should-not (tctest-cmp "[IMON] あ [PREMAZE] # え # SPC い"
    :expect "あヱい<!>"
    :setup-fun #'tctest-bind-katakana)))

(ert-deftest tctest-katakana-in-prefix-bushu ()
  "前置部首変換中にカタカナモードに変更できる。"
  (should-not (tctest-cmp "[IMON] あ [PREBUSHU] # い # い"
    :expect "あ似<!>"
    :setup-fun #'tctest-bind-katakana)))

(ert-deftest tctest-kuten-in-prefix-bushu ()
  "前置部首変換中に句点を変更できる。"
  (should-not (tctest-cmp "[IMON] [KUTEN_A] 、 [PREBUSHU] 大 [KUTEN_J] 、"
    :expect ", 犬<!>")))

;;;
;;; *tcode-isearch-start-state
;;;
(ert-deftest tctest-isearch-start-nil-off ()
  "tcode-isearch-start-state nil、IM off で isearch 開始時 IM off"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "am C-s ma [IMON] 人 [MATCH]"
    :initial "...人人..ma人..人ma..mama"
    :expect  "am...人人..[ma人<!>]..人ma..mama"
    :vars '((tcode-isearch-start-state nil)))))

(ert-deftest tctest-isearch-start-nil-on ()
  "tcode-isearch-start-state nil、IM on で isearch 開始時 IM on"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] 色 C-s 人 [IMOFF] ma [MATCH]"
    :initial "...人人..ma人..人ma..mama"
    :expect "色...人人..ma人..[人ma<!>]..mama"
    :vars '((tcode-isearch-start-state nil)))))

(ert-deftest tctest-isearch-start-0-off ()
  "tcode-isearch-start-state 0、IM off で isearch 開始時 IM off"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "am C-s ma [IMON] 人 [MATCH]"
    :initial "...人人..ma人..人ma..mama"
    :expect "am...人人..[ma人<!>]..人ma..mama"
    :vars '((tcode-isearch-start-state 0)))))

(ert-deftest tctest-isearch-start-0-on ()
  "tcode-isearch-start-state 0、IM on で isearch 開始時 IM off"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] 色 C-s ma [IMON] 人 [MATCH]"
    :initial "...人人..ma人..人ma..mama"
    :expect "色...人人..[ma人<!>]..人ma..mama"
    :vars '((tcode-isearch-start-state 0)))))

(ert-deftest tctest-isearch-start-1-off ()
  "tcode-isearch-start-state 1、IM off で isearch 開始時 IM on"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "am C-s 人 [IMOFF] ma [MATCH]"
    :initial "...人人..ma人..人ma..mama"
    :expect "am...人人..ma人..[人ma<!>]..mama"
    :vars '((tcode-isearch-start-state 1)))))

(ert-deftest tctest-isearch-start-1-on ()
  "tcode-isearch-start-state 1、IM on で isearch 開始時 IM on"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] 色 C-s 人 [IMOFF] ma [MATCH]"
    :initial "...人人..ma人..人ma..mama"
    :expect "色...人人..ma人..[人ma<!>]..mama"
    :vars '((tcode-isearch-start-state 1)))))

;;;
;;; *isearch 中の prefix argument
;;;
(ert-deftest tctest-isearch-prefix-ascii ()
  "isearch 中に prefix arg で ascii 文字を複数入力できる。"
  ;; 既知の問題。
  ;; https://github.com/kanchoku/tc/pull/30#discussion_r2602138439
  :expected-result (if (eq tcode-use-isearch 'overwrite) :failed :passed)
  (should-not (tctest-cmp "C-s C-u a [MATCH]"
    :initial "...a...aaaa"
    :expect  "...a...[aaaa<!>]")))

(ert-deftest tctest-isearch-prefix-tc-char ()
  "isearch 中に prefix arg で T-Code 文字を複数入力できる。"
  ;; 既知の問題。
  ;; https://github.com/kanchoku/tc/pull/30#discussion_r2602138439
  :expected-result (if (memq tcode-use-isearch '(advice overwrite)) :failed
		     :passed)
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] C-s C-u あ [MATCH]"
    :initial "...あ...ああああ"
    :expect  "...あ...[ああああ<!>]")))

;; 「。」は tcode--input-method-for-isearch の中で通常文字とは異なる
;; 経路を通るので、別テストとしてある。
(ert-deftest tctest-isearch-prefix-tc-str ()
  "isearch 中に prefix arg で句点を複数入力できる。"
  ;; 既知の問題。
  ;; https://github.com/kanchoku/tc/pull/30#discussion_r2602138439
  :expected-result (if (memq tcode-use-isearch '(advice overwrite)) :failed
		     :passed)
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] C-s C-u 。 [MATCH]"
    :initial "...。...。。。。"
    :expect  "...。...[。。。。<!>]")))

;;;
;;; *isearch 中の部首/交ぜ書き変換
;;;

(ert-deftest tctest-prefix-bushu-in-isearch ()
  "isearch 中に前置部首変換ができる。"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] C-s [PREBUSHU] イ反 [MATCH]"
    :initial "... イ反仮"
    :expect "... イ反[仮<!>]")))

(ert-deftest tctest-prefix-bushu-in-isearch-nest-l ()
  "isearch 中に多段の前置部首変換ができる。(左結合型)"
  ;; 非 im 実装では、合成途中でマッチが試みられて ding が発生するので回避。
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] C-s [PREBUSHU] [PREBUSHU] イヒサ [MATCH]"
    :initial "... イヒサ花"
    :expect "... イヒサ[花<!>]" :no-ding (tctest-is-non-im))))

(ert-deftest tctest-prefix-bushu-in-isearch-nest-r ()
  "isearch 中に多段の前置部首変換ができる。(右結合型)"
  ;; 非 im 実装では一段目で確定してしまう。
  ;; 既知の問題: https://github.com/kanchoku/tc/issues/41
  :expected-result (if (eq tcode-use-isearch 'im) :passed :failed)
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] C-s [PREBUSHU] サ [PREBUSHU] イヒ [MATCH]"
    :initial "... サイヒ花"
    :expect "... サイヒ[花<!>]")))

(ert-deftest tctest-prefix-maze-in-isearch ()
  "isearch 中に前置交ぜ書き変換ができる。"
  ;; 非 im 実装では確定後、minibufferを出るための RET が必要。次テストにて。
  (skip-unless (eq tcode-use-isearch 'im))
  (should-not (tctest-cmp "[IMON] C-s [PREMAZE] か手 SPC [MATCH]"
    :initial "...か手歌手"
    :expect "...か手[歌手<!>]")))

(ert-deftest tctest-prefix-maze-in-isearch-non-im ()
  "isearch 中に前置交ぜ書き変換ができる。非 'im 用。"
  ;; 非 im 実装では確定後、minibufferを出るための RET が必要。
  (skip-unless (tctest-is-non-im))
  (should-not (tctest-cmp "[IMON] C-s [PREMAZE] か手 SPC RET [MATCH]"
    :initial "...か手歌手"
    :expect "...か手[歌手<!>]")))

(ert-deftest tctest-postfix-bushu-in-isearch ()
  "isearch 中に後置部首変換ができる(変換前にマッチしないケース)。"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] C-s 糸会 [POSTBUSHU] [MATCH]"
    :initial "...絵画"
    :expect "<DING>...[絵<!>]画")))

(ert-deftest tctest-postfix-bushu-in-isearch-match-before ()
  "isearch 中に後置部首変換ができる(変換前に行き過ぎるケース)。"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] C-s 糸会 [POSTBUSHU] [MATCH]"
    :initial "...絵画 糸会"
    :expect "...[絵<!>]画 糸会")))

(ert-deftest tctest-postfix-maze-in-isearch ()
  "isearch 中に後置交ぜ書き変換ができる(変換前にマッチしないケース)。"
  ;; 非 im 実装では確定後、minibufferを出るための RET が必要。次テストにて。
  (skip-unless (eq tcode-use-isearch 'im))
  (should-not (tctest-cmp "[IMON] C-s か手 [POSTMAZE] RET [MATCH]"
    :initial "...歌手"
    :expect "<DING>...[歌手<!>]")))

(ert-deftest tctest-postfix-maze-in-isearch-non-im ()
  "isearch 中に後置交ぜ書き変換ができる(変換前にマッチしないケース)。非 im 用。"
  ;; 非 im 実装では確定後、minibufferを出るための RET が必要。
  (skip-unless (tctest-is-non-im))
  (should-not (tctest-cmp "[IMON] C-s か手 [POSTMAZE] RET RET [MATCH]"
    :initial "...歌手"
    :expect "<DING>...[歌手<!>]")))

(ert-deftest tctest-postfix-maze-in-isearch-match-before ()
  "isearch 中に後置交ぜ書き変換ができる(変換前に行き過ぎるケース)。"
  ;; 非 im 実装では確定後、minibufferを出るための RET が必要。次テストにて。
  (skip-unless (eq tcode-use-isearch 'im))
  (should-not (tctest-cmp "[IMON] C-s か手 [POSTMAZE] RET [MATCH]"
    :initial "...歌手 か手"
    :expect "...[歌手<!>] か手")))

(ert-deftest tctest-postfix-maze-in-isearch-match-before-non-im ()
  "isearch 中に後置交ぜ書き変換ができる(変換前に行き過ぎるケース)。非 im 用。"
  ;; 非 im 実装では確定後、minibufferを出るための RET が必要。
  (skip-unless (tctest-is-non-im))
  (should-not (tctest-cmp "[IMON] C-s か手 [POSTMAZE] RET RET [MATCH]"
    :initial "...歌手 か手"
    :expect "...[歌手<!>] か手")))

(ert-deftest tctest-delete-postfix-maze-in-isearch ()
  "isearch 中の後置交ぜ書き変換結果を1文字だけ削除できる。"
  ;; 非 im 実装では変換後も minibuffer にいるので問題にならない。
  (skip-unless (eq tcode-use-isearch 'im))
  (should-not (tctest-cmp "[IMON] C-s ななめ [POSTMAZE] RET DEL 陽 [MATCH]"
    :initial "...斜陽...斜め"
    :expect "<DING>...[斜陽<!>]...斜め")))

(ert-deftest tctest-delete-prefix-maze-in-isearch ()
  "isearch 中の前置交ぜ書き変換結果を1文字だけ削除できる。"
  ;; 非 im 実装では変換後も minibuffer にいるので問題にならない。
  (skip-unless (eq tcode-use-isearch 'im))
  (should-not (tctest-cmp "[IMON] C-s [PREMAZE] ななめ SPC DEL 陽 [MATCH]"
    :initial "...斜陽...斜め"
    :expect "...[斜陽<!>]...斜め")))

;;;
;;; *isearch での各種入力モード
;;;

(ert-deftest tctest-isearch-alnum2b ()
  "全角英数モードで isearch が使える。"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] [ALNUM_ZEN] C-s ABC [MATCH] [ALNUM_HAN]"
    :initial "...ABCＡＢＣ"
    :expect "...ABC[ＡＢＣ<!>]")))

(ert-deftest tctest-isearch-switch-alnum2b ()
  "全角英数モードを isearch 中に切り換えられる。"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] C-s AB [ALNUM_ZEN] CD [ALNUM_HAN] EF [MATCH]"
    :initial "...ABCDEF ABＣＤEF"
    :expect  "...ABCDEF [ABＣＤEF<!>]")))

(ert-deftest tctest-isearch-katakana ()
  "カタカナモードで isearch が使える。"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] # C-s あいう [MATCH] #"
    :initial "...あいうアイウ"
    :expect "...あいう[アイウ<!>]"
    :setup-fun #'tctest-bind-katakana)))

(ert-deftest tctest-switch-katakana ()
  "カタカナモードを isearch 中に切り換えられる。"
  ;; 非 im 実装では isearch 中に1文字コマンドの起動はできない。
  ;; 既知の問題: https://github.com/kanchoku/tc/issues/41
  :expected-result (if (eq tcode-use-isearch 'im) :passed :failed)
  (should-not (tctest-cmp "[IMON] C-s あい # うえ # おか [MATCH] #"
    :initial "...あいうえおか..あいウエおか"
    :expect  "...あいうえおか..[あいウエおか<!>]"
    :setup-fun #'tctest-bind-katakana)))

(ert-deftest tctest-isearch-kuten_J ()
  "isearch 中に 日本語句読点が入力できる。"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] C-s 。、[MATCH]"
    :initial "...。、. , "
    :expect "...[。、<!>]. , ")))

(ert-deftest tctest-isearch-kuten_A ()
  "isearch 中に ASCII 句読点が入力できる。"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] [KUTEN_A] C-s 。、[MATCH] [KUTEN_J]"
    :initial "...。、. , "
    :expect "...。、[. , <!>]")))

(ert-deftest tctest-isearch-switch-kuten ()
  "句読点の種類を isearch 中に切り換えられる。"
  ;; 非 im 実装では、句読点切り換えを実装していない
  ;; (tcode-isearch-special-function-alist に入っていない。)
  ;; 既知の問題: https://github.com/kanchoku/tc/issues/41
  :expected-result (if (eq tcode-use-isearch 'im) :passed :failed)
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] C-s 。、[KUTEN_A]。、[KUTEN_J]。、[MATCH]"
    :initial "...|. , . , . , |。、. , 。、|。、。、。、"
    :expect  "...|. , . , . , |[。、. , 。、<!>]|。、。、。、")))

(ert-deftest tctest-katakana-in-isearch-prefix-maze ()
  "isearch 中の前置交ぜ書き変換中にカタカナモードに変更できる。"
  ;; 非 im 実装では確定後、minibufferを出るための RET が必要。次テストにて。
  (skip-unless (eq tcode-use-isearch 'im))
  (should-not (tctest-cmp "[IMON] C-s あ [PREMAZE] # え # SPC い [MATCH]"
    :initial "...あえい..あヱい"
    :expect  "...あえい..[あヱい<!>]"
    :setup-fun #'tctest-bind-katakana)))

(ert-deftest tctest-katakana-in-isearch-prefix-maze-non-im ()
  "isearch 中の前置交ぜ書き変換中にカタカナモードに変更できる。非 im 用"
  ;; 非 im 実装では確定後、minibufferを出るための RET が必要。
  (skip-unless (tctest-is-non-im))
  (should-not (tctest-cmp "[IMON] C-s あ [PREMAZE] # え # SPC RET い [MATCH]"
    :initial "...あえい..あヱい"
    :expect  "...あえい..[あヱい<!>]"
    :setup-fun #'tctest-bind-katakana)))

(ert-deftest tctest-katakana-in-isearch-prefix-bushu ()
  "isearch 中の前置部首変換中にカタカナモードに変更できる。"
  ;; im でのみ実装。
  ;; 既知の問題: https://github.com/kanchoku/tc/issues/41
  :expected-result (if (eq tcode-use-isearch 'im) :passed :failed)
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] C-s あ [PREBUSHU] # い # い [MATCH]"
    :initial "...あ似"
    :expect "...[あ似<!>]"
    :setup-fun #'tctest-bind-katakana)))

(ert-deftest tctest-kuten-in-isearch-prefix-bushu ()
  "isearch 中の前置部首変換中に句点を変更できる。"
  ;; im でのみ実装。
  ;; 既知の問題: https://github.com/kanchoku/tc/issues/41
  :expected-result (if (eq tcode-use-isearch 'im) :passed :failed)
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp
	       "[IMON] C-s [KUTEN_A] 、 [PREBUSHU] 大 [KUTEN_J] 、 [MATCH]"
    :initial "..., 犬"
    :expect "...[, 犬<!>]")))

;;;
;;; *モード表示
;;;

(ert-deftest tctest-start-mode ()
  "テスト開始のモード表示は[]。" ; 初回 C-\ するまでは空。
  (should-not (tctest-cmp "[MODE]"
    :expect "[]<!>")))

(ert-deftest tctest-im-mode ()
  "[IMON][IMOFF] で [TC] 表示が切り換わる。"
  (should-not (tctest-cmp "[IMON] [MODE] 人 [IMOFF] [MODE] ma"
    :expect "[TC]人[--]ma<!>")))

(ert-deftest tctest-alnum-mode ()
  "[ALNUM_ZEN][ALNUM_HAN] で [Ｔ］表示が切り換わる。"
  (should-not (tctest-cmp
	       "[IMON] [MODE] A [ALNUM_ZEN] [MODE] A [ALNUM_HAN] [MODE] A"
    :expect "[TC]A[Ｔ]Ａ[TC]A<!>")))

(ert-deftest tctest-katakana-mode ()
  "カタカナモードの表示が切り換わる。"
  (should-not (tctest-cmp
	       "[IMON] [MODE] あ # [MODE] あ # [MODE] あ"
    :expect "[TCひ]あ[TCカ]ア[TCひ]あ<!>"
    :setup-fun #'tctest-bind-katakana)))


;;;
;;; *tcode-isearch-start-stateのモード表示
;;;
(ert-deftest tctest-isearch-start-nil-off-mode ()
  "t-i-s-s nil、IM off で isearch 開始時、モード表示 []"
  (should-not (tctest-cmp "[MODE] C-s [MODE] ma [MATCH]"
    :initial "...人.ma"
    :expect  "[][]...人.[ma<!>]"
    :vars '((tcode-isearch-start-state nil)))))

(ert-deftest tctest-isearch-start-nil-on-mode ()
  "t-i-s-s nil、IM on で isearch 開始時、モード表示 [TC]"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] [MODE] C-s [MODE] 人 [MATCH]"
    :initial "...人.ma"
    :expect  "[TC][TC]...[人<!>].ma"
    :vars '((tcode-isearch-start-state nil)))))

(ert-deftest tctest-isearch-start-0-off-mode ()
  "t-i-s-s 0、IM off で isearch 開始時、モード表示 []"
  (should-not (tctest-cmp "[MODE] C-s [MODE] ma [MATCH]"
    :initial "...人.ma"
    :expect  "[][]...人.[ma<!>]"
    :vars '((tcode-isearch-start-state 0)))))

(ert-deftest tctest-isearch-start-0-on-mode ()
  "t-i-s-s 0、IM on で isearch 開始時、モード表示 [--]"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] [MODE] C-s [MODE] ma [MATCH]"
    :initial "...人.ma"
    :expect  "[TC][--]...人.[ma<!>]"
    :vars '((tcode-isearch-start-state 0)))))

(ert-deftest tctest-isearch-start-1-off-mode ()
  "t-i-s-s 1、IM off で isearch 開始時、モード表示 [TC]"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[MODE] C-s [MODE] 人 [MATCH]"
    :initial "...人.ma"
    :expect  "[][TC]...[人<!>].ma"
    :vars '((tcode-isearch-start-state 1)))))

(ert-deftest tctest-isearch-start-1-on-mode ()
  "t-i-s-s 1、IM on で isearch 開始時、モード表示 [TC]"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] [MODE] C-s [MODE] 人 [MATCH]"
    :initial "...人.ma"
    :expect  "[TC][TC]...[人<!>].ma"
    :vars '((tcode-isearch-start-state 1)))))

;;;
;;; *line-fold search
;;;

(ert-deftest tctest-line-fold-search ()
  "isearch時、日本語文字間のスペース、改行は無視される。"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] C-s 長い段落の全体 [MATCH]"
    :initial "   長い\n   段落の   全体"
    :expect "[   長い\n   段落の   全体<!>]"
    :vars '((tcode-isearch-enable-line-fold-search t)))))

(ert-deftest tctest-no-line-fold-search ()
  "tcode-isearch-enable-line-fold-search が nil のとき、
日本語文字間のスペース、改行は無視されない。"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] C-s 長い段落の全体 [MATCH]"
    :initial "   長い\n   段落の   全体"
    :expect "   <DING>[長い<!>]\n   段落の   全体"
    :vars '((tcode-isearch-enable-line-fold-search nil)))))

(ert-deftest tctest-line-fold-search-ignore-regexp ()
  "tcode-isearch-ignore-regexp の設定で、スペース以外のものを無視できる。"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] C-s コメント中の長い段落 [MATCH]"
    :initial ";;   コメント中の\n;;   長い段落"
    :expect  "[;;   コメント中の\n;;   長い段落<!>]"
    :vars '((tcode-isearch-ignore-regexp "[\n \t;]*")))))

(ert-deftest tctest-line-fold-search-default-ignore-regexp ()
  "前テスト(ignore-regexp)の設定が常に有効でないことの確認。"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] C-s コメント中の長い段落"
    :initial ";;   コメント中の\n;;   長い段落"
    :expect  ";;<DING>   コメント中の<!>\n;;   長い段落")))

;;;
;;; *isearch 中の特殊な状況
;;;

(ert-deftest tctest-command-in-isearch ()
  "isearch 中の tcode-mode-map のコマンドは通常入力になる。
非 im 実装のみ。)"
  (skip-unless (tctest-is-non-im))
  (should-not (tctest-cmp "[IMON] C-s あ ! [MATCH]"
    :initial "...あい...あ!"
    :expect  "...あい...[あ!<!>]")))

(ert-deftest tctest-minibuf-error-in-isearch ()
  "isearch 中に input method 内でエラーがあっても復帰できる。"
  ;; im 実装の開発中に直した問題。「|」で辞書登録コマンド
  ;; (tcode-mazegaki-make-entry-and-finish) が起動し、minibuffer 使用
  ;; がinput method と競合するのでエラーが発生する。問題修正前は
  ;; minibuffer 内のエラーとなり、RET 待ちが発生して timeout となる。
  ;; 非 im 系の実装では isearch 中に1文字コマンドは起動しないので skip。
  (skip-unless tcode-use-input-method)
  (should-not (tctest-cmp "[IMON] C-s あ | い"
    :initial "...あいう"
    :expect  "...あ<DING>い<!>う")))

;;;
;;; *emacs 本体の isearch 機能
;;;

(ert-deftest tctest-char-fold-search ()
  "char-fold search が動作する。"
  (should-not (tctest-cmp "C-s M-s ' mobius SPC cafe"
    :initial "...möbius café"
    :expect  "...möbius café<!>")))

(ert-deftest tctest-word-search ()
  "word-search が動作する。"
  (should-not (tctest-cmp "M-s w ice SPC cream [MATCH]"
    :initial "...rice-cream ice-cream"
    :expect  "...rice-cream [ice-cream<!>]")))

(ert-deftest tctest-symbol-search ()
  "symbol-search が動作する。"
  (should-not (tctest-cmp "M-s _ let [MATCH]"
    :initial "...seq-let let"
    :expect  "...seq-let [let<!>]")))

(ert-deftest tctest-lax-whitespace ()
  "isearch時、英単語間の空白数の違いは無視される。"
  ;; overwrite 実装で動作しないことは、既知の問題。
  ;; https://github.com/kanchoku/tc/issues/25
  :expected-result (if (eq tcode-use-isearch 'overwrite) :failed :passed)
  (should-not (tctest-cmp "C-s a SPC b [MATCH]"
    :initial "...a     b"
    :expect "...[a     b<!>]"
    :vars '((isearch-lax-whitespace t)))))

(ert-deftest tctest-no-lax-whitespace ()
  "isearch-lax-whitespace が nil のとき、空白数の違いは無視されない。"
  (should-not (tctest-cmp "C-s a SPC b [MATCH]"
    :initial "...a     b"
    :expect  "...<DING>[a <!>]    b"
    :vars '((isearch-lax-whitespace nil)))))

(ert-deftest tctest-isearch-forward-regexp ()
  "C-M-s で正規表現サーチになる。(line-fold search に上書きされない。)"
  (should-not (tctest-cmp "C-M-s [a] [MATCH]"
    :initial "...a...[a]"
    :expect  "...[a<!>]...[a]" ; 分かりにくいが、文字セット[a] が a にマッチ。
    :vars '((tcode-isearch-enable-line-fold-search t))))) ; default

(ert-deftest tctest-isearch-forward-regexp-with-arg ()
  "C-u C-M-s で通常サーチとなり、line-fold search が有効になる。"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] C-u C-M-s 単語 [MATCH]"
    :initial "...英単\n   語...単語"
    :expect  "...英[単\n   語<!>]...単語"
    :vars '((tcode-isearch-enable-line-fold-search t))))) ; default

(ert-deftest tctest-isearch-backward-regexp ()
  "C-M-r で正規表現サーチになる。(line-fold search に上書きされない。)"
  (should-not (tctest-cmp "C-e C-M-r [a] [MATCH]"
    :initial "...[a]...a..."
    :expect  "...[a]...<!>[a]..."
    :vars '((tcode-isearch-enable-line-fold-search t))))) ; default

(ert-deftest tctest-isearch-backward-regexp-with-arg ()
  "C-u C-M-r で通常サーチとなり、line-fold search が有効になる。"
  ;; 既知の問題: overwrite 実装では、後ろ向きの line-fold search の挙
  ;; 動がおかしい。https://github.com/kanchoku/tc/issues/31
  :expected-result (if (eq tcode-use-isearch 'overwrite) :failed :passed)
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] M-> C-u C-M-r 単語 [MATCH]"
    :initial "...単語...英単\n   語..."
    :expect  "...単語...英<!>[単\n   語]..."
    :vars '((tcode-isearch-enable-line-fold-search t))))) ; default

(ert-deftest tctest-isearch-forward-with-arg ()
  "C-u C-s で正規表現サーチになる。(line-fold search に上書きされない。)"
  (should-not (tctest-cmp "C-u C-s [a] [MATCH]"
    :initial "...a...[a]"
    :expect  "...[a<!>]...[a]" ; 分かりにくいが、文字セット[a] が a にマッチ。
    :vars '((tcode-isearch-enable-line-fold-search t))))) ; default

(ert-deftest tctest-isearch-backward ()
  "C-r で、line-fold search が有効になる。"
  ;; 既知の問題: overwrite 実装では、後ろ向きの line-fold search の挙
  ;; 動がおかしい。https://github.com/kanchoku/tc/issues/31
  :expected-result (if (eq tcode-use-isearch 'overwrite) :failed :passed)
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] M-> C-r 単語 [MATCH]"
    :initial "...単語...英単\n   語..."
    :expect  "...単語...英<!>[単\n   語]..."
    :vars '((tcode-isearch-enable-line-fold-search t))))) ; default

(ert-deftest tctest-isearch-backward-with-arg ()
  "C-u C-r で正規表現サーチになる。(line-fold search に上書きされない。)"
  (should-not (tctest-cmp "C-e C-u C-r [a] [MATCH]"
    :initial "...[a]...a..."
    :expect  "...[a]...<!>[a]..."
    :vars '((tcode-isearch-enable-line-fold-search t))))) ; default

;; 通常の isearch-forward-word のテストは、tctest-word-search
(ert-deftest tctest-isearch-forward-word-with-arg ()
  "C-u M-s w で通常サーチとなり、line-fold search が有効になる。"
  (skip-unless tcode-use-isearch)
  (should-not (tctest-cmp "[IMON] C-u M-s w 単語 [MATCH]"
    :initial "...英単\n   語...単語"
    :expect  "...英[単\n   語<!>]...単語"
    :vars '((tcode-isearch-enable-line-fold-search t))))) ; default

(ert-deftest tctest-isearch-symbol-at-point ()
  "M-s . で symbol サーチになる。(line-fold search に上書きされない。)"
  (should-not (tctest-cmp "M-s . C-s [MATCH]"
    :initial "let seq-let let"
    :expect  "let seq-let [let<!>]"
    :vars '((tcode-isearch-enable-line-fold-search t))))) ; default

(ert-deftest tctest-isearch-thing-at-point ()
  "M-s M-. では line-fold search が有効になる。"
  (skip-unless (and (>= emacs-major-version 28) ; emacs-28 からの機能。
		    tcode-use-isearch))
  (should-not (tctest-cmp "M-s M-. C-s [MATCH]"
    :initial "長い単語 ...長い\n  単語"  ; (thing-at-point 'symbol)がヒット。
    :expect  "長い単語 ...[長い\n  単語<!>]"
    :vars '((tcode-isearch-enable-line-fold-search t))))) ; default

(ert-deftest tctest-word-search-forward ()
  "word-search-forward が単語サーチになる。(line-fold search に上書きされない。)"
  (should-not (tctest-cmp "M-x word-search-forward RET [IMON] 単語 RET [MATCH]"
    :initial "...単\n語 英単語帳 単語"
    :expect  "...単\n語 英単語帳 [単語<!>]"
    :vars '((tcode-isearch-enable-line-fold-search t))))) ; default

(ert-deftest tctest-word-search-backward ()
  "word-search-backward が単語サーチになる。"
  (should-not (tctest-cmp
	       "M-> M-x word-search-backward RET [IMON] 単語 RET [MATCH]"
    :initial "...単語 単\n語 英単語帳"
    :expect  "...<!>[単語] 単\n語 英単語帳"
    :vars '((tcode-isearch-enable-line-fold-search t))))) ; default

(ert-deftest tctest-word-search-forward-lax ()
  "word-search-forward-lax が単語サーチになる。"
  (should-not (tctest-cmp
	       "M-x word-search-forward-lax RET [IMON] 単語 RET [MATCH]"
    :initial "...単\n語 英単語帳 単語帳"
    :expect  "...単\n語 英単語帳 [単語<!>]帳"
    :vars '((tcode-isearch-enable-line-fold-search t))))) ; default

(ert-deftest tctest-word-search-backward-lax ()
  "word-search-backward-lax が単語サーチになる。"
  (should-not (tctest-cmp
	       "M-> M-x word-search-backward-lax RET [IMON] 単語 RET [MATCH]"
    :initial "...単語帳 単\n語 英単語帳"
    :expect  "...<!>[単語]帳 単\n語 英単語帳"
    :vars '((tcode-isearch-enable-line-fold-search t))))) ; default

;;;
;;; *その他の input method
;;;

(ert-deftest tctest-ja-alnum ()
  "ja-alnum input method で全角英数字が入力できる。"
  (should-not (tctest-cmp "[IMON] ABCdef123 [IMOFF]"
    :expect "ＡＢＣｄｅｆ１２３<!>"
    :requires '(tc-ja-alnum) :input-method "japanese-2byte-alnum")))

;;;
;;; *undo
;;;

(ert-deftest tctest-ja-alnum-undo-chars-and-dels ()
  "ja-alnum input method で削除と文字入力がまとめて undo されない。"
  (should-not (tctest-cmp "[IMON] June DEL DEL ly [UNDO]"
    :expect "Ｊｕ<!>"
    :requires '(tc-ja-alnum) :input-method "japanese-2byte-alnum")))

(ert-deftest tctest-undo-chars-and-dels ()
  "削除と文字入力がまとめて undo されない。"
  (should-not (tctest-cmp "[IMON] こんにちは DEL DEL DEL ばんは [UNDO]"
    :expect "こん<!>")))

(ert-deftest tctest-undo-chars-and-dels1 ()
  "削除と文字入力がまとめて undo されない。(1文字版)"
  (should-not (tctest-cmp "[IMON] あ DEL い [UNDO]"
    :expect "<!>")))

(ert-deftest tctest-undo-char-after-postfix-bushu ()
  "部首合成後の文字挿入の undo で、文字挿入だけ消える。"
  (should-not (tctest-cmp "[IMON] イ反 [POSTBUSHU] 想 [UNDO]"
    :expect "仮<!>")))

;;;
;;; *event-loop 関連
;;;

(ert-deftest tctest-kbd-macro-ends-with-postfix-bushu ()
  "キーボードマクロ実行の最後が部首合成であっても、すぐ終了する。"
  (should
   (eq 'success
       (with-temp-buffer
	 (switch-to-buffer (current-buffer))
	 (with-timeout (1 'timeout)
	   (let ((keys (tctest-key-filter "[IMON] イ反 [POSTBUSHU]")))
	     (advice-add 'sit-for :around #'tctest-sit-for)
             (execute-kbd-macro (kbd keys))
	     (advice-remove 'sit-for #'tctest-sit-for))
           'success)))))

(ert-deftest tctest-char-after-indent-rigidly ()
  "indent-rigidly を終了しつつ日本語入力できる。"
  (should-not (tctest-cmp "[IMON] C-SPC C-n C-n C-x C-i <right> あ SPC"
    :initial "abc\ndef\n"
    :expect " abc\n def\nあ <!>")))

(ert-deftest tctest-prefix-arg ()
  "prefix arg で日本語文字を繰り返せる。"
  (should-not (tctest-cmp "[IMON] C-u あ"
    :expect "ああああ<!>")))

(ert-deftest tctest-space-use-previous-prefix-arg ()
  "交ぜ書き変換後のスペースが前の文字の prefix-arg を引き継がない。"
  (should-not (tctest-cmp "[IMON] か手 [POSTMAZE] RET C-u あ SPC"
    :expect "歌手ああああ <!>")))

(ert-deftest tctest-prefix-arg-for-space ()
  "交ぜ書き変換後、スペースの個数を prefix-arg で指定できる。"
  (should-not (tctest-cmp "[IMON] か手 [POSTMAZE] RET C-u SPC"
    :expect "歌手    <!>")))

(defun tctest-pchitc-setup (&rest args)
  (tcode-set-key "@" 'tctest-pchitc-cmd))
(defun tctest-pchitc-cmd ()
  (insert "(cmd)")
  (add-hook 'post-command-hook 'tctest-pchitc-hook-fun))
(defun tctest-pchitc-hook-fun ()
  (remove-hook 'post-command-hook 'tctest-pchitc-hook-fun)
  (insert "(hook-fun)"))
(defun tctest-pchitc-cleanup (&rest args)
  (remove-hook 'post-command-hook 'tctest-pchitc-hook-fun)
  (tcode-set-key "@" nil))
(ert-deftest tctest-post-command-hook-in-tc-command ()
  "tc 内から呼んだコマンド直後に post-command-hook が作動する。"
  (should-not (tctest-cmp "[IMON] あい @ う"
    :expect "あい(cmd)(hook-fun)う<!>"
    :setup-fun #'tctest-pchitc-setup
    :cleanup-fun #'tctest-pchitc-cleanup)))

;;
;; *tcode-electric-space, tcode-electric-comma
;;

(defun tctest-setup-elec (params)
  (tctest-define-key params tctest-mode-map " " 'tcode-electric-space)
  (tctest-define-key params tctest-mode-map "," 'tcode-electric-comma))

;; (tc-manual.pdf p.22)
;;   非 T コードモードから T コードモードへ
;;     , : カーソルの直前の文字により動作が異なります。行頭または空白な
;;         らモード切り替え...
(ert-deftest tctest-elec-comma-at-bol ()
  "(electric)行頭の「,」で IM オン"
  (should-not (tctest-cmp ", [MODE]"
    :expect "[TC]<!>"
    :setup-fun #'tctest-setup-elec)))

(ert-deftest tctest-elec-comma-after-space ()
  "(electric)空白文字後の「,」で IM オン"
  (should-not (tctest-cmp "C-f , [MODE]"
    :initial " ."
    :expect " [TC]<!>."
    :setup-fun #'tctest-setup-elec)))

(ert-deftest tctest-elec-comma-after-tab ()
  "(electric)タブ文字後の「,」で IM オン"
  (should-not (tctest-cmp "C-f , [MODE]"
    :initial "\t."
    :expect "\t[TC]<!>."
    :setup-fun #'tctest-setup-elec)))

;; (tc-manual.pdf p.22)
;;   非 T コードモードから T コードモードへ
;;     , : ...、それ以外ならそのまま, を挿入します。
(ert-deftest tctest-elec-comma-after-char ()
  "(electric)通常文字後の「,」は、IM オフのまま"
  (should-not (tctest-cmp "C-f , [MODE]"
    :initial "a."
    :expect "a,[]<!>."
    :setup-fun #'tctest-setup-elec)))

;; (tc-manual.pdf p.22)
;;   非 T コードモードから T コードモードへ
;;     SPC , : スペースを挿入後、モードを切り替えます。
(ert-deftest tctest-elec-space-comma ()
  "(electric)「SPC ,」でスペース挿入後 IM オン。"
  (should-not (tctest-cmp "SPC , [MODE]"
    :initial "."
    :expect " [TC]<!>."
    :setup-fun #'tctest-setup-elec)))

;; (tc-manual.pdf p.23)
;;   非 T コードモードから T コードモードへ
;;     SPC TAB : スペースは、一旦挿入されますが、モードを切り替えた後
;;               取り除かれます。
(ert-deftest tctest-elec-space-tab-on ()
  "(electric)「SPC TAB」でスペース挿入無しで IM オン。"
  (should-not (tctest-cmp "SPC TAB [MODE]"
    :expect "[TC]<!>"
    :setup-fun #'tctest-setup-elec)))

(ert-deftest tctest-elec-space-char ()
  "(electric)SPC後「a」で IM オフのまま。"
  (should-not (tctest-cmp "SPC a [MODE]"
    :expect " a[]<!>"
    :setup-fun #'tctest-setup-elec)))

;; (tc-manual.pdf p.23)
;;   T コードモードから非 T コードモードへ
;;     T コードによる入力の直後 (前置型の交ぜ書き変換中のときを除く)
;;     では、モード切り替えを行い、かつスペースを挿入します。
(ert-deftest tctest-elec-kanji-space ()
  "(electric)「漢 SPC」でスペース挿入かつ IM オフ。"
  (should-not (tctest-cmp "[IMON] 漢 SPC [MODE]"
    :expect "漢 [--]<!>"
    :setup-fun #'tctest-setup-elec)))

;; (tc-manual.pdf p.23)
;;   T コードモードから非 T コードモードへ
;;     ...そうでない場合は、そのままスペースを挿入します。
(ert-deftest tctest-elec-non-kanji-space ()
  "(electric)IM オン時、C-f SPC 漢 で、空白と「漢」挿入かつ IM オンのまま。"
  (should-not (tctest-cmp "[IMON] C-f SPC 漢 [MODE]"
    :initial "あい"
    :expect "あ 漢[TC]<!>い"
    :setup-fun #'tctest-setup-elec)))

;; (tc-manual.pdf p.23)
;;   T コードモードから非 T コードモードへ
;;     SPC TAB : スペースは、一旦挿入されますが、モードを切り替えた後
;;               取り除かれます。
(ert-deftest tctest-elec-space-tab-off-after-non-char ()
  "(electric)IM オン時、C-f 後に「SPC TAB」でスペース挿入無しで IM オフ。"
  (should-not (tctest-cmp "[IMON] C-f SPC TAB [MODE]"
    :initial "あい"
    :expect "あ[--]<!>い"
    :setup-fun #'tctest-setup-elec)))

(ert-deftest tctest-elec-space-tab-off-after-char ()
  "(electric)IM オン、文字入力後、「SPC TAB」でスペース挿入無しで IM オフ。"
  ;; マニュアルでは直前が文字入力かどうかに関係なくオフになるように読
  ;; めるが、ソースコードでは、そのような実装にはなっていない。マニュ
  ;; アルとソースコードのどちらが作者の意図かはわからない。
  ;; https://github.com/kanchoku/tc/issues/40
  ;; の問題(2)で言及した。既知の問題とする。
  :expected-result :failed
  (should-not (tctest-cmp "[IMON] あ SPC TAB [MODE]"
    :expect "あ[--]<!>"
    :setup-fun #'tctest-setup-elec)))

;; (tc-manual.pdf p.23)
;;   たとえば、T コードモードの時に、‘たとえば、Emacs では’ と入力しよ
;;   うとしたとします。この場合は、‘たとえば、’ の後 SPC を入力し、続
;;   けて ‘Emacs ’ を入力します。次に、, の後 ‘では’ と入力すればよい
;;   のです。ここで、句読点の直後では、SPC による切り替えでも空白は挿
;;   入されません。
(ert-deftest tctest-elec-example ()
  "(electric)「たとえば、 Emacs ,では」で SPC と「,」が消える。"
  (should-not (tctest-cmp "[IMON] たとえば、 SPC Emacs SPC ,では"
    :expect "たとえば、Emacs では<!>"
    :setup-fun #'tctest-setup-elec)))

(ert-deftest tctest-elec-kanji-space-without-inserting ()
  "(electric)「漢 SPC」でIM オフ。without-inserting t なら空白挿入なし。"
  (should-not (tctest-cmp "[IMON] 漢 SPC [MODE]"
    :expect "漢[--]<!>"
    :vars '((tcode-electric-space-without-inserting t))
    :setup-fun #'tctest-setup-elec)))

(ert-deftest tctest-elec-space-comma-without-inserting ()
  "(electric)「SPC ,」でIM オン。without-inserting t なら空白挿入なし。"
  (should-not (tctest-cmp "SPC , [MODE]"
    :initial "."
    :expect "[TC]<!>."
    :vars '((tcode-electric-space-without-inserting t))
    :setup-fun #'tctest-setup-elec)))

(ert-deftest tctest-elec-space-read-only ()
  "(electric)read-onlyバッファで「SPC」でIMオン/オフ切り替えできる。"
  (should-not (tctest-cmp
	       "C-x C-q SPC C-x C-q [MODE] C-x C-q SPC C-x C-q [MODE]"
    :initial ""
    :expect "[TC][--]<!>"
    :setup-fun #'tctest-setup-elec)))


(provide 'tctest)
