#!/usr/bin/env bash

# 複数バージョンの emacs と、複数の初期設定オプションの組み合わせで、
# tctest のバッチテストを実行する。
#
# 実行方法:
#   1. tc-setup.el のあるディレクトリに cd する。(カレントディレクトリ
#      が load-path に加えられる。)
#   2. このスクリプトを、パス指定で実行する。通常は test/run.bash でよ
#      い。テスト開発時など、tctest.el が別の場所にある場合は、そのディ
#      レクトリにある run.bash を実行する。(run.bash のあるディレクト
#      リが load-path に加えられる。)
#   3. テスト対象の各バージョンの emacs のコマンド名を、「,」で区切っ
#      て並べたものを、第1引数に指定する。
#   4. 初期設定方法のキーワード(tctest-env.el 参照)を、「,」で区切って
#      並べたものを、第2引数に指定する。
#   5. 残りの引数は、そのまま emacs に渡される。
#
# 実行例:
#  $ cd ~/tc
#  $ test/run.bash emacs-30 isadvice --batch
#     動作: 以下と同じ。つまり、(setq tcode-use-isearch 'advice) が設
#           定され、ert のバッチテストが実行される。
#     $ TCTEST_ENV=isadvice emacs-30 -L ~/tc -L ~/tc/test -l tctest.env --batch
#
#  $ test/run.bash emacs-30,emacs-26 isoverwrite,isadvice,isim --batch
#     動作: 以下を順に実行するのと同じ。
#      $ test/run.bash emacs-30 isoverwrite --batch
#      $ test/run.bash emacs-30 isadvice    --batch
#      $ test/run.bash emacs-30 isim        --batch
#      $ test/run.bash emacs-26 isoverwrite --batch
#      $ test/run.bash emacs-26 isadvice    --batch
#      $ test/run.bash emacs-26 isim        --batch
#
# Windows での注意点:
#  - バッチモードで動作する Windows 用 emacs は、ターミナルへ直接日本
#    語文字を出力すると、終了コードが1になってしまう。原因不明。
#    workaroundとして、出力先をファイル、または、パイプにすればよい。
#     例:
#       $ test/run.bash emacs-30 isadvice --batch 2>&1 | tee /tmp/out

test_dir=$(dirname "$0")
cmds=$1; shift
keywords=$1; shift

fixed_args="-L . -L "$test_dir" -l tctest-env"

total=0
for cmd in ${cmds//,/ }; do   # split by comma
    for keyword in ${keywords//,/ }; do
	echo ========================================
	echo Running TCTEST_ENV=$keyword $cmd $fixed_args "$@"
	TCTEST_ENV=$keyword $cmd $fixed_args "$@"
	status=$?
	echo ------ exit status: $status -----
	total=$(( total + status ))
    done
done
echo exit status total: $total
test "$total" -eq 0
