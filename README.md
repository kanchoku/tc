tc:  Japanese direct input method environment for emacs
========================================================

[![CI](https://github.com/kanchoku/tc/actions/workflows/ci.yml/badge.svg)](https://github.com/kanchoku/tc/actions/workflows/ci.yml)


This repository provides tc(T-code input environment for emacs).  
tc enables you to input Japanese characters with T-code or TUT-code.  

T-code is a Japanese input method that does NOT use Kana-to-Kanji conversion.  
You can input Kanji characters directly in the same way of inputting Hiragana.  
TUT-code is one of the T-code alternatives.

## Requirement

Emacs 26以降をサポートします。

## install方法

まず
https://melpa.org/#/getting-started
を参考にMELPAのpackageを `package-install` できるようにしてください。
その後

	M-x package-install
	tc

を実行します。package-installが終わったら

	M-x tcode-install

を実行してください。
tcの設定file作成場所についてなどが問われますがすべて
defaultで問題ありません。

最後にinit.elに

`(require 'tc-setup)`

の1行を追加し、emacsをrestartしてください。
restart後は
`M-x toggle-input-method`
を実行し、用いるinput-methodにT-codeを選択してください。

### Ubuntu Linuxでの注意事項

Ubuntu Linuxでuim-tcodeをご利用の方は /etc/emacs/site-start.d/50t-code.el の内容をすべてコメントアウトしてください。


## tcの利用方法

https://www.dropbox.com/s/qushezrz948u8ks/manual.pdf?dl=0 のpdfをご覧ください

## Windowsでのfont設定

Windows用Emacs に下記の設定font設定をおすすめします。  
ストローク表をずれ無く見る上でfont設定は重要となります。  
Windowsは日本語環境にしておいた方が無難です。  
英語環境ですと日本語フォントセットを見つけることができない可能性があります。

``` emacs-lisp
(require 'tc-setup)
(add-to-list 'default-frame-alist '(font . "-outline-ＭＳ ゴシック-normal-normal-normal-mono-19-*-*-*-c-*-iso8859-1"))
```
