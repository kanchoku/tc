;;; tc-setup.el --- setup and/or loading routines for T-Code

;; Copyright (C) 1997-2002 KITAJIMA Akira.

;; Author: KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;; Created: 26 Apr 1997
;; Version: $Id: tc-setup.el,v 1.28 2003/03/25 08:32:20 kitajima Exp $

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

(defun tcode--load-isearch ()
  "`tcode-use-isearch' の設定に応じて必要なファイルをロードする。"
  (tcode--ensure-valid-value 'tcode-use-isearch '(overwrite advice nil)
			     'overwrite)
  (cond ((eq tcode-use-isearch 'overwrite)
         (require 'tc-sysdep)
         (require tcode-isearch-overwrite-module))
        (tcode-use-isearch
         (require 'tc-ishelper))))

(defun tcode--ensure-valid-value (var vals fallback)
  "VAR の値が VALS に無ければ、警告を出した上で値を FALLBACK に変える。"
  (let ((val (symbol-value var)))
    (unless (member val vals)
      (lwarn 'tcode :warning
             "Invalid value `%s' for `%s'; falling back to `%s'."
             val var fallback)
      (set var fallback))))

(if (featurep 'tc-setup)
    ()
  ;; load configuration file
  (and tcode-init-file-name
       (file-exists-p tcode-init-file-name)
       (progn
	 (require 'tc-sysdep)
	 ;; .tc を用意しているユーザーへの default 値。
	 ;; nil に設定したい場合は、.tc でセットする必要がある。
	 (setq tcode-use-isearch (or tcode-use-isearch 'overwrite))
	 (setq tcode-use-as-default-input-method t)
	 (load tcode-init-file-name)))
  ;; map toggle-input-method to `C-\'
  (if (or (memq tcode-emacs-version '(mule-1 mule-2))
	  (and (eq tcode-emacs-version 'xemacs)
	       (< emacs-major-version 21)
	       (< emacs-minor-version 3)))
      (autoload 'toggle-input-method "tc-sysdep" nil t))
  (if (not (fboundp 'register-input-method))
      (load "tc-sysdep"))
  (register-input-method "japanese-TUT-Code"
			 "Japanese"
			 'tcode-use-package
			 "TUT"
			 "a kanji direct input method")
  (register-input-method "japanese-Try-Code"
			 "Japanese"
			 'tcode-use-package
			 "TC"
			 "a kanji direct input method")
  (register-input-method "japanese-TT-Code"
			 "Japanese"
			 'tcode-use-package
			 "TT"
			 "a kanji direct input method")
  (register-input-method "japanese-T-Code"
			 "Japanese"
			 'tcode-use-package
			 "TC"
			 "a kanji direct input method")
  (if (and (<= (length (where-is-internal 'toggle-input-method)) 1)
	   (null (lookup-key global-map "\C-\\")))
      (global-set-key "\C-\\" 'toggle-input-method))
  (if tcode-use-as-default-input-method
      (progn
	(and (fboundp 'set-language-info)
	     (set-language-info "Japanese" 
				'input-method tcode-default-input-method))
	(setq-default default-input-method tcode-default-input-method)
	(setq default-input-method tcode-default-input-method)))
  ;; isearch
  (when tcode-use-as-default-input-method
    (tcode--load-isearch))
  ;; autoload
  (autoload 'tcode-use-package "tc")
  (autoload 'tcode-activate "tc")
  ;; functions in tc-jiscode.el
  (autoload 'tcode-start-jiscode "tc-jiscode" nil t)
  ;; functions in tc-help.el
  (autoload 'tcode-display-stroke-sequence "tc-help")
  (autoload 'tcode-display-stroke-for-char "tc-help" nil t)
  (autoload 'tcode-query-stroke "tc-help" nil t)
  (autoload 'tcode-show-tables "tc-help" nil t)
  (autoload 'tcode-mode-help "tc-help" nil t)
  (autoload 'tcode-display-direct-stroke "tc-help")
  ;; functions in tc-bushu.el
  (autoload 'tcode-bushu-begin-conversion "tc-bushu" nil t)
  (autoload 'tcode-bushu-convert-interactively "tc-bushu" nil t)
  (autoload 'tcode-bushu-convert-preceding-char-interactively "tc-bushu" nil t)
  (autoload 'tcode-bushu-begin-alternate-conversion "tc-bushu" nil t)
  (autoload 'tcode-bushu-compose-two-chars "tc-bushu")
  ;; functions in tc-mazegaki.el
  (autoload 'tcode-mazegaki-switch-to-dictionary "tc-mazegaki" nil t)
  (autoload 'tcode-mazegaki-begin-conversion "tc-mazegaki" nil t)
  (autoload 'tcode-mazegaki-make-entry "tc-mazegaki" nil t)
  (autoload 'tcode-mazegaki-make-entry-and-finish "tc-mazegaki" nil t)
  (autoload 'tcode-mazegaki-delete-entry "tc-mazegaki" nil t)
  (autoload 'tcode-mazegaki-make-entries-region "tc-mazegaki" nil t)
  (autoload 'tcode-mazegaki-make-entries-buffer "tc-mazegaki" nil t)
  (autoload 'tcode-mazegaki-delete-entries-region "tc-mazegaki" nil t)
  (autoload 'tcode-mazegaki-delete-entries-buffer "tc-mazegaki" nil t)
  (autoload 'tcode-mazegaki-delete-entry-by-last-yomi "tc-mazegaki" nil t)
  (autoload 'tcode-mazegaki-complete "tc-mazegaki" nil t)
  (autoload 'tcode-mazegaki-complete-and-convert "tc-mazegaki" nil t)
  (autoload 'tcode-mazegaki-put-prefix "tc-mazegaki" nil t)
  (autoload 'tcode-mazegaki-convert "tc-mazegaki" nil t)
  (autoload 'tcode-mazegaki-begin-alternate-conversion "tc-mazegaki" nil t)
  (autoload 'tcode-mazegaki-add-prefix "tc-mazegaki")
  (autoload 'tcode-mazegaki-lookup-with-prefix "tc-mazegaki")
  ;; functions in tc-inst.el
  (autoload 'tcode-install "tc-inst" nil t)
  ;; functions in tc-mkmzdic.el
  (autoload 'tcode-make-mazegaki-dictionary "tc-mkmzdic" nil t)
  ;; functions in tc-util.el
  (autoload 'tcode-inactivate-and-self-insert "tc-util" nil t)
  (autoload 'tcode-insert-register "tc-util" nil t)
  (autoload 'tcode-transpose-strokes "tc-util" nil t)
  (autoload 'set-tcode-mode-key "tc-util" nil t)
  (and window-system
       (autoload 'tcode-enable-cursor-to-change-color "tc-util" nil t))
  (autoload 'tcode-mazegaki-delete-kanji-from-dictionary "tc-util" nil t)
  (autoload 'tcode-mazegaki-get-yomi-max "tc-util" nil t)
  (autoload 'tcode-electric-space "tc-util" nil t)
  (autoload 'tcode-electric-comma "tc-util" nil t)
  (autoload 'tcode-insert-ya-outset "tc-util" nil t)
  (autoload 'tcode-transpose-strokes-or-chars "tc-util" nil t)
  (autoload 'tcode-mazegaki-show-yomi-region "tc-util" nil t)
  (autoload 'tcode-katakana-previous-char "tc-util" nil t)
  (autoload 'tcode-insert-kanji-by-kuten-code "tc-util" nil t)
  (autoload 'tcode-insert-kanji-by-jis-code "tc-util" nil t)
  (autoload 'tcode-katakana-preceding-chars "tc-util" nil t)
  (autoload 'tcode-kkc-region "tc-util" nil t)
  (autoload 'tcode-zap-to-char "tc-util" nil t)
  ;; statistics
  (autoload 'tcode-initialize-input-statistics "tc-stat")
  ;; EELLL
  (autoload 'eelll "eelll" nil t)
  (autoload 'eelll-random "eelll" nil t)
  (autoload 'eelll-region "eelll" nil t)
  (and (featurep 'frame)
       (autoload 'eelll-other-frame "eelll" nil t)))

(provide 'tc-setup)

(if tcode-load-immediate
    (progn
      ;; load all modules for T-Code
      (require 'tc-bushu)   ; tc.el will be loaded also.
      (require 'tc-help)
      (require 'tc-jiscode)
      (require 'tc-util)
      (require 'tc-mazegaki)))

;;; tc-setup.el ends here
