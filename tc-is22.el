;;; tc-is22.el --- T-Code isearch modification for Emacs 22.*.

;; Copyright (C) 1994,97-2001, 2005 Kaoru Maeda, Mikihiko Nakao, KITAJIMA Akira and Masayuki Ataka

;; Author: Kaoru Maeda <maeda@src.ricoh.co.jp>
;;      Mikihiko Nakao
;;      KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;;      Masayuki Ataka <masayuki.ataka@gmail.com>
;; Maintainer: Masayuki Ataka
;; Create: 12 Feb (Sat), 2005

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

(require 'tc-iscommon)

(if (< (string-to-number emacs-version) 22)
    (error "tc-is22 cannot run on NEmacs/Mule/Emacs20/21.  Use Emacs 22 or later!"))

;;;
;;; patch to original functions in isearch.el of Emacs 22
;;;

;; isearch-word -> isearch-regexp-function from 25.1
(defsubst tcode-isearch-regexp-function ()
  (or (bound-and-true-p isearch-regexp-function)
      (bound-and-true-p isearch-word)))

(defadvice isearch-search-string (around tcode-handling activate)
  (let ((isearch-regexp (if (or (tcode-isearch-regexp-function) isearch-regexp)
                            isearch-regexp
                          tcode-isearch-enable-wrapped-search)))
    ad-do-it))

(defun isearch-printing-char ()
  "Add this ordinary printing character to the search string and search."
  (interactive)
  (let ((char (isearch-last-command-char)))
    (if (and (boundp 'tcode-mode) tcode-mode)
	;; isearch for T-Code
	(let* ((decoded (tcode-decode-chars (isearch-last-command-char)))
	       (action (car decoded))
	       (prev (tcode-isearch-bushu)))
	  (cond ((null action)
		 (ding))
		((stringp action)
		 (setq action
		       (mapconcat 'char-to-string
				  (tcode-apply-filters
				   (string-to-list action))
				  nil))
		 (tcode-isearch-process-string action prev))
		((char-or-string-p action)
		 (tcode-isearch-process-string
		  (char-to-string (car (tcode-apply-filters (list action))))
		  prev))
		((and (not (tcode-function-p action))
		      (consp action))
		 (tcode-isearch-process-string
		  (mapconcat 'char-to-string
			     (tcode-apply-filters
			      (mapcar 'string-to-char
				      (delq nil action)))
			     nil)
		  prev))
		((tcode-function-p action)
		 (let ((func (assq action
				   tcode-isearch-special-function-alist)))
		   (if func
		       (funcall (or (cdr func)
				    action))
		     (tcode-isearch-process-string
		      (mapconcat 'char-to-string (cdr decoded) nil)
		      prev))))
		(t
		 (ding))))
      ;; original behaviour
      (if (= char ?\S-\ )
	  (setq char ?\ ))
      (if (and enable-multibyte-characters
	       (>= char ?\200)
	       (<= char ?\377))
	  (if (keyboard-coding-system)
	      (isearch-process-search-multibyte-characters char)
	    (isearch-process-search-char (unibyte-char-to-multibyte char)))
	(if current-input-method
	    (isearch-process-search-multibyte-characters char)
	  (isearch-process-search-char char))))))

(defadvice isearch-process-search-char (around tcode-handling activate)
  "Extention for T-code"
  (if (and (not isearch-regexp)
	   (boundp 'tcode-isearch-enable-wrapped-search)
	   tcode-isearch-enable-wrapped-search
	   (memq char '(?$ ?* ?+ ?. ?? ?\[ ?\\ ?\] ?^)))
      (let ((s (char-to-string char)))
	(isearch-process-search-string (concat "\\" s) s))
    ad-do-it))

(defun isearch-yank-word ()
  "Pull next word from buffer into search string."
  (interactive)
  (isearch-yank-internal (lambda ()
			   (if (= (char-width (char-after)) 2)
			       (forward-char 1)
			     (forward-word 1))
			   (point))))

(defun isearch-yank-string (string)
  "Pull STRING into search string."
  ;; Downcase the string if not supposed to case-fold yanked strings.
  (if (and isearch-case-fold-search
	   (eq 'not-yanks search-upper-case))
      (setq string (downcase string)))
  (if isearch-regexp (setq string (regexp-quote string)))
  (setq isearch-string (concat isearch-string
			       (tcode-isearch-make-string-for-wrapping string))
	isearch-message
	(concat isearch-message
		(mapconcat 'isearch-text-char-description string ""))
	;; Don't move cursor in reverse search.
	isearch-yank-flag t)
  (isearch-search-and-update))

(defun isearch-repeat (direction)
  ;; Utility for isearch-repeat-forward and -backward.
  (if (eq isearch-forward (eq direction 'forward))
      ;; C-s in forward or C-r in reverse.
      (if (equal isearch-string "")
	  ;; If search string is empty, use last one.
	  (setq isearch-string
		(or (if isearch-regexp
			(car regexp-search-ring)
		      (car search-ring))
		    (error "No previous search string"))
		isearch-message
		(mapconcat 'isearch-text-char-description
			   (tcode-isearch-remove-ignore-regexp isearch-string)
			   "")
		isearch-case-fold-search isearch-last-case-fold-search)
	;; If already have what to search for, repeat it.
	(or isearch-success
	    (progn
	      (if isearch-wrap-function
		  (funcall isearch-wrap-function)
		(goto-char (if isearch-forward (point-min) (point-max))))
	      (setq isearch-wrapped t))))
    ;; C-s in reverse or C-r in forward, change direction.
    (setq isearch-forward (not isearch-forward)))

  (setq isearch-barrier (point))	; For subsequent \| if regexp.

  (if (equal isearch-string "")
      (setq isearch-success t)
    (if (and isearch-success
	     (equal (point) isearch-other-end)
	     (not isearch-just-started))
	;; If repeating a search that found
	;; an empty string, ensure we advance.
	(if (if isearch-forward (eobp) (bobp))
	    ;; If there's nowhere to advance to, fail (and wrap next time).
	    (progn
	      (setq isearch-success nil)
	      (ding))
	  (forward-char (if isearch-forward 1 -1))
	  (isearch-search))
      (isearch-search)))

  (isearch-push-state)
  (isearch-update))

(defun tcode-isearch-read-string ()
  "インクリメンタルサーチ中に文字列を読み込む。"
  (let* (overriding-terminal-local-map
	 (minibuffer-setup-hook (lambda ()
				  (tcode-activate tcode-mode)))
	 (string (read-string (concat "Isearch read: " isearch-message)
			      nil nil nil t)))
    (unless (string= string "")
      (tcode-isearch-process-string string nil))))

(defun tcode-regexp-unquote (str)
  (let* ((ll (string-to-list str))
	 (l ll))
    (while l
      (if (eq (car l) ?\\)
	  (progn
	    (setcar l (car (cdr l)))
	    (setcdr l (cdr (cdr l)))))
      (setq l (cdr l)))
    (mapconcat (function char-to-string) ll nil)))

(defun tcode-isearch-remove-ignore-regexp (str)
  "変数 `tcode-isearch-enable-wrapped-search' が nil でないとき、
STR から `tcode-isearch-ignore-regexp' を取り除く。"
  (if (or (not tcode-isearch-enable-wrapped-search)
	  isearch-regexp)
      str
    (let (idx
	  (regexp-len (length tcode-isearch-ignore-regexp)))
      (while (setq idx (string-match
			(regexp-quote tcode-isearch-ignore-regexp)
			str))
	(setq str (concat (substring str 0 idx)
			  (substring str (+ idx regexp-len) nil))))
      (tcode-regexp-unquote str))))

(defun tcode-isearch-make-string-for-wrapping (string)
  (let ((string-list (and string
			  (string-to-list string))))
    (if (and tcode-isearch-enable-wrapped-search
	     (not isearch-regexp)
	     string-list)
	(mapconcat
	 (lambda (a)
	   (let ((s (char-to-string a)))
	     (cond ((and (string-match tcode-isearch-ignore-regexp s)
			 (> (match-end 0) 0))
		    tcode-isearch-ignore-regexp)
		   ((= (char-width a) 2)
		    (concat tcode-isearch-ignore-regexp s))
		   (t
		    (regexp-quote (char-to-string a))))))
	 string-list
	 nil)
      string)))

(add-hook 'isearch-mode-hook 'tcode-isearch-init)

(provide 'tc-is22)

;;; tc-is22.el ends here
