;;; tc-stat.el --- input statistics in T-Code

;; Copyright (C) 2002 KITAJIMA Akira.

;; Author: KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;; Maintainer: KITAJIMA Akira
;; Keyword: input method, statistics
;; Create: Nov. 22, 2002

;; $Id: tc-stat.el,v 2.5 2003/03/25 08:42:10 kitajima Exp $

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

(defvar tcode-all-chars nil)

(defvar tcode-count-input-statistics t)
(defvar tcode-input-statistics-bound-value 10000)
(defvar tcode-input-statistics-help-penalty 5)

(defvar tcode-input-statistics-file-name nil)
(defvar tcode-input-statistics-list nil)

(defun tcode-count-char (char)
  (tcode-count-in-property char 'input-count)
  char)

(defun tcode-count-in-property (char property &optional no-recursive)
  (let ((symbol (intern-soft (char-to-string char) tcode-stroke-table)))
    (if symbol
	(let ((count (get symbol property)))
	  (put symbol property (if count
				   (if (<= tcode-input-statistics-bound-value
					   count)
				       count
				     (1+ count))
				     1)))
      (if (not no-recursive)
	  (tcode-count-in-property (tcode-2-to-1 char) property t)))))

(defun tcode-cancel-input-count (char &optional no-recursive)
  (let ((symbol (intern-soft (char-to-string char) tcode-stroke-table)))
    (if symbol
	(let ((count (get symbol 'input-count)))
	  (put symbol 'input-count (if count
				       (if (<= count 0)
					   count
					 (1- count))
				     0)))
      (if (not no-recursive)
	  (tcode-cancel-input-count (tcode-2-to-1 char) t)))))

(defun tcode-learning-score (input-count help-count)
  (- input-count (* tcode-input-statistics-help-penalty help-count)))

(defun tcode-put-input-statistics-list (count-list)
  (while count-list
    (let* ((entry (car count-list))
	   (symbol (intern (car entry) tcode-stroke-table)))
      (put symbol 'input-count (car (cdr entry)))
      (put symbol 'help-count (cdr (cdr entry))))
    (setq count-list (cdr count-list))))

(defun tcode-insert-all-chars (table)
  (cond ((or (null table)
	     (tcode-function-p table)
	     (eq table t))
	 nil)
	((char-or-string-p table)
	 (if (stringp table)
	     (if (= (string-width table) 2)
		 (insert table))
	   (insert (char-to-string table))))
	((consp table)
	 (mapcar (lambda (ent)
		   (tcode-insert-all-chars (cdr ent)))
		 table))
	((vectorp table)
	 (mapcar (lambda (ent)
		   (tcode-insert-all-chars ent))
		 table))
	((and (symbolp table)
	      (boundp table)) 
	 (tcode-insert-all-chars (eval table)))))

(defun tcode-get-all-chars (table)
  (save-excursion
    (let ((buffer (get-buffer-create " *tcode: all chars*")))
      (set-buffer buffer)
      (erase-buffer)
      (tcode-insert-all-chars table)
      (buffer-string))))

(defun tcode-get-input-statistics-list ()
  (unless tcode-all-chars
    (setq tcode-all-chars (tcode-get-all-chars tcode-table)))
  (let ((chars (string-to-list tcode-all-chars))
	count-list
	last-entry)
    (while chars
      (let* ((ch (char-to-string (car chars)))
	     (icount (get (intern-soft ch tcode-stroke-table) 'input-count))
	     (hcount (get (intern-soft ch tcode-stroke-table) 'help-count)))
	(if (or icount
		hcount)
	    (setq count-list (cons (cons ch (cons (or icount 0)
						  (or hcount 0)))
				   count-list))))
      (setq chars (cdr chars)))
    (setq count-list (sort count-list 
			   (lambda (x y)
			     (let ((s1 (tcode-learning-score (car (cdr x))
							     (cdr (cdr x))))
				   (s2 (tcode-learning-score (car (cdr y))
							     (cdr (cdr y)))))
			       (cond ((> s1 s2)
				      t)
				     ((< s1 s2)
				      nil)
				     (t
				      (string< (car x) (car y))))))))
    count-list))

(defun tcode-list-input-statistics-display ()
  "List statistics of all inputted characters with T-Code."
  (interactive)
  (let* ((bufname " *tcode: statistics*")
	 (buffer (get-buffer-create bufname)))
    (set-buffer buffer)
    (erase-buffer)
    (let ((count-list (tcode-get-input-statistics-list))
	  (total-chars 0)
	  (total-inputs 0)
	  (total-helps 0)
	  last-entry)
      (while count-list
	(let ((entry (car count-list)))
	  (unless (equal last-entry entry)
	    (setq total-chars (1+ total-chars)
		  total-inputs (+ total-inputs (car (cdr entry)))
		  total-helps (+ total-helps (cdr (cdr entry))))
	    (insert (format "%2s %8d (%d %d)\n" 
			    (car entry)
			    (tcode-learning-score (car (cdr entry))
						  (cdr (cdr entry)))
			    (car (cdr entry))
			    (cdr (cdr entry))))
	    (setq last-entry entry)))
	(setq count-list (cdr count-list)))
      (insert (format "\n%d文字 (%d %d)\n"
		      total-chars total-inputs total-helps)))
    (tcode-display-help-buffer buffer)))

(defun tcode-list-unused-chars-display (&optional helped)
  "List unused characters with T-Code.
When optional HELPED is non-nil, list uninputted characters also."
  (interactive "P")
  (unless tcode-all-chars
    (setq tcode-all-chars (tcode-get-all-chars tcode-table)))
  (let* ((bufname " *tcode: statistics*")
	 (buffer (get-buffer-create bufname)))
    (set-buffer buffer)
    (erase-buffer)
    (let ((all-chars (sort (string-to-list tcode-all-chars) '<))
	  (count-list (tcode-get-input-statistics-list)))
      (while count-list
	(let ((char (tcode-string-to-char (car (car count-list)))))
	  (if (and (> (car (cdr (car count-list))) 0)
		   (null helped))
	      (setq all-chars (delq char all-chars)))
	  (setq count-list (cdr count-list))))
      (let ((count 0))
	(while all-chars
	  (let ((char (car all-chars)))
	    (when (tcode-encode char)
	      (insert (char-to-string char))
	      (if (= (mod count 10) 4)
		  (insert " "))
	      (if (= (mod count 10) 9)
		  (insert "\n"))
	      (if (= (mod count 100) 99)
		  (insert "\n"))
	      (setq count (1+ count)))
	    (setq all-chars (cdr all-chars))))
	(insert (format "\n\n計%d文字" count)))
      (tcode-display-help-buffer buffer))))

;;;###autoload
(defun tcode-initialize-input-statistics ()
  (setq tcode-all-chars nil)
  (when (and tcode-input-statistics-file-name
	     (load (tcode-path-for-write tcode-input-statistics-file-name) t)
	     tcode-input-statistics-list)
    (tcode-put-input-statistics-list tcode-input-statistics-list)
    (setq tcode-input-statistics-list nil)))

(defun tcode-save-input-statistics ()
  (interactive)
  (when (and tcode-data-directory
	     tcode-input-statistics-file-name
	     (file-writable-p tcode-input-statistics-file-name))
    (let ((bufname " *tcode: statistics*")
	  (list (tcode-get-input-statistics-list))
	  last)
    (save-excursion
      (tcode-set-work-buffer bufname tcode-input-statistics-file-name nil t)
      (erase-buffer)
      (insert ";; このファイルは自動的に更新されます。"
	      "編集しないでください。\n")
      (insert "(setq tcode-input-statistics-list '(\n")
      (while list
	(let ((entry (car list)))
	  (if (and (not (equal entry last))
		   (tcode-encode (tcode-string-to-char (car entry)))
		   (or (> (car (cdr entry)) 0)
		       (> (cdr (cdr entry)) 0)))
	      (insert (format "\t%s\n" (prin1-to-string entry))))
	  (setq last entry))
	(setq list (cdr list)))
      (insert "))\n")
      (tcode-save-buffer bufname tcode-input-statistics-file-name t)))))

(add-hook 'kill-emacs-hook 'tcode-save-input-statistics)

(setq tcode-input-filter-functions
      (cons '(tcode-count-input-statistics . tcode-count-char)
	    tcode-input-filter-functions))

(add-hook 'tcode-help-stroke-hook
	  (lambda () (if tcode-count-input-statistics
		    (tcode-count-in-property (tcode-string-to-char ch)
					     'help-count))))

(if (fboundp 'defadvice)
    (defadvice delete-backward-char (before
				     tcode-cancel-input-count
				     activate)
      "Cancel input count for tcode."
      (if (tcode-on-p)
	  (tcode-cancel-input-count (tcode-preceding-char))))
  ;; for NEmacs
  (unless (fboundp 'tcode:delete-backward-char)
    (fset 'tcode:delete-backward-char
	  (symbol-function 'delete-backward-char))
    (defun delete-backward-char (n &optional killflag)
      (interactive "*p")
      (if (tcode-on-p)
	  (tcode-cancel-input-count (tcode-preceding-char)))
      (tcode:delete-backward-char n killflag))))

(provide 'tc-stat)

;;; tc-stat.el ends here
