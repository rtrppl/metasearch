;; metasearch.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Maintainer: René Trappel <rtrappel@gmail.com>
;; URL:
;; Version: 0.1.2
;; Package-Requires: emacs "26"
;; Keywords: search web

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A package to quickly start a search on one search querry on multiple 
;; search engines.
;;
;; 0.1.2
;; - Fix for Linux, which uses xdg-open
;;
;; 0.1.1
;; - Includes a small fix for "Trailing garbage following expression" bug

(when (eq system-type 'darwin)
  (defvar open-cmd "open"))

(when (eq system-type 'gnu/linux)
  (defvar open-cmd "xdg-open"))

(defun metasearch-add-search-engine ()
  "Adds a search engine to the list of search engines."
  (interactive)
  (let* ((metasearch-list-search-engines (metasearch-get-list-of-search-engines))
	 (new-search-engine (read-from-minibuffer "Enter a search engine query URL: "))
	 (name (read-from-minibuffer "Please provide a name for the new search engine: "))
	 (name (replace-regexp-in-string "[\"'?:;\\\/]" "_" name)))
    (when (not metasearch-list-search-engines)
      (setq metasearch-list-search-engines (make-hash-table :test 'equal)))
    (puthash (concat name "::main::") new-search-engine metasearch-list-search-engines)
    (with-temp-buffer
      (let* ((json-data (json-encode metasearch-list-search-engines)))
	(insert json-data)
	(write-file "~/.metasearch-search-engine-list")))))

(defun metasearch-modify-set ()
 "Adds or removes a search engine to a search set."
 (interactive)
 (let* ((search-sets (metasearch-get-all-sets))
	(set-name))
   (when (= (length search-sets) 1)
       (if (yes-or-no-p (format "There are no search sets defined. Create one? "))
	 (setq set-name (read-from-minibuffer "Provide a name of the search set: "))))
   (when (> (length search-sets) 1)
     (setq search-sets (remove "main" search-sets)) ;; "main" should never be modified.
     (sort search-sets 'string<)    
     (setq search-sets (metasearch-presorted-completion-table search-sets))
     (setq set-name (completing-read "Select search set to modify or create new: " search-sets)))
   (let* ((metasearch-list-search-engines (metasearch-get-list-of-search-engines))
	  (search-engines (hash-table-keys metasearch-list-search-engines))
	  (search-engines-pure (metasearch-pure-list search-engines))
	  (existing-search-engines)
	  (selection))
     (while (not (string-empty-p selection))
       (setq existing-search-engines (metasearch-return-set-search-engines set-name))
        (setq search-engines-pure (metasearch-presorted-completion-table search-engines-pure)) 
       (setq selection (completing-read (format "Currently the search set \"%s\" includes: %s. Select to add or remove. Empty selection ends the process: " set-name (metasearch-prepare-results-list existing-search-engines)) search-engines-pure))
       (when (not (string-empty-p selection))
	 (metasearch-add-or-remove-from-set selection set-name))))))
	  
(defun metasearch-add-or-remove-from-set (pure-search-engine set-name)
 "Adds a search engine to a set or removes it."
 (let* ((existing-key (metasearch-return-full-name-from-pure-selection pure-search-engine))
	(existing-search-engines (metasearch-return-set-search-engines set-name))
	(new-key))
   (when (string-match-p (concat "::" set-name "::") existing-key)
       (setq new-key (replace-regexp-in-string (regexp-quote (concat "::" set-name "::")) "" existing-key)))
   (when (not (string-match-p (concat "::" set-name "::") existing-key))
     (setq new-key (concat existing-key "::" set-name "::")))
   (metasearch-update existing-key new-key)))
 
(defun metasearch-update (existing-key new-key)
 "Updates a search engine entries." 
 (let ((metasearch-list-search-engines (make-hash-table :test 'equal))
       (value))
   (with-temp-buffer
     (insert-file-contents "~/.metasearch-search-engine-list")
     (if (fboundp 'json-parse-buffer)
	 (setq metasearch-list-search-engines (json-parse-buffer))))
   (setq value (gethash existing-key metasearch-list-search-engines))
   (remhash existing-key metasearch-list-search-engines)
   (puthash new-key value metasearch-list-search-engines)
   (with-temp-buffer
     (let* ((json-data (json-encode metasearch-list-search-engines)))
       (insert json-data)
       (write-file "~/.metasearch-search-engine-list")))))

(defun metasearch-return-set-search-engines (set-name)
 "Returns all search-engines for search set."
 (let* ((metasearch-list-search-engines (metasearch-get-list-of-search-engines))
	(search-engines (hash-table-keys metasearch-list-search-engines))
	(results))
    (dolist (item search-engines)
      (if (string-match-p (concat "::" set-name "::") item)
	  (add-to-list 'results item)))
    (setq results (metasearch-pure-list results))
    results))

(defun metasearch-prepare-results-list (results)
  "Turning the list into a nice string"
  (setq results (mapconcat 'identity results ", ")))

(defun metasearch-presorted-completion-table (list)
  "Maintains the sorting of the list (for completing-read)."
  (let ((list-completion ()))
    (setq list-completion 
	  (lambda (string pred action)
	    (if (eq action 'metadata)
		`(metadata (display-sort-function . ,#'identity))
	    (complete-with-action action list string pred))))
list-completion))
	 
(defun metasearch-get-all-sets ()
  "Returns a list of all sets."
  (let* ((metasearch-list-search-engines (metasearch-get-list-of-search-engines))
	  (search-engines (hash-table-keys metasearch-list-search-engines))
 	  (search-sets))
     (dolist (item search-engines)
       (with-temp-buffer
	 (insert item)
	 (goto-char (point-min))
	 (while (and (re-search-forward "::\\(.*?\\)\\::" nil t)
		     (not (string-empty-p (match-string 1))))
	   (add-to-list 'search-sets (match-string 1)))))
     search-sets))

(defun metasearch-remove-search-engine ()
  "Remove a search engine from the list."
  (interactive)
  (let* ((metasearch-list-search-engines (metasearch-get-list-of-search-engines))
	 (search-engines (hash-table-keys metasearch-list-search-engines))
	 (search-engines-pure (metasearch-pure-list search-engines))
	 (json-data)
	 (selection))
 (setq selection
 (completing-read "Which search-engine should be removed? " search-engines-pure))
 (setq selection (metasearch-return-full-name-from-pure-selection selection))
  (if (not (member selection search-engines))
      (message "Search engine does not exist.")
    (if (yes-or-no-p (format "Are you sure you want to remove %s as a search-engine? " (gethash selection metasearch-list-search-engines)))
	  (progn
	    (remhash selection metasearch-list-search-engines)
	    (with-temp-buffer
	      (setq json-data (json-encode metasearch-list-search-engines))
	      (insert json-data)
	      (write-file "~/.metasearch-search-engine-list")))))))

(defun metasearch-pure-list (search-engines)
  "Create a list of search engines without sets."
 (let ((search-engines-pure))
   (dolist (item search-engines)
      (push (replace-regexp-in-string "\\(::.*\\)" "" item) search-engines-pure))
   (sort search-engines-pure 'string<)    
   search-engines-pure))

(defun metasearch-return-full-name-from-pure-selection (selection)
  "Return the full name of search engine with sets."
  (let* ((metasearch-list-search-engines (metasearch-get-list-of-search-engines))
	 (search-engines (hash-table-keys metasearch-list-search-engines))
	 (full-name))
    (dolist (item search-engines)
      (when (string-prefix-p (concat selection "::main::") item)
       (setq full-name item)))
    full-name))

(defun metasearch-get-list-of-search-engines ()
 "Return metasearch-name-search-engine, a hashtable that includes a list of names and locations of all search-engines."
 (let ((metasearch-file-exists (metasearch-check-for-search-engine-file)))
   (when metasearch-file-exists
     (let ((metasearch-list-search-engines (make-hash-table :test 'equal)))
       (with-temp-buffer
	 (insert-file-contents "~/.metasearch-search-engine-list")
	 (if (fboundp 'json-parse-buffer)
	     (setq metasearch-list-search-engines (json-parse-buffer))))
metasearch-list-search-engines))))

(defun metasearch-check-for-search-engine-file ()
  "Checks for a search-engine file in ~/.metasearch-search-engine-list."
  (interactive)
  (let ((metasearch-file-exists nil)
	(metasearch-list-search-engines (make-hash-table :test 'equal))
	(length-of-list))
  (when (file-exists-p "~/.metasearch-search-engine-list")
    (with-temp-buffer
	 (insert-file-contents "~/.metasearch-search-engine-list")
	 (if (fboundp 'json-parse-buffer)
	     (setq metasearch-list-search-engines (json-parse-buffer)))
	 (setq length-of-list (length (hash-table-values metasearch-list-search-engines)))
	 (when (not (zerop length-of-list))
	   (setq metasearch-file-exists t))))
  metasearch-file-exists))
    
(defun metasearch-search-all ()
  "Starts a search on all available search engines."
  (interactive)
  (let ((metasearch-file-exists (metasearch-check-for-search-engine-file)))
    (when (not metasearch-file-exists)
      (message "In order to use metasearch you first need to add search engines via metasearch-add-search-engine."))
    (when metasearch-file-exists
      (let* ((metasearch-list-search-engines (metasearch-get-list-of-search-engines))
	     (search-engines (nreverse (hash-table-values metasearch-list-search-engines)))
	     (search-query)
	     (search-cmd))
	(if (region-active-p)
	    (setq search-query (buffer-substring-no-properties (region-beginning)(region-end)))
	  (setq search-query (read-from-minibuffer "Search: ")))
	(dolist (search-engine search-engines)
	  (setq search-cmd (concat search-engine search-query))
	  (setq search-cmd (shell-quote-argument search-cmd))
	  (start-process-shell-command "metasearch.el" nil (concat open-cmd " " search-cmd)))))))

(defun metasearch-search-set (&optional set query)
  "Starts a search with a search set."
  (interactive)
  (let ((metasearch-file-exists (metasearch-check-for-search-engine-file)))
    (when (not metasearch-file-exists)
      (message "In order to use metasearch you first need to add search engines via metasearch-add-search-engine."))
    (when metasearch-file-exists
      (let* ((metasearch-list-search-engines (metasearch-get-list-of-search-engines))
	     (search-engines (hash-table-values metasearch-list-search-engines))
	     (search-sets (metasearch-get-all-sets))
	     (selected-set)
	     (matched-search-engines)
	     (search-query)
	     (search-cmd))
	(when (not set)
	  (setq search-sets (remove "main" search-sets)) ;; "main" should not be an option here.
	  (sort search-sets 'string<)
	  (setq selected-set (completing-read "Chose a search set: " (metasearch-presorted-completion-table search-sets))))
	(when set
	  (setq selected-set set))
	(when query
	  (setq search-query query))
	(when (not query)
	  (if (region-active-p)
	      (setq search-query (buffer-substring-no-properties (region-beginning)(region-end)))
	    (setq search-query (read-from-minibuffer "Search: "))))
	(when (member selected-set search-sets)
	  (setq matched-search-engines (metasearch-return-search-engines-for-set selected-set))
	   (dolist (item matched-search-engines)
	     (setq search-cmd (concat item search-query))
	     (setq search-cmd (shell-quote-argument search-cmd))
	     (start-process-shell-command "metasearch.el" nil (concat open-cmd " " search-cmd))))
	(when (not (member selected-set search-sets))
	  (message "This search set does not exist."))))))

(defun metasearch-return-search-engines-for-set (set)
  "Returns a list with all values for a search-set."
  (let* ((metasearch-list-search-engines (metasearch-get-list-of-search-engines))
	(search-engines (hash-table-keys metasearch-list-search-engines))
	(results))
    (dolist (item search-engines)
      (when (string-match-p (concat "::" set "::") item)
	(add-to-list 'results (gethash item metasearch-list-search-engines))))
    results))

(provide 'metasearch)
