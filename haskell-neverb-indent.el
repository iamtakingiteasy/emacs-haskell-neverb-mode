;;; haskell-neverb-indent.el --- Neverb's (itakingiteasy) indentation style
;;
;; Copytight (C) 2011 Alexander Tumin
;;
;;
;; some more stuff to type
;;

(defun haskell-neverb-indent-message ()
  (interactive)
  (message "Using haskell-neverb-indent"))

(defun haskell-neverb-indent ()
  (interactive)
  (defvar prevline) (defvar prevident)
  (defvar currline) (defvar currident)
  (defvar wasidented)
  (setq wasidented nil)
  (save-excursion 
	(setq currline (substring-no-properties (thing-at-point 'line)))
	(setq currident (current-indentation))
	(forward-line -1)
	(setq prevline (substring-no-properties (thing-at-point 'line)))
	(setq prevident (current-indentation))) 
  (setq case-fold-search nil)


  (string-match "^[[:space:]]*data[^=]*\\(=\\).*$" prevline)
  (let ((eqmatch (match-beginning 1)))
	(if (string= "=" (match-string 1 prevline))
		(progn
		  (setq wasidented t)
		  (setq left-margin eqmatch)
		  (indent-to-left-margin)
		)))

  (string-match "^[[:space:]]*where\\([[:space:]]*\\).*$" prevline)
  (let ((eqmatch (match-end 1)))
	(setq wasidented t)
	(setq left-margin eqmatch)
	(indent-to-left-margin)
	)


  
  (string-match "^[[:space:]]*$" prevline)
  (if (match-beginning 0)
	  (indent-to-column prevident))
  (if (not wasidented)
	  (beginning-of-line)
	)
  )

(defun turn-on-haskell-neverb-indent ()
  (set (make-local-variable 'indent-line-function) 'haskell-neverb-indent)
  (run-hooks 'haskell-neverb-indent-hook))

(provide 'haskell-neverb-indent)

(global-set-key (kbd "<f2>") 'haskell-neverb-indent)