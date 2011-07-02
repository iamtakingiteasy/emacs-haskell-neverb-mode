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
(defun apply-regexp (regex str)
  (set-match-data nil)
  (string-match regex str)
  )

(defun haskell-neverb-indent-line ()
  (interactive)
  (defvar shiftwidth 2)
  (defvar wasidented)
  (setq case-fold-search nil)
  (setq mdata (save-match-data))
  (set-match-data nil)
  (setq wasidented nil)

  (save-excursion
	(setq currmuch (current-indentation))
	(setq currline (substring-no-properties (thing-at-point 'line)))
	(forward-line -1)
	(setq prevmuch (current-indentation))
	(setq prevline (substring-no-properties (thing-at-point 'line))))
  
  (setq left-margin 0)

  (string-match "^[[:space:]]*--.*$" prevline)
  (if (match-beginning 0)
	  (setq wasidented t))

  (if (not wasidented)
	  (progn
		(apply-regexp "^.*[[:space:]]\+\\(where\\|do\\|let\\|if\\|case\\)\\([[:space:]]\+\\)" prevline)
		(let ((eqmatch (match-end 2)))
		  (if eqmatch
			  (progn
				(setq wasidented t)
				(setq left-margin eqmatch))))))

  

  (if (not wasidented)
	  (progn
		(apply-regexp "^[[:alnum:]]*[[:space:]]*=\\([[:space:]]*\\).*$" prevline)
		(let ((eqmatch (match-end 1)))
		  (if eqmatch
			  (progn
				(setq wasidented t)
				(setq left-margin eqmatch))))))

  (if (not wasidented)
	  (progn
		(apply-regexp "^[[:space:]]*data[^=]*\\(=\\).*$" prevline)
		(let ((eqmatch (match-beginning 1)))
		  (if eqmatch
			  (progn
				(setq wasidented t)
				(setq left-margin eqmatch))))))

  (if (not wasidented)
	  (progn
		(setq rexp "^[[:space:]]*\\(|\\)")
		(apply-regexp rexp currline)
		(if (match-beginning 1)
			(progn
			  (apply-regexp rexp prevline)
			  (if (not (match-beginning 1))
				  (progn
					(setq wasidented t)
					(setq left-margin (+ shiftwidth prevmuch))))))))

  (if (not wasidented)
	  (progn
		(apply-regexp "^[[:space:]]*\\(do\\|let\\|if\\|case\\)" currline)
		(if (match-beginning 0)
			(progn
			  (setq wasidented t)
			  (setq left-margin (+ shiftwidth prevmuch))))))

  (if (not wasidented)
	  (progn
		(apply-regexp "\\([!#$%&*+./<=>?@\\^|~-]\\|[[:space:]]do\\)[[:space:]]*$" prevline)
		(if (match-beginning 1)
			(progn
			  (setq wasidented t)
			  (setq left-margin (+ shiftwidth left-margin))))))

  
  (if (not wasidented)
	  (progn
		(apply-regexp "^[[:space:]]*$" prevline)
		(if (/= 0 (match-beginning 0))
			(progn
			  (setq wasidented t)
			  (setq left-margin prevmuch)))))
  (set-match-data mdata)  
 
  )

(defun haskell-neverb-indent ()
  (interactive)
  (haskell-neverb-indent-line)
  (indent-to-left-margin)   
  )

(defun turn-on-haskell-neverb-indent ()
  (set (make-local-variable 'indent-line-function) 'haskell-neverb-indent)
  (run-hooks 'haskell-neverb-indent-hook))

(provide 'haskell-neverb-indent)
