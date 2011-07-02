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
  (defvar shiftwidth 2)
  (defvar tabstops)
  (defvar wasidented)
  (setq case-fold-search nil)
  (setq mdata (save-match-data))
  (set-match-data nil)
  (setq wasidented nil)

  (save-excursion
	(forward-line -1)
	(setq prevmuch (current-indentation))
	(setq prevline (substring-no-properties (thing-at-point 'line))))

  (setq left-margin 0)

  (string-match "^[[:space:]]*--.*$" prevline)
  (if (match-beginning 0)
	  (setq wasidented t))


  (if (not wasidented)
	  (progn
		(string-match "^.*[[:space:]]\+\\(where\\|do\\|let\\|if\\|case\\|=\\)\\([[:space:]]\+\\)" prevline)

		(let ((eqmatch (match-end 2)))
		(if eqmatch
			  (progn
				(message (match-string 1 prevline))
				(setq wasidented t)
				(setq left-margin eqmatch))))))



  
  
  (if (not wasidented)
	  (progn
		(string-match "\\([!#$%&*+./<=>?@\\^|~-]$\\|[[:space:]]do$\\)" prevline)
		(let ((eqmatch (match-beginning 1)))
		  (if eqmatch
			  (progn
				(setq wasidented t)
				(setq left-margin (+ shiftwidth prevmuch)))))))
  (if (not wasidented)
	  (progn
		(string-match "^[[:space:]]*data[^=]*\\(=\\).*$" prevline)
		(let ((eqmatch (match-beginning 1)))
		  (if eqmatch
			  (progn
				(setq wasidented t)
				(setq left-margin eqmatch))))))

  

  (string-match "^[[:space:]]*$" prevline)
  (if (and (not wasidented) (/= 0 (match-beginning 0)))
	  (progn
		(string-match "^[^[:space:]]\+" (thing-at-point 'line))
		(if (/= 0 (match-beginning 0))
			(setq left-margin prevmuch)
		  )
		)
	)

  
  
  
  


  
  
  
  (indent-to-left-margin)
  
  (set-match-data mdata)
)


(defun turn-on-haskell-neverb-indent ()
  (set (make-local-variable 'indent-line-function) 'haskell-neverb-indent)
  (run-hooks 'haskell-neverb-indent-hook))

(provide 'haskell-neverb-indent)

(global-set-key (kbd "<f2>") 'haskell-neverb-indent)