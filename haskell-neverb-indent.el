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

;;  (setq tab-stop-list '(1 2 4 8 16))

  (if (not wasidented)
	  (progn
		(string-match "^[[:alnum:]]*[[:space:]]*=\\([[:space:]]*\\).*$" prevline)

		(let ((eqmatch (match-end 1)))
		(if eqmatch
			  (progn
				(setq wasidented t)
				(setq left-margin eqmatch))))))

  (setq rexp "^[[:space:]]*\\(|\\)")
  (if (not wasidented)
	  (progn
		(string-match rexp currline)
		(if (match-end 1)
			(progn
			  (string-match rexp prevline)
			  (if (not (match-end 1))
				  (progn
					(print "lol")
					(setq wasidented t)
					(setq left-margin (+ shiftwidth prevmuch))
				   )
			   )
			  )
		  )
		)
	)
  

  (if (not wasidented)
	  (progn
		(string-match "^[[:space:]]*\\(where\\|do\\|let\\|if\\|case\\)" currline)
		(let ((eqmatch  (match-beginning 0)))
		(if (/= 0 eqmatch)
			  (progn
				(print "kaka")
				(setq wasidented t)
				(setq left-margin (+ shiftwidth prevmuch))
				)))))



  
  (if (not wasidented)
	  (progn
		(string-match "^.*[[:space:]]\+\\(where\\|do\\|let\\|if\\|case\\)\\([[:space:]]\+\\)" prevline)

		(let ((eqmatch (match-end 2)))
		(if eqmatch
			  (progn
				(setq wasidented t)
				(setq left-margin eqmatch))))))

  
  (if (not wasidented)
	  (progn
		(string-match "\\([!#$%&*+./<=>?@\\^|~-]\\|[[:space:]]do\\)$" prevline)
		(let ((eqmatch (match-beginning 0)))
		  (if (not eqmatch)
			  (progn
				(setq wasidented t)
				(setq left-margin (+ shiftwidth prevmuch))
				)))))

  
  (if (not wasidented)
	  (progn
		(string-match "^[[:space:]]*data[^=]*\\(=\\).*$" prevline)
		(let ((eqmatch (match-beginning 1)))
		  (if eqmatch
			  (progn
				(setq wasidented t)
				(setq left-margin eqmatch))))))

  

  (string-match "^[[:space:]]*$" prevline)
  (if (not wasidented)
	  (if (/= 0 (match-beginning 0))
;;	  (progn
;;		(string-match "^[^[:space:]]\+" (thing-at-point 'line))
;;		(if (/= 0 (match-beginning 0))
		  (progn
			(setq wasidented t)
			(print "zzz")
			(setq left-margin prevmuch)
			)
		)
;;		  (setq left-margin (+ prevmuch shiftwidth))
	);))

  
  
    
  (set-match-data mdata)
)

(defun haskell-neverb-indent-del ()
  (interactive)
  (delete-backward-char 1)
  ;;(indent-to-left-margin)
;;  (message (number-to-string left-margin))
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
(provide 'haskell-neverb-indent-del)
  
  