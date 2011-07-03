;;; haskell-neverb-indent.el --- Neverb's (itakingiteasy) indentation style
;;
;; Copytight (C) 2011 Alexander <itakingiteasy> Tumin
;;                      <itakingiteasy@neverb.net>
;;
;; Add folowing to your .emacs file:
;;
;; (add-hook 'haskell-mode-hook '(lambda () 
;;   (interactive) 
;;   (turn-on-haskell-neverb-indent)
;;	 (local-set-key (kbd "RET") 'newline-and-indent)
;;   (local-set-key (kbd "DEL") 'haskell-neverb-indent-del)
;;   (local-set-key (kbd "SPC") 'haskell-neverb-indent-spc)))
;; 
;;


(defvar shiftwidth 2)

(defun haskell-neverb-indent-message ()
  (interactive)
	(message "Using haskell-neverb-indent"))
(defun apply-regexp (regex str)
  (set-match-data nil)
  (string-match regex str)
  )

(defun line-get-much (sourceline)
  (interactive)
  (apply-regexp "^[[:space:]]*" sourceline)
  (match-end 0)
)

(defun haskell-neverb-indent-line (currline prevline)
  (interactive)
  (defvar wasidented)
  (setq currmuch (line-get-much currline))
  (setq prevmuch (line-get-much prevline))
  (setq case-fold-search nil)
  (setq mdata (save-match-data))
  (set-match-data nil)
  (setq wasidented nil)

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
  (save-excursion
	(setq currline (substring-no-properties (thing-at-point 'line)))
	(forward-line -1)
	(setq prevline (substring-no-properties (thing-at-point 'line))))
  
  (haskell-neverb-indent-line currline prevline)
  (if (= last-command-char ?\r)
	  (indent-to-left-margin)   
	)
  )

(defun haskell-neverb-indent-line-del (currline prevline)
  (interactive)
  (setq currmuch (line-get-much currline))
  (setq prevmuch (line-get-much prevline))
  (setq mdata (save-match-data))
  (set-match-data nil)
  (setq wasidented nil)

  (if (not wasidented)
	  (progn 
		(apply-regexp ".*\\(|\\)" prevline)
		(let ((eqmatch (match-beginning 1)))
		  (if eqmatch
			  (progn
				(setq wasidented t)
				(setq left-margin eqmatch)
				)
			)
		  )
		)
	)


  (if (not wasidented)
	  (progn 
		(apply-regexp ".*[[:space:]]\+\\(do\\|case\\)\\([[:space:]]*\\)" prevline)
		(let ((eqmatch (match-end 2)))
		  (if eqmatch
			  (progn
				(setq wasidented t)
				(setq left-margin eqmatch)
				)
			)
		  )
		)
	)

  (if (not wasidented)
	  (progn
		(apply-regexp "^[[:space:]]*$" currline)
		(if (not (match-beginning 0))
			(progn 
			  (setq wasidented t)
			  (setq left-margin currmuch)
			  )
		  )
		)
	)

  (if (not wasidented)
	  (if (= left-margin 0)
		  (progn
			(setq wasidented t)
			(setq left-margin prevmuch)
			)
		)
	)


  (if (not wasidented)
	  (progn
		(setq wasidented t)
		(setq left-margin prevmuch)
	  )
	)
  (if (and (= left-margin prevmuch) (not wasidented))
	  (progn
		(setq wasidented t)
		(setq left-margin (- left-margin shiftwidth))
	  )
	)
  
  (set-match-data mdata)  
  )
(defun haskell-neverb-indent-del ()
  (interactive)

  (backward-delete-char 1)

  (setq mdata (save-match-data))
  (set-match-data nil)

  (save-excursion
	(setq uptocurs (buffer-substring-no-properties (line-beginning-position) (point)))
	(setq currmuch (current-indentation))
	(setq currline (substring-no-properties (thing-at-point 'line)))
	(forward-line -1)
	(setq prevmuch (current-indentation))
	(setq prevline (substring-no-properties (thing-at-point 'line)))
	(apply-regexp "^[[:space:]]*"  currline)
	(if (> (match-end 0) (length prevline))
		(setq prevcurs prevline)
	  (setq prevcurs (substring prevline 0 (match-end 0)))
	)
	)

  (haskell-neverb-indent-line-del uptocurs prevcurs)
  (indent-to-left-margin)
  )

(defun turn-on-haskell-neverb-indent ()
  (set (make-local-variable 'indent-line-function) 'haskell-neverb-indent)
  (run-hooks 'haskell-neverb-indent-hook))

(provide 'haskell-neverb-indent)
