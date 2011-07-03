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


(defun filter (f &rest xs)
  (setq result ()ч)
  (setq xs (car xs))
  (setq c (car xs))  
  (while c 
	(if (funcall f c) 
		(setq result (cons c result)))
	(setq xs (cdr xs))
	(setq c (car xs))) 
  (reverse result))

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
		(apply-regexp "^.*[[:space:]]\+\\(where\\|do\\|let\\|if\\|case\\||\\)\\([[:space:]]\+\\)" prevline)
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

(defun haskell-neverb-indent-line-del (currline prevline prevlinefull)
  (interactive)
  (setq currmuch (line-get-much currline))
  (setq prevmuch (line-get-much prevline))
  (setq mdata (save-match-data))
  (set-match-data nil)
  (setq wasidented nil)
  
  (setq lm 0)
  
;  (if (not wasidented)
;	  (progn
;		(apply-regexp "\\(|\\)" prevlinefull)
;		(let ((eqmatch (match-beginning 1)))
;		  (if eqmatch
;			  (progn
;				(setq wasidented t)
;				(setq lm eqmatch))))))

  (if (not wasidented)
	  (progn
		(apply-regexp "^.*[[:space:]]\+\\(do\\|case\\||\\)\\([[:space:]]*\\).*$" prevline)
		(let ((eqmatch (match-end 2)))
		  (if eqmatch
			  (progn
				(setq wasidented t)
				(setq lm eqmatch))))))

  (if (not wasidented)
	  (progn
		(setq wasidented t)
		(setq lm prevmuch)))
  

  (set-match-data mdata)  
  lm
  )
(defun haskell-neverb-indent-del ()
  (interactive)

  (backward-delete-char 1)

  (setq mdata (save-match-data))
  (set-match-data nil)
  (setq backlist ())
  (save-excursion
	(setq currline (substring-no-properties (thing-at-point 'line)))
	(apply-regexp "^[[:space:]]*" currline)
	(setq currstop (match-end 0))
	(setq endreached nil)
	(if (>= (+ (line-beginning-position) currstop) (point))
		(while (not endreached)
		  (forward-line -1)
		  (setq endreached (= (current-indentation) 0))
		  (setq prevline (substring-no-properties (thing-at-point 'line)))
		  (if (> currstop (length prevline))
			  (setq prevcurs prevline)
			(setq prevcurs (substring prevline 0 currstop))
			)
		  (setq backlist (cons (haskell-neverb-indent-line-del currline prevline prevline) backlist))
		  )
	  )
	)

;  (print backlist) ;; debug
;  (print currstop) ;; debug
  (setq left-margin (apply 'max (apply '(lambda (&rest xs) (filter '(lambda (x) (< x currstop)) xs)) backlist)))
  (indent-to-left-margin)
  )



(defun turn-on-haskell-neverb-indent ()
  (set (make-local-variable 'indent-line-function) 'haskell-neverb-indent)
  (run-hooks 'haskell-neverb-indent-hook))

(provide 'haskell-neverb-indent)







