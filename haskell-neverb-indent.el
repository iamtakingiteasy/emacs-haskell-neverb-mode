;;; haskell-neverb-indent.el --- Neverb's (itakingiteasy) indentation style
;;
;; Copytight (C) 2011 Alexander <itakingiteasy> Tumin
;;                      <itakingiteasy@neverb.net>
;;
;; Add folowing to your .emacs file:
;;
;; (add-hook 'haskell-mode-hook '(lambda ()
;;   (turn-on-haskell-neverb-indent)
;;   (local-set-key (kbd "RET") 'newline-and-indent)
;;   (local-set-key (kbd "DEL") 'haskell-neverb-indent-del)
;;   (setq haskell-neverb-tab-indent-on t))) 



;; globals
(defvar haskell-neverb-indent-shiftwidth 2)
(defvar haskell-neverb-tab-indent-on nil)
(defvar haskell-neverb-indent-brace-level 0)
(defvar haskell-neverb-indent-brace-level-group 0)
(defvar haskell-neverb-indent-open-brace-pos ())


;; regexps
(defvar haskell-neverb-indent-rexp-empty-space "[[:space:]]*\\(--.*\\|\\)")
(defvar haskell-neverb-indent-rexp-partials "\\([!#$%&*+,./< =>?@\\^|~-]\+\\|\<*do\\)")
(defvar haskell-neverb-indent-rexp-openbraces "[{([]")
(defvar haskell-neverb-indent-rexp-closebraces "[])}]")




(defun haskell-neverb-indent-message ()
	(message "Using haskell-neverb-indent"))


(defun haskell-neverb-filter (f &rest xs)
  (setq result ())
  (setq xs (car xs))
  (setq c (car xs))  
  (while c 
	(if (funcall f c) 
		(setq result (cons c result)))
	(setq xs (cdr xs))
	(setq c (car xs))) 
  (reverse result))

(defun haskell-neverb-apply-regexp (regex str)
  (set-match-data nil)
  (string-match regex str)
  )

(defun haskell-neverb-indent-line-get-much (sourceline)
  (apply-regexp "^[[:space:]]*" sourceline)
  (match-end 0)
)

(defun haskell-neverb-indent-is-empty-string ()
  (let ((thing (thing-at-point 'line)))
	(haskell-neverb-apply-regexp 
	 (concat "^" haskell-neverb-indent-rexp-empty-space "$") thing)
	(if (match-end 0)
		(setq rhs (match-end 0))
	  (setq rhs 0))
	(= rhs (- (length thing) 1))))


(defun haskell-neverb-indent-scroll-to-root ()
  (setq numlinesback 0)
  (setq linemoved 0)
  (while (and (= linemoved 0) (or (> (current-indentation) 0) (haskell-neverb-indent-is-empty-string)))
	(setq linemoved (forward-line -1))
	(setq numlinesback (+ numlinesback 1))) numlinesback)

(defun haskell-neverb-indent-check-partials ()
  (setq lm 0)
  (haskell-neverb-apply-regexp 
   (concat haskell-neverb-indent-rexp-partials 
		   haskell-neverb-indent-rexp-empty-space "$")
							   (thing-at-point 'line))
;  (if (match-beginning 0)
;	  (setq lm (+ lm haskell-neverb-indent-shiftwidth))) 
;  (- lm 2))
  lm
)


(defun haskell-neverb-substr-count  (ch str)
  (setq result 0)
  (setq sublen (length ch))
  (setq len (length str))
  (while (> len 0)
	(let ((och (substring str (- len sublen) len)))
	  (if (string= och ch)
		  (setq result (+ result 1))))
	(setq len (- len 1)))  result)
;  (while (match-beginning cnt)
;	(if (match-beginning cnt)
;		(setq cnt (+ cnt 1)))) cnt)

(defun haskell-neverb-indent-parse-braces (cline o c)
  (setq cbo (haskell-neverb-substr-count o cline))
  (setq cbc (haskell-neverb-substr-count c cline))
  (if (> cbo cbc)
	  ()
	  )
  (setq currlevel 0)
  (setq currlevel (+ currlevel cbo))
  (setq currlevel (- currlevel cbc))
  (print currlevel)
  (setq haskell-neverb-indent-brace-level
		(+ haskell-neverb-indent-brace-level currlevel))
;  (print haskell-neverb-indent-brace-level)
  )


(defun haskell-neverb-indent-parse-line ()
;  (interactive)
  (setq lm 0)
  (setq cline (thing-at-point 'line))
;  (print haskell-neverb-indent-brace-level)
  
  (haskell-neverb-indent-parse-braces cline "{" "}")
  (haskell-neverb-indent-parse-braces cline "(" ")")
  (haskell-neverb-indent-parse-braces cline "[" "]")

;  (setq lm (+ lm (haskell-neverb-indent-parse-braces cline "[" "]")))
;  (setq lm (+ lm (haskell-neverb-indent-parse-braces cline "(" ")")))
  lm
)

(defun haskell-neverb-indent-parent-level (lm)
  (let ((currind (current-indentation)))
;	(if (> currind 0)
;		(- (current-indentation) lm)
;	  0
;	  )
	)
  0
  )

(defun haskell-neverb-indent ()
  (interactive)
  (setq haskell-neverb-indent-brace-level 0)
  (setq haskell-neverb-indent-brace-level-group 0)
  (setq mdata (save-match-data)) 
  (set-match-data nil)     
;  (setq lm haskell-neverb-indent-shiftwidth)
  (setq lm 0)
  (save-excursion
	(setq numlinesback (haskell-neverb-indent-scroll-to-root))
	(setq lm (+ lm (haskell-neverb-indent-check-partials)))
	(setq i numlinesback)
	(while (>= i 0)
	  (setq i (- i 1))
	  (setq lm (+ lm (haskell-neverb-indent-parse-line)))
	  (if (= i 0) (setq lm (+ lm (haskell-neverb-indent-parent-level lm))))
	  (forward-line 1)))

  (set-match-data mdata)
;  (print lm)
;  (print haskell-neverb-indent-brace-level)
  (setq lm (+ lm (* haskell-neverb-indent-shiftwidth haskell-neverb-indent-brace-level)))
;  (print lm)
  (if (> numlinesback 0)
	  (progn
		(setq left-margin lm)
		(indent-to-left-margin))))



(defun haskell-neverb-indent-parens ()
  (interactive) 
  (self-insert-command 1) 
  (haskell-neverb-indent))

(defun turn-on-haskell-neverb-indent ()
  (set (make-local-variable 'indent-line-function) 'haskell-neverb-indent)
  (local-set-key (kbd "]") 'haskell-neverb-indent-parens)
  (local-set-key (kbd "}") 'haskell-neverb-indent-parens)
  (local-set-key (kbd ")") 'haskell-neverb-indent-parens)
  (local-set-key (kbd "<f2>") 'haskell-neverb-indent)
  (run-hooks 'haskell-neverb-indent-hook))

(provide 'haskell-neverb-indent)







