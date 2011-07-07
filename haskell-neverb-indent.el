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

(require 'cl)

(defvar haskell-neverb-indent-shiftwidth 2)
(defvar haskell-neverb-indent-absolute 0)

(defstruct neverb-indentation-level name level)
(defstruct neverb-node (childs nil) (parent nil))
;(defstruct (neverb-haskell-root-node (:include neverb-node)))
;(defstruct (neverb-haskell-function-node (:include neverb-node)))
(make-symbol "root-node")
(make-symbol "function-node")

(make-symbol "data-node")
(make-symbol "data-name-node")
(make-symbol "data-pipe-node")
(make-symbol "data-deriving-node")


(make-symbol "class-node")

(make-symbol "tuple-node")
(make-symbol "list-node")
(make-symbol "block-node")

(make-symbol "where-node")



(defvar haskell-neverb-indentation-levels
  (list 
   (make-neverb-indentation-level :level "0"  :name "base")

   (make-neverb-indentation-level :level "+"  :name "function-open")
   (make-neverb-indentation-level :level "="  :name "function-inside")
   (make-neverb-indentation-level :level "-"  :name "function-close")

   (make-neverb-indentation-level :level "+"  :name "tuple-open")
   (make-neverb-indentation-level :level "="  :name "tuple-inside")
   (make-neverb-indentation-level :level "-"  :name "tuple-close")

   (make-neverb-indentation-level :level "+"  :name "list-open")
   (make-neverb-indentation-level :level "="  :name "list-inside")
   (make-neverb-indentation-level :level "-"  :name "list-close")

   (make-neverb-indentation-level :level "+"  :name "block-open")
   (make-neverb-indentation-level :level "="  :name "block-inside")
   (make-neverb-indentation-level :level "-"  :name "block-close")

   (make-neverb-indentation-level :level "+"  :name "where-open")
   (make-neverb-indentation-level :level "="  :name "where-inside")
   (make-neverb-indentation-level :level "-"  :name "where-close")
   ))

(defun neverb-lookup-update-mapperf (name value el)
  (if (string= name (neverb-indentation-level-name el))
	  (make-neverb-indentation-level :level value :name name) el))

(defun neverb-lookup-update-mapper (name value)
  (setq haskell-neverb-indentation-levels 
		(mapcar '(lambda (x) 
				   (neverb-lookup-update-mapperf name value x)) 
				haskell-neverb-indentation-levels)))

(defun neverb-lookup-indent (key)
  (neverb-indentation-level-level 
   (find key
		 haskell-neverb-indentation-levels 
		 :key 'neverb-indentation-level-name 
		 :test 'string=)))

(defun neverb-lookup-string (iitem iseq)
  (find iitem iseq :test 'string=))


(defun haskell-neverb-indent-translate-parse (str)
  (setq result 0)
  (case (string-to-char (substring str 0 1))
	(?+ (setq result (+ haskell-neverb-indent-absolute (string-to-number str))))
	(?- (setq result (+ haskell-neverb-indent-absolute (string-to-number str))))
	(t (setq result (string-to-number str)))) result)
(defun haskell-neverb-indent-translate (mode)
  (setq result 0)
  (if (string= mode "=") 
	  (setq result haskell-neverb-indent-absolute))
  (if (string= mode "") 
	  (setq result haskell-neverb-indent-absolute))
  (if (string= mode "+") 
	  (setq result (+ result (* haskell-neverb-indent-shiftwidth 1))))
  (if (string= mode "++") 
	  (setq result (+ result (* haskell-neverb-indent-shiftwidth 2))))
  (if (string= mode "-") 
	  (setq result (- result (* haskell-neverb-indent-shiftwidth 1))))
  (if (string= mode "--") 
	  (setq result (- result (* haskell-neverb-indent-shiftwidth 2))))
  (if (= result 0)
	(setq result (haskell-neverb-indent-translate-parse mode)))
  result)

(defun neverb-lookup-translate (key)
  (haskell-neverb-indent-translate (neverb-lookup-indent key))
)

(defun neverb-apply-regexp (rexp str)
  (set-match-data nil) 
  (string-match rexp str))

(defun neverb-check-if-space (ch)
  (or (= ?\s (char-syntax ch)) (= ch ?\n)))
(defun neverb-check-if-space-string (str)
  (every 'neverb-check-if-space str))
(defun haskell-neverb-commented-out (str)
  (neverb-apply-regexp "^[[:space:]]*--.*$" str))

(defun haskell-neverb-root-node ()
  (setq thing (thing-at-point 'line))
  (and
   (= (current-indentation) (neverb-lookup-translate "base"))
   (not (neverb-check-if-space-string thing))
   (not (haskell-neverb-commented-out thing))))

(defun haskell-neverb-indent-scroll-to-root ()
  (setq l 0)
  (loop while (and (= l 0) (not (haskell-neverb-root-node))) do
		(setq l (forward-line -1)))
  (beginning-of-line))

(defun neverb-get-next-char ()
  (setq ch (following-char))
  (forward-char)
  ch
)
(defun neverb-get-next-word ()
  (skip-chars-forward "^_a-z0-9A-Z'")
  (setq curpoint (point))
  (setq diff (skip-chars-forward "_a-z0-9A-Z'"))
  (setq word (buffer-substring curpoint (+ curpoint diff)))
  (if (string= "" word) nil word))


(defun neverb-skip-quote (ch)
  (if (or (not (char-equal ch (following-char))) (char-equal (preceding-char) ?\\))
	  (progn
		(forward-char)
		(skip-chars-forward (concat "^" (char-to-string ch)))
		(if (char-equal (preceding-char) ?\\)
			(neverb-skip-quote ch)
		  )
		)
	  )
  (if (char-equal (following-char) ch)
	  (forward-char)
  )
)


(defun haskell-neverb-strip-literals (cursorpoint)
  (setq result (generate-new-buffer "Haskell parsing buffer"))

  (setq isquote nil)
  (loop while (<= (point) cursorpoint) do
		(setq ch (neverb-get-next-char))
		(if (or (char-equal ?\" ch) (char-equal ?\' ch))
			(progn
			  (neverb-skip-quote ch)
			  (setq isquote t))
		  (setq isquote nil))

		(if  (not isquote) (with-current-buffer result (insert-char ch 1))))
  (with-current-buffer result (goto-char 0))
  result
  )

(defun haskell-neverb-indent-scroll-to-indent (ind)
  (lexical-let ((result 0))
	(while 
		(and 
		 (= (forward-line 1) 0) 
		 (or 
		  (> (current-indentation) ind) 
		  (haskell-neverb-commented-out (thing-at-point 'line)))))
	(setq result (point))
	result
	))

(defun haskell-neverb-data-parse-pipe ()
  (push word tmpresult)
)

(defun haskell-neverb-data-parse (bbegin bend)
  (lexical-let ((result nil))
	(push 'data-node result)
	(goto-char bbegin)
	(forward-word 2)
 	(push (cons (thing-at-point 'word) (cons 'data-name-node nil)) result)
	(skip-chars-forward "[:space:]")
	(print (following-char))
;	(print (thing-at-point 'word))
;	(print (thing-at-point 'word))
	
;	(setq tmpresult nil)
;	(setq drvresult nil)
;	(setq rcrresult nil)
;	(setq imode 0)
;
;	(push 'data-pipe-node tmpresult)
;
;	(while (< (point) bend)
;	  (let ((word (neverb-get-next-word)))
;		(if word
;			(case imode
;			  (0 (haskell-neverb-data-parse-pipe))
;			  (1 (haskell-neverb-data-parse-derv))
;			  (2 (haskell-neverb-data-parse-rcrd))
;			  )
;		  )
;		)
;	  )
;	(push tmpresult result)
;	(push drvresult result)
	result
  ))

(defun neverb-reverse-all (list)
  (if (listp list)
	  (progn
		(setq res (reverse list))
		(setq res (mapcar 'neverb-reverse-all res)))
	(setq res list)) res)

(defun haskell-neverb-extent-parse ()
  (setq result nil)
  (while (not (eobp))
	(beginning-of-line)
	(setq blockstart (point))
	(skip-chars-forward "[:space:]")
	(setq cword (thing-at-point 'word))
	(setq blockend (haskell-neverb-indent-scroll-to-indent (current-indentation)))
	
	(save-excursion
	  (if (string= "data" cword) 
		  (push (haskell-neverb-data-parse blockstart blockend) result)))
	)
  (setq result (mapcar 'neverb-reverse-all result))
  (print result)
  result
)


(defun haskell-neverb-initial-parser (cursorpoint)
  (setq parsebuffer (haskell-neverb-strip-literals cursorpoint))
  (setq result (with-current-buffer parsebuffer (haskell-neverb-extent-parse)))
  (kill-buffer parsebuffer)
  result
)


(defun haskell-neverb-indent ()
  (interactive)
  (save-excursion
	(end-of-line)
	(setq cursorpoint (point))
	(haskell-neverb-indent-scroll-to-root)
	(haskell-neverb-initial-parser cursorpoint)
	)
  )


(defun turn-on-haskell-neverb-indent ()
  (set (make-local-variable 'indent-line-function) 'haskell-neverb-indent)
;  (local-set-key (kbd "]") 'haskell-neverb-indent-parens)
;  (local-set-key (kbd "}") 'haskell-neverb-indent-parens)
;  (local-set-key (kbd ")") 'haskell-neverb-indent-parens)
;  (local-set-key (kbd "<f2>") 'haskell-neverb-parser)
  (run-hooks 'haskell-neverb-indent-hook))

(provide 'haskell-neverb-indent)







