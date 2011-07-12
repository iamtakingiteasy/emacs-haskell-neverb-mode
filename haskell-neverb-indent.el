;;; haskell-neverb-indent.el --- Neverb's (itakingiteasy) indentation style
;;
;; Copytight (C) 2011 Alexander <itakingiteasy> Tumin
;;                      <itakingiteasy@neverb.net>
;;
;; Add folowing to your .emacs file:
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DATA

(require 'cl)

(make-symbol "node")

(make-symbol "node-data")
(make-symbol "node-data-name")
(make-symbol "node-data-eq-sign")
(make-symbol "node-data-variant")

(make-symbol "node-group-block")
(make-symbol "node-group-block-entry")
(make-symbol "node-group-tuple")
(make-symbol "node-group-tuple-entry")
(make-symbol "node-group-list")
(make-symbol "node-group-list-entry")

(defstruct haskell-node 
  (name 'node) 
  (line 0) 
  (indent 0) 
  (special nil) 
  (childs nil))

(defvar neverb-indent-levels (make-hash-table :test 'equal))
(clrhash neverb-indent-levels)
(setf (gethash "absolute"     neverb-indent-levels) 0)
(setf (gethash "desired"      neverb-indent-levels) 0)
(setf (gethash "base"         neverb-indent-levels) 0)
(setf (gethash "shiftwidth"   neverb-indent-levels) 2)

(setf (gethash "data-open"    neverb-indent-levels) "+")
(setf (gethash "data-name"    neverb-indent-levels) "+")
(setf (gethash "data-eq-sign" neverb-indent-levels) "+")
(setf (gethash "data-variant" neverb-indent-levels) "&=")

(setf (gethash "tuple-open"   neverb-indent-levels) "+")
(setf (gethash "tuple-inside" neverb-indent-levels) "=")
(setf (gethash "tuple-close"  neverb-indent-levels) "-")

(setf (gethash "list-open"    neverb-indent-levels) "+")
(setf (gethash "list-inside"  neverb-indent-levels) "=")
(setf (gethash "list-close"   neverb-indent-levels) "-")

(setf (gethash "block-open"   neverb-indent-levels) "+")
(setf (gethash "block-inside" neverb-indent-levels) "=")
(setf (gethash "block-close"  neverb-indent-levels) "-")

(setf (gethash "where-open"   neverb-indent-levels) "+")
(setf (gethash "where-inside" neverb-indent-levels) "=")
(setf (gethash "where-close"  neverb-indent-levels) "-")

(defvar neverb-indent-symbols (make-hash-table :test 'equal))
(setf (gethash "="  neverb-indent-symbols)  0)
(setf (gethash ""   neverb-indent-symbols)  0)
(setf (gethash "+"  neverb-indent-symbols)  1)
(setf (gethash "-"  neverb-indent-symbols) -1)
(setf (gethash "++" neverb-indent-symbols)  2)
(setf (gethash "--" neverb-indent-symbols) -2)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GENERICS

(defun neverb-lookup-indent (key)
  (lexical-let ((sym nil)
				(num nil)
				(shiftwidth nil)
				(result nil))

	(setq shiftwidth (gethash "shiftwidth" neverb-indent-levels))
	(setq sym (gethash key neverb-indent-levels))
	(if (numberp sym)
		(setq result sym)
	  (progn
		(setq num (gethash sym neverb-indent-symbols))
		(if num
			(setq result (* shiftwidth num))
		  (case (string-to-char (substring sym 0 1))
			((?+ ?\-) (setq result (string-to-number sym)))))))
	  result))
  
(defun neverb-set-indent (key value) (setf (gethash key neverb-indent-levels) value))

(defun neverb-get-indent (key) (gethash key neverb-indent-levels))

(defun neverb-mod-indent (key value) (incf (gethash key neverb-indent-levels) value))

(defun neverb-is-empty-line ()
  (save-excursion
	(beginning-of-line)
	(looking-at "^[[:space:]]*\\(\\|--.*\\)$")))

(defun neverb-is-comment-start ()
  (save-excursion
	(beginning-of-line)
	(looking-at ".*{-.*")))

(defun neverb-is-indent-commented ()
  (save-excursion
	(beginning-of-line)
	(looking-at "[[:space:]]*{-.*")))


(defun neverb-is-comment-end ()
  (save-excursion
	(beginning-of-line)
	(looking-at ".*-}.*")))

(defun neverb-get-absolute-indent ()
  (save-excursion
	(lexical-let ((skip nil))
	  (goto-char 0)
	  (setq skip (and (neverb-is-comment-start) (neverb-is-indent-commented)))
	  (while (and (or skip (neverb-is-empty-line)) (not (eobp)))
		(forward-line 1)
		(if (and (neverb-is-comment-start) (neverb-is-indent-commented))
			(setq skip t))
		(if (neverb-is-comment-end)
			(progn
			  (forward-line 1)
			  (setq skip nil))))
	  (current-indentation))))

(defun neverb-reverse-all (l)
  (lexical-let ((result nil))
	(if (listp l) (setq result (mapcar 'neverb-reverse-all (reverse l)) l))
	(if (haskell-node-p l) 
		(progn
		  (setf (haskell-node-childs l) (reverse (haskell-node-childs l)))
		  (setq result l)))
	result))


(defun neverb-end-is-near ()
  (save-excursion
	(skip-chars-forward "[:space:]")
	(eobp)))


(defun neverb-scroll-to-root-node ()
  (lexical-let ((skip nil))
	(setq skip (neverb-is-comment-end))
	(while 
		(and (not (bobp)) 
			 (or 
			  skip 
			  (neverb-is-empty-line)
			  (/= (current-indentation) (neverb-get-indent "base"))))
	  (forward-line -1)
	  (if (neverb-is-comment-end) (setq skip t))
	  (if (neverb-is-comment-start)
		  (progn
			(setq skip nil)
			(if (neverb-is-indent-commented)
				(forward-line -1)))))
	(if (char-equal last-input-event ?\r) 
		(progn (forward-line -1)
			   (if (neverb-is-empty-line)
				   (forward-line 1))))))

(defun neverb-maybe-buffer-substring (len)
  (if (> (+ (point) len) (buffer-size)) (setq len (- (buffer-size) (point) -1)))
  (buffer-substring-no-properties (point) (+ (point) len)))

(defun neverb-take-while (set &optional skip)
  (lexical-let ((p nil) (pdiff nil) (word ""))
	(if skip (skip-chars-forward (concat "^" set)))
	(setq p (point))
	(setq pdiff (skip-chars-forward set))
	(setq word (buffer-substring p (+ p pdiff)))
	(if (string= "" word) nil word)))

(defun neverb-get-next-haskell-word ()
  (skip-chars-forward "[:space:]")
  (neverb-take-while "_a-z0-9A-Z'"))

(defun neverb-get-next-word ()
  (skip-chars-forward "[:space:]")
  (neverb-take-while "^[:space:]"))

(defun neverb-skip-string-forward (&rest set)
  (lexical-let ((skip t) (peek nil) (it 0))
	(while (and (not (eobp)) skip)
	  (loop for item in set do
			(setq peek (neverb-maybe-buffer-substring (length item)))
			(if (string= peek item)	(setq skip nil)))
	  (if (and (not (eobp)) skip) 
		  (progn 
			(incf it)
			(forward-char 1)))) it))

(defmacro neverb-take-drop (seq n)
  (list 'lexical-let '((accum nil))
		(list 'case n
			  (list 0 (list 'setq 'accum nil))
			  (list 1 (list 'progn
							(list 'setq 'accum (list 'car seq))
							(list 'setq seq (list 'cdr seq))))
			  (list 't (list 'progn 
							 (list 'setq 'accum (list 'subseq seq 0 n))
							 (list 'setq seq (list 'nthcdr n seq)))))
		'accum
		))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PREPROCESSORS

(defun neverb-preprocess-skip-quote (ch)
  "Skips quoted string literals"
  (lexical-let ((skip nil))
	(forward-char 1)
	(setq skip t)
	(while skip
	  (skip-chars-forward (concat "^" (char-to-string ch)))
	  (if (not (char-equal ?\\ (preceding-char)))
		  (setq skip nil)
		(skip-chars-forward (char-to-string ch))))))


(defun neverb-preprocess-skip-comment ()
  "Skips {- comments -}"
  (lexical-let ((skip nil))
	(setq skip t)
	(while skip
	  (skip-chars-forward "^}")
	  (if (char-equal (preceding-char) ?\-)
		  (setq skip nil))
	  (skip-chars-forward "}"))))

(defun neverb-preprocess (parsebuffer pbegin pend)
  "Strips string literals and comments on given offset"
  (with-current-buffer parsebuffer (goto-char 0))
  (save-excursion
	(lexical-let ((skip nil) (ch nil) (tch nil))
	  (goto-char pbegin)
	  (while (and (<= (point) pend) (not (eobp)))
		(setq ch (following-char))
		(setq tch (neverb-maybe-buffer-substring 2))
		(if (or (char-equal ch ?\") (char-equal ch ?\'))
			(progn
			  (setq skip t)
			  (neverb-preprocess-skip-quote ch)))
		(if (or (string= "--" tch) (neverb-is-empty-line))
			(progn
			  (setq skip t)
			  (end-of-line)))
		(if (string= "{-" tch)
			(progn
			  (setq skip t)
			  (neverb-preprocess-skip-comment)))
		(if skip (setq skip nil) (with-current-buffer parsebuffer (insert ch)))
		(if (not (eobp)) (forward-char 1))
		)))
  (with-current-buffer parsebuffer (goto-char 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PARSERS

(defun neverb-make-indent (name &optional special childs)
   (make-haskell-node 
	:name name 
	:line (line-number-at-pos) 
	:indent (current-indentation)
	:special special
	:childs childs
	))
(defmacro neverb-push-with-indent (dest name &optional special childs)
  (list 'push (list 'neverb-make-indent name special childs) dest))


(defun neverb-parse-group (cur)
  (lexical-let ((skip t)
				(ch nil)
				(co nil)
				(cc nil)
				(cs nil)
				(cn nil)
				(ce nil)
				(parse-result nil))
	(case cur
	  (?\{ 
	   (progn (setq co ?\{)
			  (setq cc ?\})
			  (setq cs ?\,)
			  (setq cn 'node-group-block)
			  (setq ce 'node-group-block-entry)))

	  (?\( 
	   (progn (setq co ?\()
			  (setq cc ?\))
			  (setq cs ?\,)
			  (setq cn 'node-group-tuple)
			  (setq ce 'node-group-tuple-entry)))
	  (?\[
	   (progn (setq co ?\[)
			  (setq cc ?\])
			  (setq cs ?\,)
			  (setq cn 'node-group-list)
			  (setq ce 'node-group-list-entry))))
	(if (not (neverb-end-is-near)) 
		(progn
		  (forward-char 1)
		  (setq parse-result (neverb-make-indent cn))
		  (skip-chars-forward "[:space:]")
		  (if (and 
			   (not (char-equal cc (following-char))) 
			   (not (char-equal co (following-char))))
			  (push (neverb-make-indent ce) (haskell-node-childs parse-result)))))
	(while skip
	  (setq ch (following-char))
	  (if (not (neverb-end-is-near))  (forward-char 1))

	  (if (and (not (char-equal ch cur))
			   (or (char-equal ch ?\()
				   (char-equal ch ?\{)
				   (char-equal ch ?\[)))
		  (push
		   (neverb-make-indent ce nil (neverb-parse-group ch))
		   (haskell-node-childs parse-result)))

	  (if (char-equal ch cs)
			(push (neverb-make-indent ce) (haskell-node-childs parse-result)))

	  (if (char-equal ch co)
			(push 
			 (neverb-make-indent ce nil (neverb-parse-group cur)) 
			 (haskell-node-childs parse-result)))

 	  (if (char-equal ch cc) (setq skip nil))
	  (if (neverb-end-is-near) (setq skip nil) (skip-chars-forward "[:space:]")))
	parse-result))

(defun neverb-parse-data ()
  (lexical-let (
				(parse-result nil) 
				(tmp nil)
				(skipped 0)
				(skip t))
	(neverb-push-with-indent parse-result 'node-data)
	(if (neverb-get-next-haskell-word)
		(neverb-push-with-indent parse-result 'node-data-name))
	(if (neverb-get-next-word)
		(neverb-push-with-indent parse-result 'node-data-eq-sign (- (current-column) 1)))
	(while (not (neverb-end-is-near))
	  (skip-chars-forward "[:space:]")
	  (case (following-char)
		((?\{ ?\| ?d) ()) 
		(t (if (neverb-get-next-haskell-word)
			   (progn
				 (neverb-push-with-indent parse-result 'node-data-variant)
				 (skip-chars-forward "[:space:]")
				 (if (> (neverb-skip-string-forward "{" "|" "deriving") 0)
					 (save-excursion
					   (skip-chars-backward "[:space:]")
					   (push
						(neverb-make-indent 'node-data-variant-content)
						(haskell-node-childs (first parse-result)))))))))
	  (case (following-char)
		(?\{ (push
			  (neverb-parse-group ?\{)
			  (haskell-node-childs (first parse-result))))
		(?| (skip-chars-forward "|"))
		(?d (progn
			  (neverb-get-next-haskell-word)
			  (skip-chars-forward "[:space:]")
			  (neverb-push-with-indent parse-result 'node-data-deriving)
			  (setq tmp (neverb-parse-group ?\())
			  (if tmp (push tmp (haskell-node-childs (first parse-result))))))
		(t (skip-chars-forward "="))))
	parse-result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INDENTATORS

(defun neverb-indentate-group (parsed-result)
  (lexical-let ((indent (neverb-get-indent "base")))
;	(print parsed-result)
;	(print indent)
	))



(defmacro neverb-maybe-indent (key item line pline rest mto &optional special)
  `(lexical-let ((keyvalue nil))
	   (if ,item
		   (if (> (haskell-node-line ,item) ,line)
			   (progn
				 (setq keyvalue (neverb-lookup-indent ,key))
				 (if (or (not keyvalue) ,special) (setq keyvalue ,special))
				 (if (and (char-equal ,last-input-event ?\r)
						  (not ,rest)
						  (not ,mto))
					 (neverb-set-indent "desired" keyvalue)
				   (neverb-set-indent "absolute" (+ ,pline keyvalue)))
				 (setq ,line (haskell-node-line ,item))
				 (setq ,pline (haskell-node-indent ,item))
             (setq ,mto nil))
			 (progn
			   (setq ,mto t)
			   (neverb-set-indent "desired" 0)
			   
			   (if ,special
				   (neverb-set-indent "absolute" ,special)
				 ))))))

(defun neverb-indentate-data (parsed-result)
  (lexical-let ((current-line 0)
				(more-then-one nil)
				(prev-line-indent 0)
				(node-data nil)
				(node-name nil)
				(node-eq-sign nil)
				(eq-sign-column nil))
	(setq node-data (neverb-take-drop parsed-result 1))
	(neverb-maybe-indent "data-open" 
						 node-data 
						 current-line 
						 prev-line-indent 
						 parsed-result 
						 more-then-one)

	(setq node-data-name (neverb-take-drop parsed-result 1))
	(neverb-maybe-indent "data-name" 
						 node-data-name 
						 current-line 
						 prev-line-indent 
						 parsed-result 
						 more-then-one)

	(setq node-data-eq-sign (neverb-take-drop parsed-result 1))   	
	(setq eq-sign-column (haskell-node-special node-data-eq-sign))
	(neverb-maybe-indent "data-eq-sign" 
						 node-data-eq-sign 
						 current-line 
						 prev-line-indent
						 parsed-result
						 more-then-one)
	
	(while parsed-result
	  (setq variant (neverb-take-drop parsed-result 1))
	  (neverb-maybe-indent "data-variant"
						   variant
						   current-line
						   prev-line-indent
						   parsed-result
						   more-then-one
						   eq-sign-column)
	  )

;	(if node-data 
;		(progn
;		  (neverb-set-indent "absolute" (haskell-node-indent node-data))
;		  (setq prev-line-indent (haskell-node-indent node-data))
;		  (setq current-line (haskell-node-line node-data))
;		  (neverb-set-indent "desired" (neverb-lookup-indent "data-open"))))



  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXTERNALS

(defun neverb-parse (pbegin pend)
  (save-excursion
	(lexical-let ((parsebuffer nil))
	  (setq parsebuffer (generate-new-buffer "haskell-parsing-buffer"))
	  (neverb-preprocess parsebuffer pbegin pend)
	  (set-buffer parsebuffer)
	  (goto-char 0)
	  (setq parse-result nil)
	  (skip-chars-forward "[:space:]")
	  (if (string= "data" (neverb-get-next-word))
		  (neverb-indentate-data (neverb-reverse-all (neverb-parse-data))))
	  (kill-buffer parsebuffer))))

(defun neverb-indent ()
  (interactive)

  (lexical-let ((pbegin 0) (pend 0) (lastline 0) (firstline 0))
	(save-excursion
	  
	  (setq lastline (line-number-at-pos))
	  (neverb-set-indent "base" (neverb-get-absolute-indent))
	  (neverb-set-indent "desired" 0)
	  (neverb-set-indent "absolute" 0)

	  (end-of-line)
	  (setq pend (point))
	  (neverb-scroll-to-root-node)
	  (beginning-of-line)
	  (setq pbegin (point))
;	  (print (thing-at-point 'line))
	  (setq firstline (line-number-at-pos))
	  (neverb-parse pbegin pend)
	  )
  (if (/= lastline firstline)
	  (progn
		(setq left-margin 
			  (+ (neverb-get-indent "absolute") (neverb-get-indent "desired")))
		(indent-to-left-margin)))
))

(defun turn-on-haskell-neverb-indent ()
  (set (make-local-variable 'indent-line-function) 'neverb-indent)
  (local-set-key (kbd "<f2>") 'neverb-is-empty-line)
  (run-hooks 'haskell-neverb-indent-hook))

(provide 'haskell-neverb-indent)