;;; haskell-neverb-indent.el --- Neverb's (itakingiteasy) indentation style
;;
;; Copytight (C) 2011 Alexander <itakingiteasy> Tumin
;;                      <itakingiteasy@neverb.net>
;;
;; Add folowing to your .emacs file:
;;

(require 'cl)

(defvar neverb-indent-levels (make-hash-table :test 'equal))
(clrhash neverb-indent-levels)
(setf (gethash "absolute"     neverb-indent-levels) "0")
(setf (gethash "desired"      neverb-indent-levels) "0")
(setf (gethash "base"         neverb-indent-levels) "0")
(setf (gethash "shiftwidth"   neverb-indent-levels) "2")

(setf (gethash "data-open"    neverb-indent-levels) "+")
(setf (gethash "data-name"    neverb-indent-levels) "+")
(setf (gethash "data-eq"      neverb-indent-levels) "+")
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


(defun neverb-lookup-relative-indent (key)
  (setq sym (gethash key neverb-indent-levels))
  (setq num (gethash sym neverb-indent-symbols))
  (setq shiftwidth (string-to-number (gethash "shiftwidth" neverb-indent-levels)))
  (if num
	  (setq result (* shiftwidth num))
	(let ((ch (string-to-char (substring sym 0 1))))
	  (case ch
		((?+ ?-) (setq result (string-to-number sym)))
		(t (setq result nil)))))
  result
)

(defun neverb-lookup-indent (key)
  (setq result (string-to-number (gethash "absolute" neverb-indent-levels)))
  (setq shiftwidth (string-to-number (gethash "shiftwidth" neverb-indent-levels)))
  (setq sym (gethash key neverb-indent-levels))
  (setq num (gethash sym neverb-indent-symbols))
  (if num
	  (setq result (+ result (* shiftwidth num)))
	(let ((ch (string-to-char (substring sym 0 1))))
	  (case ch
		((?+ ?-) (setq result (+ result (string-to-number sym))))
		(?& (setq result nil))
		(t (setq result (string-to-number sym)))))) result)

(defun neverb-is-empty-line ()
  (save-excursion
	(beginning-of-line)
	(skip-chars-forward "[:space:]")
	(if (or (>= (skip-chars-forward "-") 2) (eolp)) t nil)))

(defun neverb-is-comment-start ()
  (save-excursion
	(setq found nil)
	(let ((data (match-data)))
	  (set-match-data'nil)
	  (string-match "^[[:space:]]*{-" (thing-at-point 'line))
	  (if (match-beginning 0) (setq found t))
	  (set-match-data data)) found))

(defun neverb-is-comment-end ()
  (save-excursion
	(setq found nil)
	(let ((data (match-data)))
	  (set-match-data'nil)
	  (string-match "-}" (thing-at-point 'line))
	  (if (match-beginning 0) (setq found t))
	  (set-match-data data)) found))

(defun neverb-get-absolute-indent ()
  (save-excursion
	(goto-char 0)
	(setq insidecomment nil)
	(setq skip (if (or (neverb-is-empty-line) (neverb-is-comment-start)) t nil))
	(while (and skip (not (eobp)))
	  (if (neverb-is-comment-start) (setq insidecomment t))
	  (if (neverb-is-comment-end) (progn (setq insidecomment nil) (forward-line 1)))
	  (setq skip (or insidecomment (neverb-is-empty-line)))
	  (if skip (forward-line 1)))
	(current-indentation)))



(defun neverb-lookup-string (iitem iseq)
  (find iitem iseq :test 'equal))

(defun neverb-is-root-node ()
  (let ((thing (thing-at-point 'line)))
	(and 
	 (= (current-indentation) (neverb-lookup-indent "base"))
	 (not (neverb-is-empty-line)))))

(defun neverb-scroll-to-root-node ()
  (setq insidecomment nil)
  (setq skip (if 
				 (or 
				  (neverb-is-empty-line) 
				  (neverb-is-comment-end)
				  (not (neverb-is-root-node))
				  ) t nil))
  (while (and skip (not (bobp)))
	  (if (neverb-is-comment-end) (setq insidecomment t))
	  (if (neverb-is-comment-start) (progn (setq insidecomment nil) (forward-line -1)))
	  (setq skip (or insidecomment (not (neverb-is-root-node)) (neverb-is-empty-line)))
	  (if skip (forward-line -1))))

(defun neverb-safe-read-from-buffer (peek)
  (if (> (+ (point) peek) (buffer-size)) (setq peek (- (buffer-size) (point) -1)))
  (buffer-substring-no-properties (point) (+ (point) peek)))


(defun neverb-skip-quote (ch ep)
  (setq ntch (neverb-safe-read-from-buffer 2))  
  (if (not (string= ntch "\"\""))
	  (progn
		(forward-char 1)
		(while (and 
				(<= (point) ep) 
				(not (eobp))
				(not (char-equal ch (following-char))))
		  (if (string= (neverb-safe-read-from-buffer 2) "\\\"")
			  (forward-char 1))
		  (forward-char 1)))
	(forward-char 1)))

(defun neverb-skip-comment (ep)
  (setq ntch (neverb-safe-read-from-buffer 2))
  (while (and (<= (point) ep) (not (string= ntch "-}")))
	(setq ntch (neverb-safe-read-from-buffer 2))
	(forward-char 1)))

(defun neverb-preparse (buf bp ep)
  (with-current-buffer buf (goto-char 0))
  (save-excursion
	(goto-char bp)
	(setq skip nil)
	(while (and (<= (point) ep) (not (eobp)))
	  (setq nch (following-char))

	  (setq ntch (neverb-safe-read-from-buffer 2))

	  (if (or (char-equal nch ?\") (char-equal nch ?\'))
		  (progn
			(setq skip t)
			(neverb-skip-quote nch ep)))

	  (if (string= "--" ntch)
		  (progn
			(setq skip t)
			(end-of-line)))

	  (if (string= "{-" ntch)
		  (progn
			(setq skip t)
			(neverb-skip-comment ep)))
	  (if (neverb-is-empty-line)
		  (setq skip t)
		  )
		  
	  (if skip (setq skip nil) (with-current-buffer buf (insert nch)))
	  (forward-char 1)))
  (with-current-buffer buf (goto-char 0)))

(defun neverb-take-while (rexp &optional skip)
  (if skip
	  (skip-chars-forward (concat "^" rexp))
  )
  (setq p (point))
  (setq pdiff (skip-chars-forward rexp))
  (setq word (buffer-substring p (+ p pdiff)))
  (if (string= "" word) nil word))

(defun neverb-get-next-haskell-word (&optional skip)
  (skip-chars-forward "[:space:]")
  (neverb-take-while "_a-z0-9A-Z'" skip))


;; (defun neverb-parse-haskell-untill-pipe ()
;;   (setq w nil)
;;   (while (and (not (char-equal ?| (following-char))) (not (eobp)))
;; 	(push (following-char) w)
;; 	(forward-char 1)
;; 	)
;;   (if w
;; 	  (concat (reverse w))
;; 	nil
;; 	)
;; )

;; (defun neverb-parse-haskell-block ()
  
;; )

;; (defun neverb-parse-haskell-node-data ()
;;   (setq data-result nil)
;;   (push 'haskell-node-data data-result)
;;   (push (neverb-get-next-haskell-word t) data-result)
;;   (skip-chars-forward "[:space:]=")
;;   (while (not (eobp))
;; 	(let ((word (neverb-get-next-haskell-word t)))
;; 	  (if word 
;; 		  (progn
;; 			(setq tmp nil)
;; 			(push 'haskell-node-data-values tmp)
;; 			(push word tmp)
;; 			(skip-chars-forward "[:space:]")
;; 			(if (char-equal ?{ (following-char))
;; 				(push (neverb-parse-haskell-block) tmp)
;; 			  (push (neverb-parse-haskell-untill-pipe) tmp)
;; 				)
;; 			(push tmp data-result)
;; 			)
;; 		  )
;; 	  )
;; 	)
;;   data-result
;; )

(defun neverb-reverse-all (l)
  (if (listp l) (mapcar 'neverb-reverse-all (reverse l)) l))

(defun neverb-add-absolute-indent (value)
  (setf 
   (gethash "absolute" neverb-indent-levels) 
   (number-to-string 
	(+ (string-to-number (gethash "absolute" neverb-indent-levels)) value))))

(defun neverb-set-absolute-indent (value)
  (setf (gethash "absolute" neverb-indent-levels) (number-to-string value)))

(defun neverb-mod-absolute-indent (key)
  (setq ivalue (neverb-lookup-indent key))
  (if ivalue (neverb-set-absolute-indent (neverb-lookup-indent key)) nil))

(defun neverb-end-is-near ()
  (skip-chars-forward "[:space:]")
  (eobp))

;; (defmacro neverb-maybe-indent (key &optional a)
;;   (if (and cont-ok (neverb-end-is-near))
;; 	  (progn
;; 		(setq cont-ok nil)
;; 		(if a
;; 			(neverb-set-absolute-indent (eval a))
;; 		  (save-excursion
;; 			(forward-line -2) ; -1 for current; -1 for end-is-near
;; 			(setq oldmod (current-indentation))
;; 			(setq diff (neverb-lookup-relative-indent key))
;; 			(print diff)
;; 			(neverb-set-absolute-indent (+ oldmod diff)))))
;; 	(if cont-ok (neverb-set-absolute-indent (current-indentation)))))

;; (defun neverb-parse-haskell-node-block ()
;;   (setq block-offset (current-column))
;;   (forward-char 1)
;;   (setq doforward t)
;;   (while (and doforward (not (neverb-end-is-near)))
;; 	(skip-chars-forward "^}{\n")
;; 	(setq ch (following-char))
;; 	(forward-char 1)
;; 	(case ch
;; 	  (?\n (neverb-set-absolute-indent block-offset))
;; 	  (?{  (neverb-parse-haskell-node-block))
;; 	  (?}  (setq doforward nil)))))

;; (defun neverb-parse-haskell-node-data ()
;;   (setq cont-ok t)
;;   (neverb-maybe-indent "data-open")
;;   (neverb-get-next-haskell-word t)
;;   (neverb-maybe-indent "data-name")
;;   (skip-chars-forward "^=")
;;   (setq eq-column nil)
;;   (if (char-equal (following-char) ?=)
;; 	  (progn
;; 		(setq eq-column (current-column))
;; 		(skip-chars-forward "[:space:]=")
;; 		(neverb-maybe-indent "data-eq")))
;;   (while (not (neverb-end-is-near))
;;   	(if (and eq-column (not (neverb-lookup-indent "data-variant")))
;;    		(neverb-set-absolute-indent eq-column))
;;    	(skip-chars-forward "^{|")
;;    	(if (char-equal ?{ (following-char))
;;    		  (neverb-parse-haskell-node-block))
;; 	(skip-chars-forward "^|")
;; 	(skip-chars-forward "|")))

(defun neverb-get-next-word ()
  (skip-chars-forward "[:space:]")
  (neverb-take-while "^[:space:]")
)


(make-symbol "node-data")
(make-symbol "node-data-name")
(make-symbol "node-data-eq")
(make-symbol "node-data-variant")
(make-symbol "node-data-variant-content")
(make-symbol "node-data-deriving")

(defun neverb-combine-indent (what indent &optional special)
  (setq tmp nil)
  (push what tmp)
  (push indent tmp)
  (push special tmp)
  tmp
)

(defmacro neverb-push-with-indent (what &optional seq special absolute)
   (if (not (eval absolute)) (setq absolute (current-indentation)))
   (list 'setq seq (list 'cons (list 'neverb-combine-indent what absolute special) seq)))

(make-symbol "node-block")
(make-symbol "node-block-entry")
(make-symbol "node-tuple")
(make-symbol "node-tuple-entry")
(make-symbol "node-list")
(make-symbol "node-list-entry")

(defun neverb-parse-haskell-node-block (&optional cur recurse)
  (lexical-let ((cont-ok t)
				(co ?\{)
				(cc ?\})
				(cs ?\,)
				(cn 'node-block)
				(ce 'node-block-entry)
				(block-parse-results nil))
	(case cur 
	  (?\{ ())
	  (?\( 
	   (progn
		 (setq co ?\()
		 (setq cc ?\))
		 (setq cs ?\,)
		 (setq cn 'node-tuple)
		 (setq ce 'node-tuple-entry)
		 )
	   )
	  (?\[
	   (progn
		 (setq co ?\[)
		 (setq cc ?\])
		 (setq cs ?\,)
		 (setq cn 'node-list)
		 (setq ce 'node-list-entry)
		 )
	   )
	  )
	(setq block-parse-results  (neverb-combine-indent cn (current-indentation)))
	(forward-char 1)
	(skip-chars-forward "[:space:]")
 	(if (not (char-equal co (following-char)))
 		(neverb-push-with-indent ce block-parse-results))
	(while cont-ok
	  (setq ch (following-char))
	  (forward-char 1)
	  (if (char-equal ch cs)
		  (progn
			(skip-chars-forward "[:space:]")
			(neverb-push-with-indent ce block-parse-results)))
	  (if (char-equal ch co)
		  (progn
			(backward-char 1)
			(neverb-push-with-indent
			 ce
			 block-parse-results
			 (neverb-parse-haskell-node-block cur t))
			(forward-char 1)))
	  (if (char-equal ch cc)
			(setq cont-ok nil))
	  (if (neverb-end-is-near) 
			(setq cont-ok nil)))
	block-parse-results))



(defun neverb-parse-haskell-node-data ()

  (setq parse-result (neverb-combine-indent 'node-data (current-indentation)))
  (neverb-get-next-haskell-word)
  (neverb-push-with-indent 'node-data-name parse-result)
  (neverb-get-next-word)
  (neverb-push-with-indent 'node-data-eq-sign parse-result (current-column))
  (while (not (neverb-end-is-near))
	(skip-chars-forward "[:space:]")
	(if (string= "deriving" (neverb-get-next-haskell-word))
		(progn
		  (skip-chars-forward "[:space:]")
		  (setq parse-tmp-cont (neverb-parse-haskell-node-block ?\())
		  (neverb-push-with-indent 'node-data-deriving parse-result parse-tmp-cont)
		  )
	  (progn
		(setq parse-tmp nil)
		(setq parse-tmp-cont nil)
		(setq node-data-variant-content (neverb-take-while "^{|d"))
		(if (char-equal ?{ (following-char))
			(progn
			  (setq parse-tmp-cont (neverb-parse-haskell-node-block ?{))
			  (skip-chars-forward "^|d")))
;		(print node-data-variant-content)
		(neverb-push-with-indent 'node-data-variant parse-result parse-tmp-cont)
		(skip-chars-forward "|"))))
  (print (neverb-reverse-all parse-result))
  parse-result)

(defun neverb-parse (foobegin fooend)
  (setq parsebuffer (generate-new-buffer "haskell-parsing-buffer"))
  (save-excursion (neverb-preparse parsebuffer foobegin fooend))
  (set-buffer parsebuffer)
  (save-excursion
	(goto-char 0)
	(if (string= "data" (neverb-get-next-word))
		(neverb-parse-haskell-node-data))
	)
  (kill-buffer parsebuffer)
)

(defun neverb-indent ()
  (interactive)
  (neverb-set-absolute-indent 0)
;  (print (neverb-lookup-indent "data-variant"))
  (save-excursion
	(setq currline (thing-at-point 'line))
	(setf 
	 (gethash "base" neverb-indent-levels) 
	 (number-to-string (neverb-get-absolute-indent)))
	(neverb-mod-absolute-indent "base")
	
	(end-of-line)
	(setq fooend (point))
	(neverb-scroll-to-root-node)
	(beginning-of-line)
	(setq foobegin (point))
	
	(setq firstline (thing-at-point 'line))
	(neverb-parse foobegin fooend)

	)
;  (print (gethash "absolute" neverb-indent-levels))
  (if (not (string= currline firstline))
	  (progn
;		(setq left-margin (string-to-number (gethash "absolute" neverb-indent-levels)))
;		(indent-to-left-margin)
		)
	)
  )


(defun turn-on-haskell-neverb-indent ()
  (set (make-local-variable 'indent-line-function) 'neverb-indent)
  (run-hooks 'haskell-neverb-indent-hook))

(provide 'haskell-neverb-indent)