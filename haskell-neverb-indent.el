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
		(setq l (forward-line -1))))


(defun haskell-neverb-indent ()
  (interactive)
;  (neverb-lookup-update-mapper "base" "99")
;  (print (neverb-lookup-translate "base"))
  (save-excursion
	(haskell-neverb-indent-scroll-to-root)
;	(message (thing-at-point 'line))
	))


(defun turn-on-haskell-neverb-indent ()
  (set (make-local-variable 'indent-line-function) 'haskell-neverb-indent)
;  (local-set-key (kbd "]") 'haskell-neverb-indent-parens)
;  (local-set-key (kbd "}") 'haskell-neverb-indent-parens)
;  (local-set-key (kbd ")") 'haskell-neverb-indent-parens)
  (local-set-key (kbd "<f2>") 'haskell-neverb-indent)
  (run-hooks 'haskell-neverb-indent-hook))

(provide 'haskell-neverb-indent)







