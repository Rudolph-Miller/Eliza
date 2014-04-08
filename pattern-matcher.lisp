(defun eliza ()
  (loop
	(print 'eliza>)
	(print (flatten (use-eliza-rules (read))))))

(defun interactive-interpreter (prompt transformer)
  (loop
	(print prompt)
	(print (funcall transformer (read)))))


(defun compose (f g)
  #'(lambda (x) (funcall f (funcall g (x)))))

(defun eliza ()
  (interactive-interpreter 'eliza>
						   (compose #'flatten #'use-eliza-rules)))

(defun interactive-interpreter (prompt transformer)
  (loop
	(handler-case
	  (progn 
		(if (stringp prompt)
		  (print prompt)
		  (funcall prompt))
		(print (funcall transformer (read)))))
	(error (condition)
		   (format t "~&;; Error ~a ignored, back to top level." condition))))

(defun prompt-generator (&optional (num 0) (ctl-string "[~d] "))
  #'(lambda () (format t ctl-string (incf num))))

(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond ((eq bindings fail) fail)
		((variable-p pattern)
		 (match-variable pattern input bindings))
		((eql pattern input) bindings)
		((segment-pattern-p pattern)
		 (segment-matcher pattern input bindings))
		((single-pattern-p pattern)
		 (single-matcher pattern input bindings))
		((and (consp pattern) (consp input))
		 (pat-match (cdr pattern) (cdr input) (pat-match (car pattern) (car input) bindings)))
		(t fail)))

(defconstant fail nil)
(defconstant no-bindings '((t . t)))

(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun get-bindings (var bindings)
  (assoc var bindings))

(defun binding-var (binding)
  (car binding))

(defun binding-val (binding)
  (cdr binding))

(defun make-binding (var val)
  (cons var val))

(defun lookup (var vaindings)
  (binding-val (get-bindings var bindings)))

(defun extend-bindings (var val bindings)
  (cons (make-binding var val)
		(if (eq bindings no-bindings)
		  nil
		  bindings)))

(defun match-variable (var input bindings)
  (let ((binding (get-bindings var bindings)))
	(cond ((not binding) (extend-bindings var input bindings))
		  ((equal input (binding-val binding)) bindings)
		  (t fail))))

(setf (get '?is 'single-match) 'match-is)
(setf (get '?or 'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(setf (get '?* 'segment-match) 'segment-match)
(setf (get '?+ 'segment-match) 'segment-match+)
(setf (get '?? 'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

(defun segment-pattern-p (pattern)
  (and (consp pattern) (consp (car pattern))
	   (symbolp (caar pattern))
	   (segment-match-fn (caar pattern))))

(defun single-pattern-p (pattern)
  (and (consp pattern)
	   (single-match-fn (car pattern))))

(defun segment-matcher (pattern input bindings)
  (funcall (segment-match-fn (caar pattern)) pattern input bindings))

(defun single-matcher (pattern input bindings)
  (funcall (single-match-fn (car pattern)) (cdr pattern) input bindings))

(defun segment-match-fn (x)
  (when (symbolp x) (get x 'segment-match)))

(defun single-match-fn (x)
  (when (symbolp x) (get x 'single-match)))

(defun match-is (var-and-pred input bindings)
  (let* ((var (car var-and-pred))
		 (pred (cadr var-and-pred))
		 (new-bindings (pat-match var input bindings)))
	(if (or (eq new-bindings fail)
			(not (funcall pred input)))
	  fail
	  new-bindings)))

(defun match-and (patterns input bindings)
  (cond ((eq bindings fail) fail)
		((null patterns) bindings)
		(t (match-and (cdr patterns) input
					  (pat-match (car patterns) input bindings)))))

(defun match-or (patterns input bindings)
  (if (null patterns)
	fail
	(let ((new-bindings (pat-match (car patterns) input bindings)))
	  (if (eq new-bindings fail)
		(match-or (cdr patterns) input bindings)
		new-bindings))))

(defun match-not (patterns input bindings)
  (if (match-or patterns input bindings)
	fail
	bindings))

(defun segment-match (pattern input bindings &optional (start 0))
  (let ((var (cadar pattern))
		(pat (cdr pattern)))
	(if (null pat)
	  (match-variable vat input bindings)
	  (let ((pos (first-match-pos (car pat) input start)))
		(if (null pos)
		  fail
		  (let ((b2 (pat-match pat (subseq input pos) (match-variable var (subseq input 0 pos) bindings))))
			(if (eq b2 fail)
			  (segment-match pattern input bindings (1+ pos))
			  b2)))))))

(defun first-match-pos (pat1 input start)
  (cond ((and (atom pat1) (not (variable-p pat1)))
		 (positino pat1 input :start start :test #'equal))
		((<= start (length input)) start)
		(t nil)))

(defun segment-match+ (pattern input bindings)
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  (let ((var (cadar pattern))
		(pat (cdr pattern)))
	(or (pat-match (cons var pat) input bindings)
		(pat-match pat input bindings))))

(defun match-if (pattern input bindings)
  (and (progv (mapcar #'car bindings)
		 (mapcar #'cdr bindings)
		   (eval (cadar pattern)))
	   (pat-match (cdr pattern) input bindings)))




;(print (pat-match '(?x > ?y (?if (> ?x ?y))) '(5 > 4)))


;(print (pat-match '(?x  ?op ?y is ?z (?if (eql (?op ?x ?y) ?z)))
				  ;'(3 + 4 is 7)))
