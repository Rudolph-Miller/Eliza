;;PAIP Chap.5

(defun simple-equal (x y)
  (if (or (atom x) (atom y))
	(eql x y)
	(and (simple-equal (car x) (car y))
		 (simple-equal (cdr x) (cdr y)))))

(defun pat-match (pattern input)
  (if (variable-p pattern)
	t
	(if (or (atom pattern ) (atom input))
	  (eql pattern input)
	  (and (pat-match (car pattern) (car input))
		   (pat-match (cdr pattern) (cdr input))))))

(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defconstant fail nil)
(defconstant no-bindings '((t . t)))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))

(defun lookup (var bindings)
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  (cons (cons var val) bindings))

(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond ((eq bindings fail) fail)
		((variable-p pattern)
		 (match-variable pattern input bindings))
		((eql pattern input) bindings)
		((and (consp pattern) (consp input))
		 (pat-match (cdr pattern) (cdr input)
					(pat-match (car pattern) (car input) bindings)))
		(t fail)))

(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
	(cond ((not binding) (extend-bindings var input bindings))
		  ((equal input (binding-val binding)) bindings)
		  (t fail))))


(defun extend-bindings (var val bindings)
  (cons (cons var val)
		(if (eq bindings no-bindings)
		  nil
		  bindings)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond ((eq bindings fail) fail)
		((variable-p pattern) 
		 (match-variable pattern input bindings))
		((eql pattern input) bindings)
		((segment-pattern-p pattern)
		 (segment-match pattern input bindings))
		((and (consp pattern) (consp input))
		 (pat-match (cdr pattern) (cdr input)
					(pat-match (car pattern) (car input) bindings)))
		(t fail)))

(defun segment-pattern-p (pattern)
  (and (consp pattern)
	   (starts-with (car pattern) '?*)))

(defun starts-with (lst symb)
  (if (consp lst)
	(eql (car lst) symb)
	(eql lst symb)))

(defun segment-match (pattern input bindings &optional (start 0))
  (let ((var (cadr (car pattern)))
		(pat (cdr pattern)))
	(if (null pat)
	  (match-variable var input bindings)
	  (let ((pos (position (car pat) input :start start :test #'equal)))
		(if (null pos)
		  fail
		  (let ((b2 (pat-match pat (subseq input pos) bindings)))
			(if (eq b2 fail)
			  (segment-match pattern input bindings (1+ pos))
			  (match-variable var (subseq input 0 pos) b2))))))))

(defun segment-match (pattern input bindings &optional (start 0))
  (let ((var (cadr (car pattern)))
		(pat (cdr pattern)))
	(if (null pat)
	  (match-variable var input bindings)
	  (let ((pos (position (car pat) input :start start :test #'equal)))
		(if (null pos)
		  fail
		  (let ((b2 (pat-match
					  pat (subseq input pos)
					  (match-variable var (subseq input 0 pos)
									  bindings))))
			(if (eq b2 fail)
			  (segment-match pattern input bindings (1+ pos))
			  b2)))))))

(defun rule-pattern (rule) (car rule))
(defun rule-responses (rule) (cdr rule))

(defparameter *eliza-rules*
  '((((?* ?x) hello (?* ?y))
	 (how do you do. Please state your problem.))
	(((?* ?x) i want (?* ?y))
	 (what would it mean if you got ?y)
	 (why do you want ?y))
	(((?* ?x) if (?* ?y))
	 (do you really think its likely that ?y)
	 (what do you think about ?y)
	 (really-- if ?y))
	(((?* ?x) no (?* ?y))
	 (why not?)
	 (you are being a bit negative)
	 (are you saying "NO" just to be negative?))
	(((?* ?x) i feel (?* ?y))
	 (do you often feel ?y ?))
	(((?* ?x) i felt (?* ?y))
	 (what other feelings do you have?))))

(defun eliza ()
  (loop
	(print 'eliza>)
	(write (flattern (use-eliza-rule (read))) :pretty t)))

(defun use-eliza-rule (input)
  (some #'(lambda (rule)
			(let ((result (pat-match (rule-pattern rule) input)))
			  (if (not (eq result fail))
				(sublis (switch-viewpoint result)
						(random-elt (rule-responses rule))))))
		*eliza-rules*))
(defun switch-viewpoint (words)
  (sublis '((i . you) (you . i) (me . you) (am . are))
		  words))

(defun random-elt (lst)
  (elt lst (random (length lst))))

(defun mappend (fn lst)
  (apply #'append (mapcar fn lst)))

(defun flattern (lst)
  (mappend #'mklist lst))

(defun mklist (x)
  (if (listp x)
	x
	(list x)))

(eliza)



