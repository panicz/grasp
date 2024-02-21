(module-name (editor document copy-paste))

(import (srfi :11))

(import (language assert))
(import (language define-object))
(import (language define-type))
(import (language define-interface))
(import (language define-parameter))
(import (language keyword-arguments))
(import (language fundamental))
(import (language infix))
(import (language match))
(import (language for))

(import (utils functions))
(import (utils print))
(import (utils string-building))

(import (editor interfaces elements))
(import (editor types spaces))
(import (editor document cursor))
(import (editor types primitive))
(import (editor document document-operations))
(import (editor document editor-operations))
(import (editor document history-tracking))
(import (editor input evaluation)) ;; for grasp

(define the-clip #!null)

(define-interface Clipboard ()
  (upload! new-content ::list)::boolean
  (content)::list)

(define-object (SystemClipboardMissing)::Clipboard

  (define items ::list '())
  
  (define (upload! new-content ::list)::boolean
    (set! items new-content)
    #t)
  
  (define (content)::list
    items))

(define-parameter (the-system-clipboard)::Clipboard
  (SystemClipboardMissing))

;; To oczywiscie bedzie trzeba dopicowac
(define (the-selection-start)
  (the-cursor))

(define (the-selection-end)
  (the-cursor))

(define/kw (cut-selection! at: cursor ::Cursor := (the-cursor)
			   to: range ::integer := (the-selection-range)
			   in: document := (the-document))
  ::boolean
  (let ((clipboard ::Clipboard (the-system-clipboard)))
    (if (= range 0)
	(and-let* ((core ::Cursor (cursor-core cursor document))
		   (`(,top . ,root) core)
		   (parent ::cons (cursor-ref document root))
		   (target ::Indexable (parent:part-at top))
		   (first-index ::Index (target:first-index))
		   (preceding-cursor (cursor-retreat (recons
						      first-index
						      core)))
		   (content (drop (quotient top 2) parent)))
	  ;; the target will be cut off from the rest
	  ;; of the document after performing Remove
	  (perform&record!
	   (Remove element: content
		   at: core
		   with-shift: (car preceding-cursor)))
	  (clipboard:upload! content))
	(let-values (((selection-start selection-end)
		      (selection-start+end cursor range)))
	  ;; czy sa w tym samym obiekcie tekstowym?
	  (or (and-let* ((`(,start . ,stem) selection-start)
			 (`(,end . ,,stem) selection-end))
		#t)
	      #f)))))

(define/kw (copy-selection! at: cursor ::Cursor := (the-cursor)
			    to: range ::integer := (the-selection-range)
			     in: document := (the-document))`
  ::boolean
  (let ((clipboard ::Clipboard (the-system-clipboard)))
    (if (= range 0)
	(and-let* ((core ::Cursor (cursor-core cursor document))
		   (expression (the-expression)))
	  (clipboard:upload! (cons expression (empty))))
	#f)))

(define/kw (paste-selection! into: document := (the-document)
			     at: cursor ::Cursor := (the-cursor)
			     to: range ::integer := (the-selection-range))
  ::boolean
  (and-let* ((clipboard ::Clipboard (the-system-clipboard))
	     ;; moze trzeba tutaj zrobic kopie:
	     (content ::list (clipboard:content))
	     ((pair? content))
	     (content ::list (copy content)))
    (if (= range 0)
	(let ((target (cursor-ref document cursor)))
	  (cond
	   ((Space? target)
	    (record&perform!
	     (Insert element: content
		     at: cursor
		     in: document))
	    )
	   #;((Atom? target)
	    ;; rozszczepiamy atom?
	    ...)
	   ((Textual? target)
	    (let ((insertions (cons
			       (InsertCharacter
				list: (show->string (car content))
				after: cursor)
			       '()))
		  (tip (car cursor))
		  (stem (cdr cursor)))
	      (let loop ((growth-cone insertions)
			 (content (cdr content)))
		(when (pair? content)
		  (let ((last ::InsertCharacter (car growth-cone))
			(item (car content)))
		    (set! tip (+ tip (length last:list)))
		    (set! (cdr growth-cone)
			  (cons
			   (InsertCharacter
			    list: (show->string item)
			    after: (recons tip stem))
			   '()))
		    (loop (cdr growth-cone)
			  (cdr content)))))
	      (record&perform!
	       (EditSequence operations: insertions))))
	   (else
	    (WARN "unhandled target: "(target:getClass) target)
	    #f)))
	#f)))

