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

(define the-clip #!null)

(define-interface Clipboard ()
  (upload! new-content ::pair)::void
  (content)::list)

(define-object (SystemClipboardMissing)::Clipboard

  (define items ::list '())
  
  (define (upload! new-content ::pair)::void
    (set! items new-content))
  
  (define (content)::list
    (copy items)))

(define-parameter (the-system-clipboard)::Clipboard
  (SystemClipboardMissing))

(define/kw (cut-selection! at: cursor ::Cursor := (the-cursor)
			   to: range ::integer := (the-selection-range)
			   in: document := (the-document))
  ::void
  (and-let* ((clipboard ::Clipboard (the-system-clipboard))
	     (core ::Cursor (cursor-core cursor document))
	     (`(,top . ,root) core)
	     (parent ::cons (cursor-ref document root))
	     (target ::Indexable (parent:part-at top))
	     (first-index ::Index (target:first-index))
	     (last-index ::Index (target:last-index))
	     (preceding-cursor (cursor-retreat (recons
						first-index
						core)))
	     (content (drop (quotient top 2) parent))
	     (selection-start selection-end
			      (selection-start+end cursor range))
	     (`(,start . ,start-stem) selection-start)
	     (`(,end . ,end-stem) selection-end))
    (set! (the-selection-range) 0)
    (cond
     ((= range 0)
      ;; the target will be cut off from the rest
      ;; of the document after performing Remove
      (perform&record!
       (Remove element: content
	       at: core
	       with-shift: (car preceding-cursor)))
      (clipboard:upload! content))
	  ;; czy sa w tym samym obiekcie tekstowym?
     ((isnt start-stem equal? end-stem)
      (WARN "cutting selections spanning multiple objects\
 is currently not supported"))
     
     ((and (equal? first-index start)
	   (equal? last-index end))
      (record&perform!
       (Remove element: content
	       at: core
	       with-shift: (car preceding-cursor)))
      (clipboard:upload! content))

     (else
      (let ((selection ::string (make-string (- end start)))
	    (source ::Textual (as Textual target)))
	(for i from start below end
	     (string-set! selection (- i start)
			  (source:char-ref i)))
	(record&perform!
	 (RemoveCharacter list: selection
			  before: selection-end))
	(clipboard:upload! (cons selection '())))))))

(define/kw (copy-selection! at: cursor ::Cursor := (the-cursor)
			    to: range ::integer := (the-selection-range)
			    in: document := (the-document))
  ::void
  (and-let* ((clipboard ::Clipboard (the-system-clipboard))
	     (core ::Cursor (cursor-core cursor document))
	     (expression (the-expression))
	     (selection-start selection-end
			      (selection-start+end cursor range))
	     (`(,start . ,start-stem) selection-start)
	     (`(,end . ,end-stem) selection-end))
    (cond
     ((= range 0)
      (clipboard:upload! (cons expression (empty))))

     ((equal? start-stem end-stem)
      (let ((selection ::string (make-string (- end start)))
	    (source ::Textual (as Textual expression)))
	(for i from start below end
	     (string-set! selection (- i start)
			  (source:char-ref i)))
	(clipboard:upload! (cons selection '()))))

     (else
      (WARN "copying selections spanning multiple objects\
 is currently not supported")))))
     
(define/kw (paste-selection! into: document := (the-document)
			     at: cursor ::Cursor := (the-cursor)
			     to: range ::integer := (the-selection-range))
  ::void
  (and-let* ((clipboard ::Clipboard (the-system-clipboard))
	     ;; moze trzeba tutaj zrobic kopie:
	     (content ::list (clipboard:content))
	     ((pair? content)))
    (if (= range 0)
	(let ((target (cursor-ref document cursor)))
	  (cond
	   ((Space? target)
	    (record&perform!
	     (Insert element: content
		     at: cursor))
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
	    (WARN "unhandled target: "(target:getClass) target)))))))

