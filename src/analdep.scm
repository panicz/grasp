#!/bin/sh
#|
exec java -jar ../libs/kawa.jar --no-warn-unreachable -f "$0" $*
|#
(import (kawa regex))
(import (only (srfi :1) filter-map))
(import (language define-syntax-rule))
(import (language define-interface))
(import (utils shell))
(import (utils functions))
(import (language infix))
(import (language match))
(import (utils hash-table))
(import (language mapping))
(import (language for))

(define-syntax-rule (print elements ...)
  (display elements)
  ...
  (display #\newline))

(define (matching string pattern)
  (regex-match pattern string))

(define (reach #;of graph #;from node)
  (define (walk #;from novel #;into visited)
    (let* ((vicinity (fold-left union '() (map graph novel)))
	   (visited (union visited novel))
	   (novel (difference vicinity visited)))
      (if (null? novel)
	  visited
	  (walk #;from novel #;into visited))))
  (walk #;from (graph node) #;into '()))

(define (all-scm-files-from-current-directory)
  (only (is _ matching "\\.scm$") (string-split (shell "ls") "\n")))

(define (all-scm-files-from-subdirectories)
  (filter-map (lambda (name)
		(and (is (length name) > 2)
		     (none (is _ eq? #\#) name)
		     (string-drop name 2)))
	      (string-split (shell "find . -name *.scm") "\n")))

(define list? ::boolean #f)

(define layers? ::boolean #f)

(define dump? ::boolean #f)

(define files
  (match (command-line)
    (`(,command)
     (all-scm-files-from-subdirectories))

    (`(,command "--" "--list" . ,args)
     (set! list? #t)
     args)
    
    (`(,command "--" "--layers" . ,args)
     (set! layers? #t)
     (if (null? args)
	 (all-scm-files-from-subdirectories)
	 args))

    (`(,command "--" "--dump" . ,args)
     (set! dump? #t)
     (if (null? args)
	 (all-scm-files-from-subdirectories)
	 args))
    
    (`(,command "--" . ,args)
     args)))

(define (module-name file)
  (map string->symbol
       (string-split (string-take file
				  (- (length file) 4))
		     "/")))
  
(define (dependency-graph files)
  (let ((dependencies (mapping (module) '())))
    (for file in files
      (let* ((contents (with-input-from-file file read-all))
	     (imports (apply
		       append
		       (map (lambda (expression)
			      (match expression
				(`(import . ,modules)
				 (map (lambda (module-spec)
					(match module-spec
					  (`(rename ,module . ,_)
					   module)
					  (_
					   module-spec)))
				      modules))
				(_
				 '()))) contents)))
	     (source-module (module-name file)))
	(set! (dependencies source-module) imports)))
    dependencies))

(define files-dependency-graph
  (dependency-graph
   (all-scm-files-from-subdirectories)))

(for module in (map module-name files)
  (let ((dependencies (reach files-dependency-graph module)))
    (when (is module in dependencies)
      (print "circular dependency from "module": "dependencies))))

(define (system-module? x)
  (match x
    (`(srfi . ,_)
     #t)
    (`(kawa . ,_)
     #t)
    (`(tools ,_)
     #t)
    (_
     #f)))

(define (module-file module)
  (string-append (string-join (map symbol->string module) "/") ".scm"))

(when list?
  (let ((dependencies '()))
    (for module in (map module-name files)
	 (set! dependencies (union dependencies
				   (only (isnt _ system-module?)
					 (reach files-dependency-graph
						module)))))
    (for module in dependencies
	 (display (module-file module))
	 (display " "))))

(when dump?
  (for module in (keys files-dependency-graph)
    (print module ": "(files-dependency-graph module))))

(define (graph-layers graph)
  (let loop ((modules (keys graph))
	     (layers '())
	     (allowed-dependencies '()))
    (let-values (((layer modules)
		  (partition (is (only (isnt _ system-module?)
				       (graph _)) subset?
				 allowed-dependencies)
			     modules)))
      (cond
       ((null? modules)
	`(,layer . ,layers))
       ((null? layer)
	(print "empty layer with remaining modules")
	`(,modules . ,layers))
       (else
	(loop modules
	      `(,layer . ,layers)
	      `(,@layer ,@allowed-dependencies)))))))

(when layers?
  (let ((layers (graph-layers files-dependency-graph)))
    (for layer in layers
      (print layer)
      (newline))))

(exit)
