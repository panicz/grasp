#!/bin/sh
#|
mkdir -p build/cache

exec java -jar "libs/kawa.jar" -Dkawa.import.path="|:.:build/cache:src" \
  --no-warn-unreachable --no-warn-unknown-member -f "$0"
|#

(import (kawa regex))
(import (only (srfi :1) filter-map))
(import (language define-syntax-rule))
(import (language assert))
(import (language define-interface))
(import (language define-type))
(import (language define-property))
(import (language keyword-arguments))
(import (language define-object))
(import (language curry))
(import (utils functions))
(import (language infix))
(import (language match))
(import (utils hash-table))
(import (language mapping))
(import (language for))
(import (language define-cache))

(define-syntax-rule (print elements ...)
  (display elements)
  ...
  (display #\newline))

(define (reach #;of graph #;from node)
  (define (walk #;from novel #;into visited)
    (let* ((vicinity (fold-left union '() (map graph novel)))
	   (visited (union visited novel))
	   (novel (difference vicinity visited)))
      (if (null? novel)
	  visited
	  (walk #;from novel #;into visited))))
  (walk #;from (graph node) #;into '()))

(define (files #!key (from ::(either string java.io.File) ".")
	       (matching ".*"))::(list-of java.io.File)
  (let ((directory ::java.io.File
		   (if (string? from)
		       (java.io.File (as String from))
		       from))
	(result '()))
    (assert (directory:isDirectory))
    (for file ::java.io.File in (directory:listFiles)
      (cond
       ((file:isDirectory)
	(set! result `(,@(files from: file matching: matching) ,@result)))
       ((and (file:isFile) (regex-match matching (file:getPath)))
	(set! result `(,file . ,result)))))
    result))

(define (module-name file ::java.io.File)::(list-of symbol)
  (match (regex-match "^(?:./)?src/([^.]*)[.]scm$" (file:getPath))
    (`(,_ ,core)
     (map string->symbol (string-split core "/")))))

(define (module-file module-name ::(list-of symbol))::java.io.File
  (java.io.File
   (as String
       (string-append "src/" (string-join module-name "/") ".scm"))))

(define source-files ::(list-of java.io.File)
  (files from: "src" matching: "[.]scm$"))

(define source-modules ::(list-of (list-of symbol))
  (map module-name source-files))

(define (imported-modules contents)::(list-of (list-of symbol))
  (apply
   append
   (map (lambda (expression)
	  (match expression
	    (`(import . ,modules)
	     (only (is _ in source-modules)
		   (map (lambda (module-spec)
			  (match module-spec
			    (`(rename ,module . ,_)
			     module)
			    (_
			     module-spec)))
			modules)))
	    (_
	     '()))) contents)))

(define (build-module-dependency-graph files)
  (let ((dependencies (mapping (module) '())))
    (for file ::java.io.File in files
      (let* ((contents (with-input-from-file (file:getPath) read-all))
	     (imports (imported-modules contents))
	     (source-module (module-name file)))
	(set! (dependencies source-module) imports)))
    dependencies))
  
(define module-dependency-graph
  (build-module-dependency-graph source-files))

(let ((circular-dependencies
       (only (lambda (m)
	       (is m in (reach module-dependency-graph m)))
	     (keys module-dependency-graph))))
  (when (isnt circular-dependencies null?)
    (print "circular dependencies between "
	   circular-dependencies)
    (exit)))

;; no dobra, to teraz musimy sprawdzic, ktore moduly

(define-cache (dependencies module)
  (reach module-dependency-graph module))
;; maja nowsza date od ich odpowiednikow w katalogu
;; build/cache (albo nie maja owych odpowiednikow
;; w ogole)

(define cached-files (files from: "build/cache" matching: "[.]class$"))

(define (source-file class-file ::java.io.File)::java.io.File
  
  (define (try pattern::string)::string
    (and-let* ((`(,_ ,stem) (regex-match
			     pattern
			     (class-file:getPath))))
      stem))
  
  (let ((stem
	 (or (try "^build/cache/([^.]*)[$]frame[0-9]*.class$")
	     (try "^build/cache/([^.]*)[$][0-9]*.class$")
	     (try "^build/cache/([^.]*).class$")
	     (error "invalid class file: "class-file))))
    (java.io.File
     (as String (string-append "src/"stem".scm")))))

;; i teraz chcemy zrobic dwie rzeczy:
;; po pierwsze, zbudowac liste plikow .class do skasowania
;; po drugie, chcemy zbudowac liste plikow .scm, ktore chcemy skompilowac.
;; oczywiscie zrodlo kazdego pliku .class, ktory kasujemy, musi byc
;; na liscie, ale beda na niej rowniez te pliki, dla ktorych pliki
;; .class nie istnieja.
;; Co zatem musimy zrobic?
;; - przeiterowac przez wszystkie pliki .class, sprawdzajac, czy
;;   sa starsze od swojego zrodla, i jesli tak, dodac je (oraz wszystko, co
;;   od nich zalezy) do listy obiektow do skasowania, a jesli nie - usunac
;;   pliki zrodlowe z listy

(define-cache (module-dependencies module)
  (reach module-dependency-graph module))

(define-cache (module-users module)
  (only (is _ in (module-dependencies module))
	(keys module-dependency-graph)))

(define-mapping (module-classes module) '())

(for class ::java.io.File in cached-files
     (let* ((scm ::java.io.File (source-file class))
	    (module (module-name scm)))
       (set! (module-classes module)
	     `(,class . ,(module-classes module)))))

(define source-files-to-build source-files)

(define modules-to-regenerate '())

(define class-files-to-remove '())

(for class::java.io.File in cached-files
  (let* ((scm ::java.io.File (source-file class))
	 (module (module-name scm)))
    (cond
     ((isnt scm in source-files)
      (print "The cache contains a .class file"
	     " with no .scm counterpart: "class))
     ((is (class:lastModified) <= (scm:lastModified))
      (let ((affected-modules `(,module . ,(module-users module))))
	(set! modules-to-regenerate
	      (union modules-to-regenerate affected-modules))
	(set! class-files-to-remove
	      (union class-files-to-remove
		     (apply append (map module-classes
					affected-modules))))))
     (else
      (set! source-files-to-build
	    (difference source-files-to-build `(,scm)))))))

(set! source-files-to-build
      (union source-files-to-build
	     (map module-file modules-to-regenerate)))

(define (graph-layers graph nodes)
  (let loop ((modules nodes)
	     (layers '())
	     (allowed-dependencies '()))
    (let-values (((layer modules)
		  (partition (is (graph _) subset?
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

(define build-list
  (map module-file
       (apply
	append
	(reverse
	 (graph-layers module-dependency-graph
		       (map module-name source-files-to-build))))))

(print build-list)
