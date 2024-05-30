#!/bin/sh
#|
mkdir -p build/cache

exec java -jar "libs/kawa.jar" -Dkawa.import.path="|:.:build/cache:src" \
  --no-warn-unreachable -f "$0"
|#

(import (kawa regex))
(import (only (srfi :1) filter-map))
(import (language define-syntax-rule))
(import (language assert))
(import (language define-interface))
(import (utils shell))
(import (utils functions))
(import (language infix))
(import (language match))
(import (utils hash-table))
(import (language mapping))
(import (language for))
(import (define-cache))

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

(define (all-scm-files-from-subdirectories-of dir::string)
  (assert (is (string-length dir) > 0))
  (assert (none char-whitespace? dir))
  (let ((prefix-length (+ (string-length dir) 1)))
    (filter-map (lambda (name)
		  (and (is (length name) > prefix-length)
		       (none (is _ eq? #\#) name)
		       name))
		(string-split
		 (shell "find "dir" -name *.scm") "\n"))))

(define (module-name path ::string);;(list-of symbol)
  (match (regex-match "^(?:./)?src/([^.]*)[.]scm$" path)
    (`(,_ ,core)
     (map string->symbol (string-split core "/")))))

(define (module-file module-name)::string
  (string-append "src/" (string-join module-name "/") ".scm"))

(define (imported-modules contents)
  (apply
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
	     '()))) contents))

(define (build-module-dependency-graph files)
  (let ((dependencies (mapping (module) '())))
    (for file in files
      (let* ((contents (with-input-from-file file read-all))
	     (imports (imported-modules contents))
	     (source-module (module-name file)))
	(set! (dependencies source-module) imports)))
    dependencies))

(define source-files
  (all-scm-files-from-subdirectories-of "src"))
  
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

(define cached-files
  (string-split
   (shell "find build/cache -name '*.class'") "\n"))

(define (source-file class-file ::string)::string
  (define (try pattern)
    (and-let* ((`(,_ ,stem) (regex-match pattern class-file)))
      stem))
  (let ((stem
	 (or (try "^build/cache/([^.]*)[$]frame[0-9]*.class$")
	     (try "^build/cache/([^.]*)[$][0-9]*.class$")
	     (try "^build/cache/([^.]*).class$")
	     (error "invalid class file: "class-file))))
    (string-append "src/"stem".scm")))

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

