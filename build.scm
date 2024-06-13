#!/bin/sh
#|
mkdir -p build/cache

JARS=`ls libs/*.jar | tr '\n' ':' | sed 's/:$//'`

exec java -cp "$JARS:build/cache" kawa.repl \
  -Dkawa.import.path="|:src:build/cache:." \
  -f "$0"
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
(import (utils file))
(import (utils graph))
(import (utils build))

(define-syntax-rule (print elements ...)
  (display elements)
  ...
  (display #\newline)
  (flush-output-port))

(define extra-dependencies-desktop
  '("libs/jsvg-1.0.0.jar"))

(define extra-dependencies-terminal
  '("libs/lanterna-3.1.1.jar"))

(define extra-dependencies-android
  '("libs/androidsvg-1.4.jar"))

(print "Gathering file list...")

(define dependency-files ::(list-of java.io.File)
  (list-files from: "src"
	      such-that: (is "^(?:./)?src/[^/]+/.+[.]scm$"
			     regex-match (_:getPath))))

(define application-files ::(list-of java.io.File)
  (list-files from: "src"
	      such-that: (is "^(?:./)?src/grasp-[^/]+[.]scm"
			     regex-match (_:getPath))
	      max-depth: 0))

(define test-files ::(list-of java.io.File)
  (list-files from: "src"
	      such-that: (is "^(?:./)?src/test-[^/]+[.]scm"
			     regex-match (_:getPath))
	      max-depth: 0))

(define all-files ::(list-of java.io.File)
  `(,@test-files ,@application-files ,@dependency-files))
  
(define source-modules ::(list-of (list-of symbol))
  (map internal-module-name all-files))

(print "Building dependency graph...")

(define module-dependency-graph
  (build-module-dependency-graph all-files))

(define-cache (module-dependencies module ::(list-of symbol))
  ::(list-of (list-of symbol))
  (reach module-dependency-graph module))

(print "Checking for circular dependencies...")

(let ((circular-dependencies
       (only (lambda (m)
	       (is m in (module-dependencies m)))
	     (keys module-dependency-graph))))
  (when (isnt circular-dependencies null?)
    (print"circular dependencies between "
	   circular-dependencies)
    (exit)))

(print "Layering the dependency graph...")

(define layered-modules
  (graph-layers module-dependency-graph source-modules))

(print "Gathering cache files...")

(define cached-files ::(list-of java.io.File)
  (list-files from: "build/cache"
	      such-that: (is "[.]class$" regex-match (_:getPath))))

(define-cache (module-users module ::(list-of symbol))
  ::(list-of (list-of symbol))
  (only (is module in (module-dependencies _))
	(keys module-dependency-graph)))

(define-mapping (module-classes module ::(list-of symbol))
  ::(list-of java.io.File)
  '())

(for class ::java.io.File in cached-files
     (let* ((scm ::java.io.File (source-file class))
	    (module (internal-module-name scm)))
       (set! (module-classes module)
	     (union (module-classes module) `(,class)))))

(define source-files-to-build ::(list-of java.io.File)
  dependency-files)

(define modules-to-regenerate ::(list-of (list-of symbol))
  '())

(define class-files-to-remove ::(list-of java.io.File)
  '())
  
(print "Checking which files need to be recompiled...")

(for class::java.io.File in cached-files
  (let* ((scm ::java.io.File (source-file class))
	 (module ::(list-of symbol) (internal-module-name scm)))
    (cond
     ((isnt scm in dependency-files)
      (print "Unknown source for "class": "scm))
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
      (intersection
       dependency-files
       (union source-files-to-build
	      (map module-file modules-to-regenerate))))

(define build-list
  (let ((modules-to-build (map internal-module-name
			       source-files-to-build)))
    (fold-left
     (lambda (a b)
       (append (map module-file (intersection
				 b modules-to-build))
	       a))
     (reverse application-files)
     layered-modules)))

(for class::java.io.File in class-files-to-remove
  (print "removing "class)
  (class:delete))

(for file::java.io.File in build-list
  (let-values (((name::string dir::java.io.File)
		(target-file-name+directory file)))
    (dir:mkdirs)
    (let ((target (string-append  "build/cache/" name)))
      (print"building "(file:getPath))
      (compile-file (file:getPath) target)
      (unless (is file in application-files)
	(unzip target into: "build/cache")
	(delete target))
      (and-let* ((src ::java.io.File (existing-file
				      "build/cache/src"))
		 ((src:isDirectory)))
	(move-files from: "build/cache/src" to: "build/cache")
	(src:delete)))))


#|
(let ((desktop-jar ::java.io.File (as-file "build/desktop.jar"))
      (internal-dependencies (module-dependencies
				   '(grasp-desktop))))
    
  ;; 1. dodac wszystkie klasy z build/cache/grasp-desktop.zip
  ;; 2. dodac wszystkie zaleznosci z build/cache
  ;; 3. dodac wszystkie elementy z jarow z zaleznosciami
  ;; 4. dodac manifest
  (when (desktop-jar:exists)
    (desktop-jar:delete))
  (let ((output (ZipOutputSteam (FileOutputStream desktop-jar))))
    (append-zip-entries!
     from: (ZipFile "build/cache/grasp-desktop.zip")
     to: output)
    (let* (



(unless (and-let* ((kawa-dir ::java.io.File
			     (existing-file "build/kawa")))
	  (kawa-dir:isDirectory))
  (print "Unzipping libs/kawa.jar into build/kawa")
  (unzip "libs/kawa.jar" into: "build/kawa"))

(unless (and-let* ((desktop-dir ::java.io.File
				(existing-file "build/desktop")))
	  (desktop-dir:isDirectory))
  (for dependency in extra-dependencies-desktop
    (print "Unzipping "dependency" into build/desktop")
    (unzip dependency into: "build/desktop")
    (and-let* ((module-info ::java.io.File
			    (existing-file
			     "build/desktop/module-info.class")))
      (module-info:delete))))

(let ((desktop-jar ::java.io.File (as-file "build/desktop.jar")))
  (when (desktop-jar:exists)
    (desktop-jar:delete))
  (let ((output ...))
    ...))
|#

;; no dobra, to teraz musimy:
;; - skompilowac grasp-desktop.scm
;; - rozpakowac wszystkie repozytoria
;;   (a tak naprawde to powinny juz byc rozpakowane)
;; - przeniesc pliki .class z kompilacji do
;;   pdpowiedniego folderu
;; - przekopiowac pliki "assets"
;; - przekopiowac plik init.scm
;; - zzipowac
