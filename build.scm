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
  (synchronized (current-output-port)
    (display elements)
    ...
    (display #\newline)
    (flush-output-port)))

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

(define build-layers
  (let ((modules-to-build (map internal-module-name
			       source-files-to-build)))
    (fold-left
     (lambda (a b)
       (cons (map module-file
		  (intersection b modules-to-build))
	     a))
     (list application-files)
     layered-modules)))

(for class::java.io.File in class-files-to-remove
  (print "removing "class)
  (class:delete))

(for build-list in build-layers
  (for file::java.io.File in-parallel build-list
    (let-values (((name::string dir::java.io.File)
		  (target-file-name+directory file)))
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
	  (src:delete))))))

(print "Reindexing .class files...")
(let ((classes (list-files from: "build/cache"
			   such-that: (is "[.]class$"
					  regex-match
					  (_:getPath)))))
  (reset! module-classes)
  (for class ::java.io.File in classes
       (let* ((scm ::java.io.File (source-file class))
	      (module (internal-module-name scm)))
	 (set! (module-classes module)
	       (union (module-classes module) `(,class))))))

(let ((desktop-jar ::java.io.File
		   (as-file "build/grasp-desktop.jar"))
      (internal-dependencies ::(list-of java.io.File)
			     (append-map module-classes
					 (module-dependencies
					  '(grasp-desktop))))
      (external-dependencies ::(list-of string)
			     `("libs/kawa.jar"
			       . ,extra-dependencies-desktop)))
  (when (desktop-jar:exists)
    (print "removing "desktop-jar)
    (desktop-jar:delete))
  (let ((output (ZipBuilder desktop-jar)))
    (print "appending build/cache/grasp-desktop.zip")
    (output:append-entries! (ZipFile
			     "build/cache/grasp-desktop.zip"))
    (for class::java.io.File in internal-dependencies
      (print "adding "class)
      (output:add-file-at-level! 2 class))
    
    (for library-path::string in external-dependencies
      (print "appending "library-path)
      (output:append-entries-unless!
       (lambda (entry::ZipEntry) ::boolean
	       (let ((name ::string (entry:getName)))
		 (any (is _ regex-match name)
		      '("module-info.class$"
			"^META-INF"
			"MANIFEST.MF$"))))
       (ZipFile library-path)))
    
    (for asset::java.io.File in (list-files from: "assets")
      (print "adding "asset)
      (output:add-file-at-level! 0 asset))

    (print "adding init.scm")
    (output:add-file-as! "assets/init.scm"
			 (as-file "init/init.scm"))

    (print "writing manifest")
    (output:add-file-with-text! "\
Manifest-Version: 1.0
Main-Class: grasp$Mndesktop
" "META-INF/MANIFEST.MF")
    (output:close)
))
