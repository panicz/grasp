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

(import (utils shell))

(define-syntax-rule (print elements ...)
  (display elements)
  ...
  (display #\newline)
  (flush-output-port))

(define (reach #;of graph #;from node)
  (define (walk #;from novel #;into visited)
    (let* ((vicinity (fold-left union '() (map graph novel)))
	   (visited (union visited novel))
	   (novel (difference vicinity visited)))
      (if (null? novel)
	  visited
	  (walk #;from novel #;into visited))))
  (walk #;from (graph node) #;into '()))

(define (as-file path::(either string java.io.File))::java.io.File
  (if (java.io.File? path)
      path
      (java.io.File (as String path))))

(define (list-files #!key
		    (from ::(either string java.io.File) ".")
		    (such-that always)
		    (max-depth +inf.0))
  ::(list-of java.io.File)
  (let ((directory ::java.io.File (as-file from))
	(result '()))
    (assert (directory:isDirectory))
    (for file ::java.io.File in (directory:listFiles)
	 (cond
	  ((and (file:isDirectory)
		(is max-depth > 0))
	   (set! result `(,@(list-files from: file
					such-that: such-that
					max-depth: (- max-depth 1))
			  ,@result)))
	  ((and (file:isFile) (such-that file))
	   (set! result `(,file . ,result)))))
    result))

(define (internal-module-name file ::java.io.File)::(list-of symbol)
  (match (regex-match "^(?:./)?src/([^.]*)[.]scm$" (file:getPath))
    (`(,_ ,core)
     (map string->symbol (string-split core "/")))))

(print "Gathering file list...")

(define (module-file module-name ::(list-of symbol))::java.io.File
  (as-file (string-append "src/" (string-join module-name "/")
			  ".scm")))

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
  `(#;,@test-files ,@application-files ,@dependency-files))
  
(define source-modules ::(list-of (list-of symbol))
  (map internal-module-name all-files))

(print "Gathering dependencies...")

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
	     (source-module (internal-module-name file)))
	(set! (dependencies source-module) imports)))
    dependencies))

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

(print "Gathering cache files...")

(define cached-files ::(list-of java.io.File)
  (list-files from: "build/cache"
	      such-that: (is "[.]class$" regex-match (_:getPath))))

(define (extract-source-file class-file ::java.io.File)::java.io.File
  (otherwise #!null
    (and-let* ((port (open-binary-input-file class-file))
	       (,port (skip-characters from: port
				       until-having-read:
				       "SourceFile"))
	       ((isnt (read-char port) eof-object?))
	       ((isnt (read-char port) eof-object?))
	       ((isnt (read-char port) eof-object?))
	       (name ::string (let ((name ::gnu.lists.FString
					  (gnu.lists.FString)))
				(let loop ()
				  (let ((c (read-char port)))
				    (cond
				     ((or (eof-object? c)
					  (eq? c #\x01))
				      name)
				     (else
				      (name:append c)
				      (loop)))))))
	       (class-directory (class-file:getParentFile))
	       (`(,_ ,path) (regex-match "^(?:./)?build/cache/(.*)$"
					 (class-directory:getPath))))
      (as-file (string-append "src/"path"/"name)))))

(define (source-file class-file ::java.io.File)::java.io.File
  
  (define (try pattern::string)::(either string #f)
    (and-let* ((`(,_ ,stem) (regex-match
			     pattern
			     (class-file:getPath))))
      stem))
  
  (let* ((stem
	 (or (try "^build/cache/([^.]*)\\$frame[0-9]*.class$")
	     (try "^build/cache/([^.]*)\\$[0-9]*.class$")
	     (try "^build/cache/([^.]*).class$")
	     (error "invalid class file: "class-file)))
	 (fixed-stem (fold-left (lambda (stem replacement)
				  (and-let* ((`(,pattern ,replacement)
					      replacement))
				    (regex-replace* pattern
						    stem
						    replacement)))
				stem
				'(("\\$Mn" "-")))))
    (as-file (string-append "src/"fixed-stem".scm"))))

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
	     `(,class . ,(module-classes module)))))

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
      (and-let* ((source ::java.io.File (extract-source-file class))
		 ((is (class:lastModified) <= (source:lastModified))))
	(set! class-files-to-remove
	      	      (union class-files-to-remove `(,class)))))
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
	(print"empty layer with remaining modules "modules)
	`(,modules . ,layers))
       (else
	(loop modules
	      `(,layer . ,layers)
	      `(,@layer ,@allowed-dependencies)))))))

(define layered-modules
  (graph-layers module-dependency-graph source-modules))

(define (unzip archive ::string #!key (into ::string "."))::void
  ;;(print"decompressing "archive" into "into)
  (let* ((dir ::java.io.File (java.io.File (as String into)))
	 (buffer ::(array-of byte) ((array-of byte) length: 1024))
	 (data ::java.io.FileInputStream
	       (java.io.FileInputStream (java.io.File archive)))
	 (source ::java.util.zip.ZipInputStream
		 (java.util.zip.ZipInputStream data)))
    (let next-entry ()
      (let ((entry ::java.util.zip.ZipEntry (source:getNextEntry)))
	(when entry
	  ;;(print"deflating "entry)
	  (let ((file ::java.io.File (java.io.File
				      dir (entry:getName))))
	    (if (entry:isDirectory)
		(file:mkdirs)
		(let ((parent ::java.io.File (file:getParentFile)))
		  (parent:mkdirs)
		  (let ((output ::java.io.FileOutputStream
				(java.io.FileOutputStream file)))
		    (let rewrite-next ()
		      (let ((n ::int (source:read buffer))) ;<
			(when (is n > 0)
			  (output:write buffer 0 n)
			  (rewrite-next))))
		    (output:close)))))
	  (next-entry))))
    (source:closeEntry)
    (source:close)))

(define (move-files #!key from to)
  ::void
  (let ((source-directory ::java.io.File (as-file from))
	(target-directory ::java.io.File (as-file to)))
    (for file::java.io.File in (source-directory:listFiles)
      (cond
       ((file:isDirectory)
	(move-files from: file to: (java.io.File target-directory
						 (file:getName)))
	(file:delete))
       (else
	(file:renameTo (java.io.File target-directory
				     (file:getName))))))))

(define (delete filename ::string)
  (let ((file ::java.io.File
	      (java.io.File (as String filename))))
    (file:delete)))

(define (existing-file filename ::string)::java.io.File
  (let ((file ::java.io.File
	      (java.io.File (as String filename))))
    (if (file:exists)
	file
	#!null)))

(define build-list
  (let ((modules-to-build (map internal-module-name
			       source-files-to-build)))
    (map module-file
	 (fold-left
	  (lambda (a b)
	    (append (intersection b modules-to-build) a))
	  '()
	  layered-modules))))

(define (target-file-name+directory source-file ::java.io.File)
  ::(Values string java.io.File)
  (match (regex-match "^src/([^.]*)[.]scm$" (source-file:getPath))
    (`(,_ ,stem)
     (match (regex-match "(.*)/([^/]*)$" stem)
       (`(,_ ,path ,name)
	(values
	 (string-append name ".zip")
	 (as-file (string-append "build/cache/" path))))))))

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
      (unzip target into: "build/cache")
      (delete target)
      (and-let* ((src ::java.io.File (existing-file
				      "build/cache/src"))
		 ((src:isDirectory)))
	(move-files from: "build/cache/src" to: "build/cache")
	(src:delete)))))
