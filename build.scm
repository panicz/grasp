#!/bin/sh 
#|
mkdir -p build/cache
rm -rf build/cache/io/github

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
      (unless (is scm in application-files)
	(print "Unknown source for "class": "scm)))
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
     '()
     layered-modules)))

(for class::java.io.File in class-files-to-remove
  (print "removing "class)
  (class:delete))

(define (build-file source ::string
		    #!key
		    (target-directory ::string "build/cache")
		    (package #!null)
		    (top-class-name #!null))
  (print"building "source)
  (let* ((messages ::gnu.text.SourceMessages
		   (gnu.text.SourceMessages))
	 (parse-options ::int (+ gnu.expr.Language:PARSE_PROLOG
				 gnu.expr.Language:PARSE_EXPLICIT))
	 (language ::gnu.expr.Language
		   (gnu.expr.Language:getDefaultLanguage))
	 (module-manager ::gnu.expr.ModuleManager
			 (gnu.expr.ModuleManager:getInstance))
	 (module-info ::gnu.expr.ModuleInfo
		      (module-manager:findWithSourcePath source))
	 (input ::gnu.kawa.io.InPort
		(open-input-file source))
	 (class-prefix-default ::String
			       gnu.expr.Compilation:classPrefixDefault))
    (try-finally
     (begin
       (when package
	 (set! gnu.expr.Compilation:classPrefixDefault package))
       (when top-class-name
	 (module-info:setClassName
	  (string-append
	   (or gnu.expr.Compilation:classPrefixDefault "")
	   (gnu.expr.Mangling:mangleNameIfNeeded top-class-name))))
       (module-manager:setCompilationDirectory target-directory)
       (let ((compilation ::gnu.expr.Compilation
			  (language:parse input messages
					  parse-options
					  module-info)))
	 (if (messages:seenErrors)
	     (primitive-throw (gnu.text.SyntaxException messages)))
	 (module-info:loadByStages gnu.expr.Compilation:CLASS_WRITTEN)
	 (if (messages:seenErrors)
	     (primitive-throw (gnu.text.SyntaxException messages)))))
     (set! gnu.expr.Compilation:classPrefixDefault class-prefix-default))))

(define (build-zip source-file ::string target-file ::string)
  (print "building zip "source-file)
  (compile-file source-file target-file))

(for build-list in build-layers
  (for file::java.io.File in-parallel build-list
    (build-file (file:getPath))))

(concurrently
 (build-zip "src/grasp-desktop.scm" "build/cache/grasp-desktop.zip")
 (build-zip "src/grasp-terminal.scm" "build/cache/grasp-terminal.zip")
 (build-file "src/grasp-android.scm"
	     ;;target-directory: "build/grasp-android"
	     package: "io.github.grasp."
	     top-class-name: "io.github.grasp.GRASP"))

(print "Reindexing .class files...")
(set! cached-files (list-files from: "build/cache"
			   such-that: (is "[.]class$"
					  regex-match
					  (_:getPath))))
(reset! module-classes)

(for class::java.io.File in cached-files
  (let* ((scm ::java.io.File (source-file class))
	 (module (internal-module-name scm)))
    (set! (module-classes module)
	  (union (module-classes module) `(,class)))))

(define (file->path file::java.io.File)::java.nio.file.Path
  (file:toPath))

(define (update-dex-cache input::(list-of java.io.File)
			  output::java.io.File)
  ::void
  (let ((command ::com.android.tools.r8.D8Command:Builder
		 (com.android.tools.r8.D8Command:builder)))
    (command:setMinApiLevel 23)
    (command:addProgramFiles (map file->path input))
    (command:addLibraryFiles (map file->path
				  (list
 				   (as-file "libs/kawa.jar")
				   (as-file "libs/android.jar")
				   (as-file "build/cache"))))
    (command:setIntermediate #t)
    (command:setOutput (output:toPath)
		       ;;com.android.tools.r8.OutputMode:DexIndexed
		       com.android.tools.r8.OutputMode:DexFilePerClassFile)
    (com.android.tools.r8.D8:run
     (command:build))))

(print"building a list of classes to dex")

(define classes-to-dex ::(list-of java.io.File)
  (only
   (lambda (class-file::java.io.File)
     (match (regex-match "^(.*)[.]class$" (class-file:getPath))
       (`(,_ ,stem)
	(let ((dex-file ::java.io.File (as-file
					(string-append stem ".dex"))))
	  (or (not (dex-file:exists))
	      (is (dex-file:lastModified) <= (class-file:lastModified)))))))
   cached-files))

(print"dexing "classes-to-dex)

(update-dex-cache classes-to-dex (as-file "build/cache"))

(print"Generating the .dex index")

(define dex-files
  (list-files from: "build/cache"
	      such-that: (is "[.]dex$" regex-match
			     (_:getPath))))

(define dex-libraries
  (list-files from: "libs"
	      such-that: (is "[.]dex$" regex-match
			     (_:getPath))))

(define (integrate-dex input::(list-of java.io.File)
		       output::java.io.File)
  ::void
  (let ((command ::com.android.tools.r8.D8Command:Builder
		 (com.android.tools.r8.D8Command:builder)))
    (command:addProgramFiles (map file->path input))
    (command:setMinApiLevel 23)
    
    #;(command:addLibraryFiles (map file->path
				  (list
 				   (as-file "libs/kawa.jar")
				   (as-file "libs/android.jar")
				   (as-file "build/cache"))))
    (command:setOutput (output:toPath)
		       com.android.tools.r8.OutputMode:DexIndexed)
    (com.android.tools.r8.D8:run
     (command:build))))

(print "Integrating the .dex files")

(integrate-dex `(,@dex-libraries ,@dex-files) (as-file "build/cache"))

(let* ((apk-file ::java.io.File (as-file "build/grasp.apk"))
       (resources  (list-files from: "res"))
       (assets (list-files from: "assets"))
       (output (ZipBuilder apk-file)))
  (for resource in resources
    (output:add-file-at-level! 0 resource))
  (for asset in assets
    (output:add-file-at-level! 0 asset))
  (output:add-file-at-level! 0 (as-file "AndroidManifest.xml"))
  (output:add-file-at-level! 0 (as-file "resources.arsc"))
  (output:add-file-at-level! 2 (as-file "build/cache/classes.dex"))
  (output:close)
  (com.iyxan23.zipalignjava.ZipAlign:alignZip
   (java.io.RandomAccessFile apk-file "r")
   (FileOutputStream (as java.io.File (as-file "build/grasp-aligned.zip"))))

  (let ((args ::(array-of String)
	      ((array-of String) "sign"
	       "--ks" "/data/data/com.termux/files/home/pland.keystore"
	       "--ks-key-alias" "pland"
	       "--ks-pass" "pass:quack01"
	       "--min-sdk-version" "23"
	       "build/grasp-aligned.zip")))
  (com.android.apksigner.ApkSignerTool:main args))
  
  #;(let ((params ::com.android.apksigner.SignerParams
		(com.android.apksigner.SignerParams)))
    (params:setKeystoreFile "~/pland.keystore")
    (params:setKeystoreKeyAlias "pland")
    (params:setKeystorePasswordSpec "pass:quack01")
    (let ((signer ::com.android.apksig.ApkSigner:Builder
		  (com.android.apksig.ApkSigner:Builder params)))
      (signer:setInputApk (as java.io.File (as-file "build/grasp-aligned.zip")))
      (signer:setOutputApk (as java.io.File (as-file "build/grasp-signed.zip")))
      (let ((signer ::com.android.apksig.ApkSigner (signer:build)))
	(signer:sign)))))

#;(define dex-files ::(list-of java.io.File)
  (list-files from: "build/cache"
	      such-that: (is "[.]dex$" regex-match (_:getPath))))

;;(dex cached-files (as-file "build/cache"))

#;(dex-classes dex-files)

(define (build-jar! #!key
		    module-dependencies::(maps ((list-of symbol))
					       to: (list-of
						    (list-of symbol)))
		    main-class::string
		    extra-dependencies::(list-of string)
		    (assets::(either string #f)"assets")
		    (init::string "init/init.scm")
		    output-name)
  (let* ((output-name (or output-name
			  (string-append "build/" main-class ".jar")))
	 (main-class-name (fold-left (lambda (stem replacement)
				       (and-let* ((`(,pattern ,replacement)
						   replacement))
					 (regex-replace* pattern
							 stem
							 replacement)))
				     main-class
				     '(("\\-" "\\$Mn"))))
	 (output-jar ::java.io.File (as-file output-name))
	 (init-file ::java.io.File (as-file init))
	 (internal-dependencies ::(list-of java.io.File)
				(append-map
				 module-classes
				 (fold-left
				  (lambda (dependencies module)
				    (union dependencies
					   `(,module)
					   (module-dependencies module)))
				  (module-dependencies
				   `(,(string->symbol main-class)))
				  
				  (imported-modules
				   init-file
				   (map internal-module-name
					dependency-files)))))
	 (external-dependencies ::(list-of string)
				`("libs/kawa.jar"
				  . ,extra-dependencies)))
  (when (output-jar:exists)
    (print "removing "output-jar)
    (output-jar:delete))
  
  (let ((output (ZipBuilder output-jar)))
    (print "appending build/cache/"main-class".zip")
    (output:append-entries! (ZipFile
			     (string-append
			      "build/cache/"main-class".zip")))
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

    (when assets
      (for asset::java.io.File in (list-files from: assets)
	(print "adding "asset)
	(output:add-file-at-level! 0 asset)))

    (print "adding "init)
    (output:add-file-as! "assets/init.scm" init-file)

    (print "writing manifest")
    (let ((content ::String (string-append "\
Manifest-Version: 1.0
Main-Class: "main-class-name"
")))
      (output:add-file-with-text! content "META-INF/MANIFEST.MF"))
    (output:close))))

#;(concurrently
 (build-jar!
  module-dependencies: module-dependencies
  main-class: "grasp-desktop"
  extra-dependencies: '("libs/jsvg-1.0.0.jar"))

 (build-jar!
  module-dependencies: module-dependencies
  main-class: "grasp-terminal"
  assets: #f
  extra-dependencies: '("libs/lanterna-3.1.1.jar")))
