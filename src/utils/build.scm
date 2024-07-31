(module-name (utils build))

(import (kawa regex))
(import (language assert))
(import (language define-interface))
(import (language define-syntax-rule))
(import (language define-type))
(import (language define-property))
(import (language keyword-arguments))
(import (utils functions))
(import (language infix))
(import (language match))
(import (language mapping))
(import (language while))
(import (language for))
(import (language define-cache))
(import (utils hash-table))
(import (utils android))
(import (utils graph))
(import (utils file))

(define-syntax-rule (print elements ...)
  (synchronized (current-output-port)
    (display elements)
    ...
    (display #\newline)
    (flush-output-port)))

(define (internal-module-name file ::java.io.File)
  ::(list-of symbol)
  (match (regex-match "^(?:./)?src/([^.]*)[.]scm$" (file:getPath))
    (`(,_ ,core)
     (map string->symbol (string-split core "/")))
    (_
     (error "Unable to parse file name "file))))

(define (module-file module-name ::(list-of symbol))::java.io.File
  (as-file (string-append "src/" (string-join module-name "/")
			  ".scm")))

(define (target-class scm-source ::java.io.File)::java.io.File
  (match (regex-match "^(?:./)?src/(.*)[.]scm$" (scm-source:getPath))
    (`(,_ ,core)
     (as-file (string-append "build/cache/"core".class")))))

(define (imported-modules file ::java.io.File
			  source-modules ::(list-of (list-of symbol)))
  ::(list-of (list-of symbol))
  (let ((contents (with-input-from-file (file:getPath) read-all)))
    (append-map
     (lambda (expression)
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
	  '())))
     contents)))

(define (build-module-dependency-graph files)
  (let ((dependencies (mapping (module) '()))
	(source-modules (map internal-module-name files)))
    (for file ::java.io.File in files
      (let* ((imports (imported-modules file source-modules))
	     (source-module (internal-module-name file)))
	(set! (dependencies source-module) imports)))
    dependencies))

(define (guess-source-file-from-name class-file ::java.io.File)::string
  
  (define (try pattern::string)::(either string #f)
    (and-let* ((`(,_ ,_ ,stem) (regex-match
				pattern
				(class-file:getPath))))
      stem))
  
  (let* ((stem
	  (or (try "^build/cache(.*/)([^/]+)\\$frame[0-9]*[.]class$")
	      (try "^build/cache(.*/)([^/]+)\\$[0-9]*[.]class$")
	      (try "^build/cache(.*/)([^/]+)[.]class$")
	      (error "invalid class file: "class-file)))
	 (fixed-stem (fold-left (lambda (stem replacement)
				  (and-let* ((`(,pattern ,replacement)
					      replacement))
				    (regex-replace* pattern
						    stem
						    replacement)))
				stem
				'(("\\$Mn" "-")))))
    (string-append fixed-stem".scm")))

(define (source-file class-file ::java.io.File)::java.io.File
  (otherwise #!null
    (and-let* ((port (open-binary-input-file class-file))
	       (,port (skip-characters from: port
				       until-having-read:
				       "SourceFile"))
	       ((isnt (read-char port) eof-object?))
	       ((isnt (read-char port) eof-object?))
	       ((isnt (read-char port) eof-object?))
	       (name ::string
		     (let ((name ::gnu.lists.FString
				 (gnu.lists.FString)))
		       (let loop ()
			 (let ((c (read-char port)))
			   (cond
			    ((or (eof-object? c)
				 (isnt 32 < (char->integer c) < 127))
			     name)
			    (else
			     (name:append c)
			     (loop)))))
		       (if (isnt "[.]scm$" regex-match name)
			   (guess-source-file-from-name class-file)
			   name)))
	       (class-directory (class-file:getParentFile))
	       (`(,_ ,path) (regex-match "^(?:./)?build/cache/(.*)$"
					 (class-directory:getPath))))
      (as-file (string-append "src/"path"/"name)))))

(define (build-file source ::string
		    #!key
		    (target-directory ::string "build/cache")
		    (package #!null)
		    (top-class-name #!null))
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
    (print"building "source)
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
    (command:setOutput
     (output:toPath)
     ;;com.android.tools.r8.OutputMode:DexIndexed
     com.android.tools.r8.OutputMode:DexFilePerClassFile)
    (com.android.tools.r8.D8:run
     (command:build))))

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

(define (build-zip source-file ::string target-file ::string)
  (compile-file source-file target-file))

(define (build-jar! #!key
		    module-dependencies ::(maps ((list-of symbol))
					       to: (list-of
						    (list-of symbol))) 
		    module-classes ::(maps ((list-of symbol))
					  to: (list-of java.io.File)) 
		    main-class ::string
		    available-modules ::(list-of (list-of symbol))
		    extra-dependencies ::(list-of string)
		    (assets ::(either string #f)"assets")
		    (init ::string "init/init.scm")
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
				   available-modules))))
	 (external-dependencies ::(list-of string)
				`("libs/kawa.jar"
				  . ,extra-dependencies)))
  (when (output-jar:exists)
    (output-jar:delete))
  
  (let ((output (ZipBuilder output-jar)))
    (output:append-entries! (ZipFile
			     (string-append
			      "build/cache/"main-class".zip")))
    (for class::java.io.File in internal-dependencies
      (output:add-file-at-level! 2 class))
    
    (for library-path::string in external-dependencies
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
	(output:add-file-at-level! 0 asset)))

    (output:add-file-as! "assets/init.scm" init-file)

    (let ((content ::String (string-append "\
Manifest-Version: 1.0
Main-Class: "main-class-name"
")))
      (output:add-file-with-text! content "META-INF/MANIFEST.MF"))
    (output:close))))

(define (build-apk! #!key
		    (init ::string "init/init.scm")
		    (package ::string "io.github.panicz")
		    (icon ::string "icons/grasp.png")
		    (label ::string "GRASP")
		    (keystore ::string "binary/keystore")
		    (password ::string "untrusted")
		    (key ::string "grasp-public")
		    (output-name ::string "build/grasp.apk"))
  (let* ((apk-file ::java.io.File (as-file output-name))
	 (temp-file ::java.io.File (java.io.File:createTempFile
				    "grasp-" ".apk"
				    (as-file "build")))
	 (assets (list-files from: "assets"))
	 (output (ZipBuilder temp-file)))
    (output:add-file-as! "res/drawable/icon.png"
			 (as-file icon))
    (for asset in assets
      (output:add-file-at-level! 0 asset))
    (output:add-file-as! "assets/init.scm" (as-file init))
    (let* ((manifest ::AndroidXML
		     (AndroidManifest package: package
				      label: label))
	   (axml ::bytevector (list->u8vector
			       (manifest:serialize))))
      (output:add-file-with-binary-content!
       axml "AndroidManifest.xml"))
    (output:add-file-with-binary-content! (resources-arsc
					   package)
					  "resources.arsc")
    (output:add-file-at-level!
     2 (as-file "build/cache/classes.dex"))
    (output:close)
    (com.iyxan23.zipalignjava.ZipAlign:alignZip
     (java.io.RandomAccessFile temp-file "r")
     (FileOutputStream (as java.io.File apk-file)))
    (temp-file:delete)
    (let ((args ::(array-of String)
		((array-of String) "sign"
		 "--ks" keystore
		 "--ks-key-alias" key
		 "--ks-pass" (string-append "pass:" password)
		 "--min-sdk-version" "23"
		 apk-file)))
      (com.android.apksigner.ApkSignerTool:main args))))

(define (build #!key
	       (targets ::(list-of (either 'android
					  'terminal
					  'desktop))
		       '(android desktop terminal))
	       (name ::string "GRASP")
	       (icon ::string "icons/grasp.png")
	       (init ::string "init/init.scm")
	       (package ::string "io.github.grasp")
	       (keystore ::string "binary/keystore")
	       (key ::string "grasp-public")
	       (password ::string "untrusted"))

  (assert (or (is 'android in targets)
	      (is 'desktop in targets)
	      (is 'terminal in targets)))
  
  (define package-components (string-split package "."))
  
  (define previous (load-mapping "build/previous.map"))

  (define previous-package (previous 'package))

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
  
  (define (main-class-file target)::string
    (match target
      ('desktop "build/cache/grasp-desktop.zip")
      ('terminal "build/cache/grasp-terminal.zip")
      ('android "build/cache/classes.dex")))
  
  (define main-class-files (map main-class-file targets))

    #;(and previous-package
	       (string=? package previous-package)
	       ;; jeszcze trzeba tu dodac warunek, ze zaleznosci
	       ;; w init.scm sie nie zwiekszyly wzgledem
	       ;; poprzednich
	       (every file-exists? main-class-files)
	       (let ((main-class-update
		      (apply min (map (lambda (name::string)
					(let ((file ::java.io.File
						    (as-file name)))
					  (file:lastModified)))
				      main-class-files))))
		 (every (lambda (source::java.io.File)
			  (is (source:lastModified) < main-class-update))
			`(,@application-files ,@dependency-files))))

  (when previous-package
    (let* ((previous-package-components
	    (string-split previous-package "."))
	   (previous-package-path
	    (string-append
	     "build/cache/"
	     (string-join previous-package-components
			  java.io.File:separator)))
	   (files-to-remove (list-files from: previous-package-path
					such-that: 
					(lambda (file::java.io.File)
					  (file:isFile)))))
      (for file::java.io.File in files-to-remove
	(print "deleting "file)
	(file:delete))
      (for depth from (length previous-package-components) to 1 by -1
	   (let ((directory (string-append
			     "build/cache/"
			     (string-join
			      (take depth previous-package-components)
			      "/"))))
	     (print "deleting directory "directory)
	     (delete-file directory)))))

  (delete-if-exists "build/cache/classes.dex")

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
		such-that: (is "[.]class$" regex-match
			       (_:getPath))))

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
	   (module ::(list-of symbol)
		   (internal-module-name scm)))
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
    (class:delete)
    (match (regex-match "^(.*)[.]class$" (class:getPath))
      (`(,_ ,stem)
       (delete-if-exists (string-append stem ".dex")))))

  (for build-list in build-layers
    (for file::java.io.File in-parallel build-list
      (build-file (file:getPath))))
  
  (concurrently
   (when (is 'desktop in targets)
     (build-zip "src/grasp-desktop.scm"
		(main-class-file 'desktop)))

   (when (is 'terminal in targets)
     (build-zip "src/grasp-terminal.scm"
		(main-class-file 'terminal)))

   ;; this needs to be called last, because it changes
   ;; some of the global properties of the Kawa compiler
   (when (is 'android in targets)
     (build-file "src/grasp-android.scm"
		 ;;target-directory: "build/grasp-android"
		 package: (string-append package ".")
		 top-class-name: (string-append package ".GRASP"))

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
     
     (print"building a list of classes to dex")

     (define classes-to-dex ::(list-of java.io.File)
       (let ((android-dependencies ::(list-of java.io.File)
				   (append-map
				    module-classes
				    (fold-left
				     (lambda (dependencies module)
				       (union dependencies
					      `(,module)
					      (module-dependencies module)))
				     (module-dependencies '(grasp-android))
				     (imported-modules
				      (as-file init)
				      (map internal-module-name
					   dependency-files))))))
	 (only
	  (lambda (class-file::java.io.File)
	    (let ((path ::String (class-file:getPath)))
	      (match (regex-match "^(.*)[.]class$" path)
		(`(,_ ,stem)
		 (let ((dex-file ::java.io.File (as-file
						 (string-append stem
								".dex"))))
		   (and
		    (or (not (dex-file:exists))
			(is (dex-file:lastModified)
			    <= (class-file:lastModified)))
		    (or (is class-file in android-dependencies)
			(is package regex-match path))))))))
	  cached-files)))

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

     (print "Integrating the .dex files")

     (integrate-dex `(,@dex-libraries ,@dex-files)
		    (as-file "build/cache"))
     ))
  
  (concurrently
   (when (is 'android in targets)
     (let ((file-name (string-append "build/"name".apk")))
       (print "building "file-name)
       (build-apk! output-name: file-name
		   init: init
		   package: package
		   icon: icon
		   keystore: keystore
		   key: key
		   label: name
		   password: password)))
   
   (when (is 'desktop in targets)
     (let ((file-name (string-append "build/" name"-desktop.jar")))
       (print "building "file-name)
       (build-jar!
	output-name: file-name
	module-dependencies: module-dependencies
	module-classes: module-classes
	available-modules: (map internal-module-name
				dependency-files)
	init: init
	main-class: "grasp-desktop"
	extra-dependencies: '("libs/jsvg-1.0.0.jar"))))

   (when (is 'terminal in targets)
     (let ((file-name (string-append "build/" name"-terminal.jar")))
       (print "building "file-name)

       (build-jar!
	output-name: file-name
	module-dependencies: module-dependencies
	module-classes: module-classes
	available-modules: (map internal-module-name
				dependency-files)
	init: init
	main-class: "grasp-terminal"
	assets: #f
	extra-dependencies:
	'("libs/lanterna-3.1.1.jar")))))
  
  (set! (previous 'name) name)
  (set! (previous 'init) init)
  (set! (previous 'package) package)
  (save-mapping previous "build/previous.map"))

(define-syntax-rule (with-command-line-arguments arg-list
			 ((name help default)
			  ...)
		       . actions)
  (let ((name ::string default)
	...)
    (let process ((args arg-list))
      (match args
	(`(,,@(is _ string=? (string-append
			      "--" (symbol->string 'name)))
	   ,value . ,rest)
	 (set! name value)
	 (process rest))
	...
	('() . actions)
	(_
	 (print "Invalid argument: "args)
	 (print "Available options:")
	 (print "  --"(symbol->string 'name)" "help" ["default"]")
	 ...
	 (exit))))))

(define (build-with command-line-args::(list-of string))
  (with-command-line-arguments (cdr command-line-args)
      ((targets "<comma-separated list of targets>"
		"android,desktop,terminal")
       (name "<target application name>" "GRASP")
       (icon "<path to icon file>" "icons/grasp.png")
       (init "<path to init file>" "init/init.scm")
       (package "<top class package>" "io.github.grasp")
       (keystore "<path to keystore>" "binary/keystore")
       (key "<key alias>" "grasp-public")
       (password "<key password>" "untrusted"))
    (build targets: (map string->symbol (string-split targets ","))
	   name: name
	   icon: icon
	   init: init
	   package: package
	   keystore: keystore
	   key: key
	   password: password)))
