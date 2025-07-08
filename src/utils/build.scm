(module-name (utils build))

(import (kawa regex))
(import (language assert))
(import (language define-interface))
(import (language define-syntax-rule))
(import (language define-type))
(import (language attributes))
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

(define-syntax-rule (report elements ...)
  (synchronized (current-output-port)
    (display elements)
    ...
    (display #\newline)
    (flush-output-port)))

(define (escape-slashes regex::java.lang.String)
  (regex:replaceAll "\\\\" "\\\\\\\\"))

(define // (escape-slashes java.io.File:separator))

(define-alias ModuleTag (list-of symbol))

(define (file-module-tag file ::java.io.File)
  ::ModuleTag
  (match (regex-match (string-append "^(?:."//")?src"//"([^.]*)[.]scm$")
		      (file:getPath))
    (`(,_ ,core)
     (map string->symbol (string-split core java.io.File:separator)))
    (_
     (error "Unable to parse file name "file))))

(define (module-file module-name ::ModuleTag)::java.io.File
  (as-file (string-append (string-join `("src" . ,module-name) java.io.File:separator)
			  ".scm")))

(define (target-class scm-source ::java.io.File)::java.io.File
  (match (regex-match (string-append "^(?:."//")?src"//"(.*)[.]scm$")
		      (scm-source:getPath))
    (`(,_ ,core)
     (as-file "build" "cache" (string-append core".class")))))

(define (imported-modules file ::java.io.File
			  source-modules ::(list-of ModuleTag))
  ::(list-of ModuleTag)
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
  ::(maps ModuleTag to: (list-of ModuleTag))
  (let ((dependencies (mapping (module ::ModuleTag)
			::(list-of ModuleTag)
			'()))
	(source-modules (map file-module-tag files)))
    (for file ::java.io.File in files
      (let* ((imports (imported-modules file source-modules))
	     (source-module (file-module-tag file)))
	(set! (dependencies source-module) imports)))
    dependencies))

(define (guess-source-file-from-name class-file ::java.io.File)::string

  (define / (escape-slashes java.io.File:separator))
  
  (define (try . pattern)::(either string #f)
    (and-let* ((`(,_ ,_ ,stem) (regex-match
				(string-join pattern "")
				(class-file:getPath))))
      stem))
  
  (let* ((stem
	  (or (try "^build"/"cache(.*"/")([^"/"]+)\\$frame[0-9]*[.]class$")
	      (try "^build"/"cache(.*"/")([^"/"]+)\\$[0-9]*[.]class$")
	      (try "^build"/"cache(.*"/")([^"/"]+)[.]class$")
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
	       (`(,_ ,path) (regex-match (string-append "^(?:."//")?build"//"cache"//"(.*)$")
					 (class-directory:getPath))))
      (as-file "src" path name))))

(define (build-file source ::string
		    #!key
		    (target-directory ::string (join-path "build" "cache"))
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
    (report "building "source)
    (try-finally
     (begin
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
  (let ((command ::com.android.tools.r8.D8Command$Builder
		 (com.android.tools.r8.D8Command:builder)))
    (command:setMinApiLevel 23)
    (command:addProgramFiles (map file->path input))
    (command:addLibraryFiles (map file->path
				  (list
 				   (as-file "libs" "kawa.jar")
				   (as-file "libs" "android.jar")
				   (as-file "build" "cache"))))
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
  (let ((command ::com.android.tools.r8.D8Command$Builder
		 (com.android.tools.r8.D8Command:builder)))
    (command:addProgramFiles (map file->path input))
    (command:setMinApiLevel 23)
    
    #;(command:addLibraryFiles (map file->path
				  (list
 				   (as-file "libs" "kawa.jar")
				   (as-file "libs" "android.jar")
				   (as-file "build" "cache"))))
    (command:setOutput (output:toPath)
		       com.android.tools.r8.OutputMode:DexIndexed)
    (com.android.tools.r8.D8:run
     (command:build))))

(define (build-zip source-file ::string target-file ::string)
  (report "building "target-file)
  (compile-file source-file target-file))

(define (build-jar! #!key
		    module-dependencies ::(maps (ModuleTag)
					       to: (list-of ModuleTag)) 
		    module-classes ::(maps (ModuleTag)
					  to: (list-of java.io.File)) 
		    main-class ::(maybe string)
		    (available-modules ::(list-of ModuleTag)'())
		    (extra-dependencies ::(list-of string) '())
		    (assets ::(list-of java.io.File) '())
		    (init ::string (join-path "init" "init.scm"))
		    (package ::string "")
		    output-name)
  (let* ((output-name (or output-name
			  (join-path "build" (string-append main-class ".jar"))))
	 (main-class-name (fold-left (lambda (stem replacement)
				       (and-let* ((`(,pattern
						     ,replacement)
						   replacement))
					 (regex-replace* pattern
							 stem
							 replacement)))
				     main-class
				     '(("\\-" "\\$Mn"))))
	 (main-class-name (if (string=? package "")
			      main-class-name
			      (string-append
			       package "." main-class-name)))
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
				`(,(join-path "libs" "kawa.jar")
				  . ,extra-dependencies)))
  (when (output-jar:exists)
    (output-jar:delete))
  
  (let ((output (ZipBuilder output-jar)))
    (output:append-entries! (ZipFile
			     (as String
				 (join-path "build" "cache"
					    (string-append main-class".zip")))))
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
    
    (for asset::java.io.File in assets
      (output:add-file-as! (join-path "assets" (asset:getName)) asset))

    (output:add-file-as! (join-path "assets" "init.scm") init-file)

    (let ((content ::String (string-append "\
Manifest-Version: 1.0
Main-Class: "main-class-name"
")))
      (output:add-file-with-text! content (join-path "META-INF" "MANIFEST.MF")))
    (output:close))))

(define (build-apk! #!key
		    (init ::string (join-path "init" "init.scm"))
		    (package ::string "io.github.panicz")
		    (icon ::string (join-path "icons" "grasp.png"))
		    (label ::string "GRASP")
		    (keystore ::string (join-path "binary" "keystore"))
		    (password ::string "untrusted")
		    (key ::string "grasp-public")
		    (assets ::(list-of string) '("assets"))
		    (output-name ::string (join-path "build" "grasp.apk")))
  (let* ((apk-file ::java.io.File (as-file output-name))
	 (temp-file ::java.io.File (java.io.File:createTempFile
				    "grasp-" ".apk"
				    (as-file "build")))
	 (assets (append-map (lambda (source::java.io.File)
			       (list-files from: source))
			     assets))
	 (output (ZipBuilder temp-file)))
    (output:add-file-as! (join-path "res" "drawable" "icon.png")
			 (as-file icon))
    (for asset ::java.io.File in assets
      (output:add-file-as! (join-path "assets" (asset:getName)) asset))
    (output:add-file-as! (join-path "assets" "init.scm") (as-file init))
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
     2 (as-file "build" "cache" "classes.dex"))
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
	       (icon ::string (join-path "icons" "grasp.png"))
	       (init ::string (join-path "init" "init.scm"))
	       (package ::string "io.github.grasp")
	       (keystore ::string (join-path "binary" "keystore"))
	       (key ::string "grasp-public")
	       (assets ::(list-of string) '("assets"))
	       (password ::string "untrusted"))

  (assert (or (is 'android in targets)
	      (is 'desktop in targets)
	      (is 'terminal in targets)))
  
  (define package-components (string-split package "."))
  
  (report "Gathering file list...")
  
  (define dependency-files ::(list-of java.io.File)
    (list-files from: "src"
		such-that: (is (string-append
				"^(?:."//")?src"//"[^"//"]+"//".+[.]scm$")
			       regex-match (_:getPath))))
  
  (define application-files ::(list-of java.io.File)
    (list-files from: "src"
		such-that: (is (string-append "^(?:."//")?src"//"grasp-[^"//"]+[.]scm")
			       regex-match (_:getPath))
		max-depth: 0))

  (define test-files ::(list-of java.io.File)
    (list-files from: "src"
		such-that: (is (string-append "^(?:."//")?src"//"test-[^"//"]+[.]scm")
			       regex-match (_:getPath))
		max-depth: 0))

  (define dependency-modules ::(list-of ModuleTag)
    (map file-module-tag dependency-files))
  
  (define init-dependencies 
    (imported-modules (as-file init) dependency-modules))
  
  (define all-files ::(list-of java.io.File)
    `(,@test-files ,@application-files ,@dependency-files))

  (define source-modules ::(list-of ModuleTag)
    (map file-module-tag all-files))
  
  (define (main-class-file target)::string
    (match target
      ('desktop (join-path "build" "cache" "grasp-desktop.zip"))
      ('terminal (join-path "build" "cache" "grasp-terminal.zip"))
      ('android (join-path "build" "cache" "classes.dex"))))
  
  (define main-class-files (map main-class-file targets))

  (report "Building dependency graph...")

  (define module-dependency-graph
    (build-module-dependency-graph all-files))

  (define-cache (module-dependencies module ::ModuleTag)
    ::(list-of ModuleTag)
    (reach module-dependency-graph module))

  (report "Checking for circular dependencies...")

  (let ((circular-dependencies
	 (only (lambda (m)
		 (is m in (module-dependencies m)))
	       (keys module-dependency-graph))))
    (when (isnt circular-dependencies null?)
      (report "circular dependencies between "
	    circular-dependencies)
      (exit)))

  (report "Layering the dependency graph...")

  (define layered-modules
    (graph-layers module-dependency-graph source-modules))

  (report "Gathering cache files...")

  (define cached-files ::(list-of java.io.File)
    (list-files from: (join-path "build" "cache")
		such-that: (is "[.]class$" regex-match
			       (_:getPath))))

  (define-cache (module-users module ::ModuleTag)
    ::(list-of ModuleTag)
    (only (is module in (module-dependencies _))
	  (keys module-dependency-graph)))

  (define-mapping (module-classes module ::ModuleTag)
    ::(list-of java.io.File)
    '())

  (for class ::java.io.File in cached-files
       (let* ((scm ::java.io.File (source-file class))
	      (module (file-module-tag scm)))
	 (set! (module-classes module)
	       (union (module-classes module) `(,class)))))

  (define source-files-to-build ::(list-of java.io.File)
    dependency-files)

  (define modules-to-regenerate ::(list-of ModuleTag)
    '())

  (define class-files-to-remove ::(list-of java.io.File)
    '())

  (define previous (load-mapping (join-path "build" "previous.map")))

  (define previous-package (previous 'package))
  
  (report "Checking which files need to be recompiled...")

  (for class::java.io.File in cached-files
    (let* ((scm ::java.io.File (source-file class))
	   (module ::ModuleTag
		   (file-module-tag scm)))
      (cond
       ((isnt scm in dependency-files)
	(unless (or (is scm in application-files)
		    (regex-match package (scm:toString))
		    (and previous-package
			 (regex-match previous-package (scm:toString))))
	  (report "Unknown source for "class": "scm)))
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
    (let ((modules-to-build (map file-module-tag
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

  (define skip-android-class-update? ::boolean
    (or (isnt 'android in targets)
	(and-let* (((equal? previous-package package))
		   (classes-dex ::java.io.File
				(existing-file
				 (join-path "build" "cache" "classes.dex")))
		   (classes-dex-update (classes-dex:lastModified))
		   ((every (is (_:lastModified) < classes-dex-update)
			   `(,@application-files
			     ,@dependency-files)))
		   (previous-init-dependencies (previous
						'init-dependencies))
		   ((is init-dependencies
			subset? previous-init-dependencies))))))

  (set! gnu.expr.Compilation:classPrefixDefault
	(string-append package "."))
  
  (concurrently
   (when (is 'desktop in targets)
     (build-zip (join-path "src" "grasp-desktop.scm")
		(main-class-file 'desktop)))

   (when (is 'terminal in targets)
     (build-zip (join-path "src" "grasp-terminal.scm")
		(main-class-file 'terminal)))

   (begin
     ;; this needs to be called last, because it changes
     ;; some of the global properties of the Kawa compiler
     (unless skip-android-class-update?
       (and-let* ((previous-package)
		  (previous-package-components
		   (string-split previous-package "."))
		  (previous-package-path
		   (join-path `("build" "cache" . ,previous-package-components)))
		  ((file-exists? previous-package-path))
		  (files-to-remove (list-files from: previous-package-path
					       such-that: 
					       (lambda (file::java.io.File)
						 (file:isFile)))))
	 (for file::java.io.File in files-to-remove
	   (report "deleting "file)
	   (file:delete))
	 (for depth from (length previous-package-components) to 1 by -1
	      (let ((directory
		     (join-path `("build" "cache" . ,(take depth previous-package-components)))))
		(report "deleting directory "directory)
		(delete-file directory))))
       
       (delete-if-exists (join-path "build" "cache" "classes.dex"))

       (build-file (join-path "src" "grasp-android.scm")
		   ;;target-directory: (join-path "build" "grasp-android")
		   top-class-name: (string-append package ".GRASP")))

     (report "Reindexing .class files...")
     (set! cached-files (list-files from: (join-path "build" "cache")
				    such-that: (is "[.]class$"
						   regex-match
						   (_:getPath))))
     (reset! module-classes)

     (for class::java.io.File in cached-files
       (let* ((scm ::java.io.File (source-file class))
	      (module (file-module-tag scm)))
	 (set! (module-classes module)
	       (union (module-classes module) `(,class)))))

     (unless skip-android-class-update?
       (report "building a list of classes to dex")

       (define classes-to-dex ::(list-of java.io.File)
	 (let ((android-dependencies ::(list-of java.io.File)
				     (append-map
				      module-classes
				      (fold-left
				       (lambda (dependencies module)
					 (union dependencies
						`(,module)
						(module-dependencies
						 module)))
				       (module-dependencies '(grasp-android))
				       (imported-modules
					(as-file init)
					(map file-module-tag
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

       (report "dexing "classes-to-dex)

       (update-dex-cache classes-to-dex (as-file "build" "cache"))

       (report "Generating the .dex index")

       (define dex-files
	 (list-files from: (join-path "build" "cache")
		     such-that: (is "[.]dex$" regex-match
				    (_:getPath))))

       (define dex-libraries
	 (list-files from: "libs"
		     such-that: (is "[.]dex$" regex-match
				    (_:getPath))))

       (report "Integrating the .dex files")

       (integrate-dex `(,@dex-libraries ,@dex-files)
		      (as-file "build" "cache"))
       )))
  
  (concurrently
   (when (is 'android in targets)
     (let ((file-name (join-path "build" (string-append name".apk"))))
       (report "building "file-name)
       (build-apk! output-name: file-name
		   init: init
		   package: package
		   icon: icon
		   keystore: keystore
		   key: key
		   label: name
		   assets: (append-map (lambda (source::string)
					 (list-files from: source))
				       assets)
		   password: password)))
   
   (when (is 'desktop in targets)
     (let ((file-name (join-path "build" (string-append name"-desktop.jar"))))
       (report "building "file-name)
       (build-jar!
	output-name: file-name
	module-dependencies: module-dependencies
	module-classes: module-classes
	available-modules: dependency-modules
	init: init
	package: package
	main-class: "grasp-desktop"
	assets: (append-map (lambda (source::string)
			      (list-files from: source))
			    assets)
	extra-dependencies: (list (join-path "libs" "jsvg-1.0.0.jar")))))

   (when (is 'terminal in targets)
     (let ((file-name (join-path "build" (string-append name"-terminal.jar"))))
       (report "building "file-name)
       (build-jar!
	output-name: file-name
	module-dependencies: module-dependencies
	module-classes: module-classes
	available-modules: dependency-modules
	init: init
	package: package
	main-class: "grasp-terminal"
	assets: (append-map (lambda (source::string)
			      (list-files from: source
					  such-that:
					  (lambda (file::java.io.File)
					    (none (is _ regex-match (file:getPath))
						  '(".svg$" ".ttf$" ".otf$")))))
			    assets)
	extra-dependencies:
	(list (join-path "libs" "lanterna-3.1.1.jar"))))))

  (set! (previous 'init-dependencies) init-dependencies)
  (set! (previous 'name) name)
  (set! (previous 'init) init)
  (set! (previous 'package) package)
  (save-mapping previous (join-path "build" "previous.map")))

