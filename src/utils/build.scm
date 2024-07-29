(module-name (utils build))

(import (kawa regex))
(import (language assert))
(import (language define-interface))
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
(import (utils android))
(import (utils file))
(import (utils print))

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

(define (imported-modules file::java.io.File
			  source-modules::(list-of (list-of symbol)))
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
		    module-dependencies::(maps ((list-of symbol))
					       to: (list-of
						    (list-of symbol)))
		    module-classes::(maps ((list-of symbol))
					  to: (list-of java.io.File))
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

#;(define (build #!key
	       (targets ::(list-of (either 'android
					  'terminal
					  'desktop))
		       '(android desktop terminal))
	       (name ::string "GRASP")
	       (icon ::string "icons/grasp.png")
	       (init ::string "init/init.scm")
	       (package ::string "io.github.panicz")
	       (keystore ::string "binary/keystore")
	       (key ::string "grasp-public")
	       (password ::string "untrusted"))

  (assert (or (is 'android in targets)
	      (is 'desktop in targets)
	      (is 'terminal in targets)))
  
  (define package-components (string-split package "."))
  
  (define previous (load-mapping "build/previous.map"))

  (define previous-package (previous 'package))
  
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

  (define (main-class-file target)::java.io.File
    (match target
      ('desktop (as-file "build/cache/grasp-desktop.zip"))
      ('terminal (as-file "build/cache/grasp-terminal.zip"))
      ('android (as-file "build/cache/classes.dex"))))
  
  (define main-class-files (map main-class-file targets))

  (unless (and previous-package
	       (string=? package previous-package)
	       ;; jeszcze trzeba tu dodac warunek, ze zaleznosci
	       ;; w init.scm sie nie zwiekszyly wzgledem
	       ;; poprzednich
	       (every file-exists? main-class-files)
	       (let ((main-class-update
		      (apply min (map (lambda (class::java.io.File)
					(class:lastModified))
				      main-class-files))))
		 (every (lambda (source::java.io.File)
			  (is (source:lastModified) < main-class-update))
			`(,@application-files ,@dependency-files))))
    (when previous-package
      (let* ((previous-package-components
	      (string-split previous-package "."))
	     (previous-package-path
	      (string-join previous-package-components
			   java.io.File:separator))
	     (files-to-remove (list-files from: previous-package-path
					  such-that: 
					  (lambda (file::java.io.File)
					    (file:isFile)))))
	(for file::java.io.File in files-to-remove
	  (file:delete))
	(for depth from (length previous-package-components) to 1 by -1
	     (delete-file
	      (string-join (take depth previous-package-components)
			   "/")))))

    (delete-if-exists "build/cache/classes.dex")

    )
  
  (concurrently
   (when (is 'android in targets)
     (build-apk! output-name: "build/grasp.apk"))
   
   (when (is 'desktop in targets)
     (build-jar!
      module-dependencies: module-dependencies
      module-classes: module-classes
      main-class: "grasp-desktop"
      extra-dependencies: '("libs/jsvg-1.0.0.jar")))

   (when (is 'terminal in targets)
     (build-jar!
      module-dependencies: module-dependencies
      module-classes: module-classes
      main-class: "grasp-terminal"
      assets: #f
      extra-dependencies:
      '("libs/lanterna-3.1.1.jar"))))
  
  (set! (previous 'name) name)
  (set! (previous 'init) init)
  (set! (previous 'package) package)
  (save-mapping previous "build/previous.map"))
