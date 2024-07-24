(module-name (utils build))

(import (kawa regex))
(import (language define-interface))
(import (language define-type))
(import (language define-property))
(import (language keyword-arguments))
(import (utils functions))
(import (language infix))
(import (language match))
(import (language mapping))
(import (language for))
(import (language define-cache))

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
	       (name ::string (let ((name ::gnu.lists.FString
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

(define-constant word-size ::uint 4)
(define-constant utf16-character-size ::uint 2)

(define-interface BinaryXML ()
  (tag)::uint
  (header-size)::uint
  (parse input-port)::BinaryXML
  (serialize)::(list-of ubyte))

(define-interface BinaryXMLChild ()
  (tag)::uint
  (header-size)::uint
  (parse input-port parent::AndroidXML)::BinaryXMLChild
  (serialize parent::AndroidXML)::(list-of ubyte))

(define-constant AndroidXMLDocumentTag ::uint #x00080003)
(define-constant AndroidXMLStringTableTag ::uint #x001C0001)
(define-constant AndroidXMLResourceTableTag ::uint #x00080180)
(define-constant AndroidXMLNamespaceStartTag ::uint #x00100100)
(define-constant AndroidXMLNamespaceEndTag ::uint #x00100101)
(define-constant AndroidXMLOpenTag ::uint #x00100102)
(define-constant AndroidXMLCloseTag ::uint #x00100103)
(define-constant AndroidXMLTextTag ::uint #x00100104)

(define-type (AndroidXMLStringTable content: (sequence-of CharSequence))
  implementing BinaryXML
  with
  ((tag)::uint AndroidXMLStringTableTag)

  ((header-size)::uint (* word-size 7))
  
  ((offsets)::(list-of ubyte)
   (fold-left
     (lambda (l::list n::integer)
       `(,@(little-endian-bytes-u32 n) . ,l))
     '()
     (drop 1 (fold-left (lambda (offsets::(list-of integer) s::string)
			  ::(list-of integer)
			  (match offsets
			    (`(,last . ,_)
			     `(,(+ last word-size (* utf16-character-size
						     (string-length s)))
			       . ,offsets))))
			'(0)
			content))))

  ((strings)::(list-of ubyte)
   (concatenate!
    (map (lambda (s::string)
	   `(,(string-length s) ,0
	     ,@(concatenate! (map (lambda (c::char)
			       `(,(char->integer c) ,0)) s))
	     ,0 ,0))
	 content)))
  
  ((serialize)::(list-of ubyte)
   (let* ((offset-table (offsets))
	  (string-table (strings))
	  (chunk-size (+ (header-size)
			 (length string-table)
			 (length offset-table)))
	  (strings-offset (+ (header-size) (length offset-table))))
     (append!
      (little-endian-bytes-u32 (tag))
      (little-endian-bytes-u32 chunk-size)
      (little-endian-bytes-u32 (length content))
      (little-endian-bytes-u32 0 #;styles)
      (little-endian-bytes-u32 0 #;flags)
      (little-endian-bytes-u32 strings-offset)
      (little-endian-bytes-u32 0 #;styles-offset)
      offset-table
      string-table)))

  ((index s::string)::uint
   (if s
       (escape-with return
	 (let ((i ::uint 0))
	   (for x in content
	     (when (string=? s x)
	       (return i))
	     (set! i (+ i 1)))
	   (return #xffffffff)))
       #xffffffff))
  
  ((parse input-port)::BinaryXML
   (let* ((read-u32le (lambda () (read-u32le input-port)))
	  (chunk-size (read-u32le))
	  (number-of-strings ::uint (read-u32le))
	  (number-of-styles (read-u32le))
	  (flags (read-u32le))
	  (string-offset (read-u32le))
	  (style-offset (read-u32le))
	  (offsets-table (make-bytevector
			  (* word-size number-of-strings)))
	  (strings-table (make-bytevector
			  (- chunk-size (header-size)
			     (* word-size number-of-strings)))))
     (read-bytevector! offsets-table input-port)
     (read-bytevector! strings-table input-port)

     (define (string-at index::uint)::String
       (let* ((offset (bytevector-u32le-ref offsets-table
					    (* word-size index)))
	      (builder ::java.lang.StringBuilder (java.lang.StringBuilder))
	      (size (strings-table offset)))
	 (for i from 1 to size
	      (builder:append (as char
				  (integer->char
				   (strings-table
				    (+ offset (* utf16-character-size
						 i)))))))
	 (builder:toString)))

     (let ((table (java.util.ArrayList number-of-strings)))       
       (for i from 0 below number-of-strings
	    (let ((s (string-at i)))
	      (table:add s)))
       (set! content table))
   
     (this))))

(define-type (AndroidXMLResourceTable
	      content: (sequence-of uint)
	      := '(#x101021b #x101021c #x1010572 #x1010573
			     #x101020c #x1010270 #x1010003
			     #x1010603 #x1010001 #x1010002
			     #x1010594 #x1010000 #x101001f))
  implementing BinaryXML
  with
  ((tag)::uint AndroidXMLResourceTableTag)
  ((header-size)::uint (* 2 word-size))
  ((parse input-port)::BinaryXML
   (let* ((read-u32le (lambda () (read-u32le input-port)))
	  (chunk-size (read-u32le))
	  (number-of-resources ::uint (/ (- chunk-size (header-size))
					 word-size)))
     (set! content (java.util.ArrayList number-of-resources))
     (for i from 0 below number-of-resources
	  (content:add (read-u32le))))
   (this))
     
  ((serialize)::(list-of ubyte)
   (append!
    (little-endian-bytes-u32 (tag))
    (little-endian-bytes-u32 (+ (* (length content) word-size)
				(header-size)))
    (concatenate! (map little-endian-bytes-u32 content)))))

(define-bimapping (AndroidXMLAttributeTypeName code::uint)::symbol
  (error "Unknown AndroidXMLAttribute type" code))
  
(set! (AndroidXMLAttributeTypeName (as uint #x01000008)) 'id-reference)
(set! (AndroidXMLAttributeTypeName (as uint #x02000008)) 'attribute-reference)
(set! (AndroidXMLAttributeTypeName (as uint #x03000008)) 'string)
(set! (AndroidXMLAttributeTypeName (as uint #x05000008)) 'dimension)
(set! (AndroidXMLAttributeTypeName (as uint #x06000008)) 'fraction)
(set! (AndroidXMLAttributeTypeName (as uint #x10000008)) 'int)
(set! (AndroidXMLAttributeTypeName (as uint #x04000008)) 'float)
(set! (AndroidXMLAttributeTypeName (as uint #x11000008)) 'flags)
(set! (AndroidXMLAttributeTypeName (as uint #x12000008)) 'bool)
(set! (AndroidXMLAttributeTypeName (as uint #x1C000008)) 'color)
(set! (AndroidXMLAttributeTypeName (as uint #x1D000008)) 'color2)

(define-type (AndroidXMLNamespace prefix: string
				  uri: string
				  line: uint := 0
				  closing-line: uint := 0)
  implementing BinaryXMLChild
  with
  ((tag)::uint AndroidXMLNamespaceStartTag)
  ((header-size)::uint (* 6 word-size))
  ((parse input-port parent ::AndroidXML)::BinaryXMLChild
   (let* ((next-word (lambda () (read-u32le input-port)))
	  (chunk-size (next-word))
	  (line-number (next-word))
	  (comment (next-word))
	  (prefix-index (next-word))
	  (uri-index (next-word)))
     (assert (= chunk-size (header-size)))
     (set! line line-number)
     (set! prefix (parent:string-table:content prefix-index))
     (set! uri (parent:string-table:content uri-index))
     (this)))
   
  ((serialize parent ::AndroidXML)::(list-of ubyte)
   (append!
    (little-endian-bytes-u32 (tag))
    (little-endian-bytes-u32 (header-size))
    (little-endian-bytes-u32 line)
    (little-endian-bytes-u32 #xffffffff #;comment)
    (little-endian-bytes-u32 (parent:string-table:index prefix))
    (little-endian-bytes-u32 (parent:string-table:index uri))))
  
  ((serialize/close parent ::AndroidXML)::(list-of ubyte)
   (append!
    (little-endian-bytes-u32 AndroidXMLNamespaceEndTag)
    (little-endian-bytes-u32 (header-size))
    (little-endian-bytes-u32 closing-line)
    (little-endian-bytes-u32 #xffffffff #;comment)
    (little-endian-bytes-u32 (parent:string-table:index prefix))
    (little-endian-bytes-u32 (parent:string-table:index uri))))

  ((parse/close input-port parent ::AndroidXML)::BinaryXMLChild
   (let* ((next-word (lambda () (read-u32le input-port)))
	  (chunk-size (next-word))
	  (line-number (next-word))
	  (comment (next-word))
	  (prefix-index (next-word))
	  (uri-index (next-word)))
     (assert (= chunk-size (header-size)))
     (assert (string=? prefix (parent:string-table:content prefix-index)))
     (assert (string=? uri (parent:string-table:content uri-index)))
     (set! closing-line line-number)
     (this)))
   )

(define-type (AndroidXMLTagAttribute
	      name: string
	      value: string
	      value-type: symbol
	      resource: uint
	      namespace: string)
  implementing BinaryXMLChild
  with
  ((tag)::uint (error "Attributes are untagged"))
  ((header-size) (* word-size 5))
  ((parse input-port parent ::AndroidXML)::AndroidXMLTagAttribute
   (let* ((next-word (lambda () (read-u32le input-port)))
	  (namespace-uri-index (next-word))
	  (name-index (next-word))
	  (value-index (next-word))
	  (type (next-word))
	  (data (next-word)))
     (set! name (if (= name-index #xffffffff)
		    #!null
		    (parent:string-table:content name-index)))
     (set! namespace (if (= namespace-uri-index #xffffffff)
			 #!null
			 (parent:string-table:content namespace-uri-index)))
     (set! value (if (= value-index #xffffffff)
		     #!null
		     (parent:string-table:content value-index)))
     (set! value-type (AndroidXMLAttributeTypeName type))
     (set! resource data))
   (this))
  ((serialize parent ::AndroidXML)::(list-of ubyte)
   (let ((result
	  (append!
	   (little-endian-bytes-u32 (parent:string-table:index namespace))
	   (little-endian-bytes-u32 (parent:string-table:index name))
	   (little-endian-bytes-u32 (parent:string-table:index value))
	   (little-endian-bytes-u32 ((inverse AndroidXMLAttributeTypeName)
				     value-type))
	   (little-endian-bytes-u32 resource))))
     result))
  )

(define-type (AndroidXMLTagClose name: string
				 line: uint := 0
				 namespace-uri: string)
  implementing BinaryXMLChild
  with
  ((tag)::uint AndroidXMLCloseTag)
  ((header-size)::uint (* 6 word-size))
  ((parse input-port parent ::AndroidXML)::BinaryXMLChild
   (let* ((next-word (lambda () (read-u32le input-port)))
	  (chunk-size (next-word))
	  (line-number (next-word))
	  (comment (next-word))
	  (namespace-uri-index (next-word))
	  (element-name-index (next-word)))
     (set! line line-number)
     (set! name (parent:string-table:content element-name-index))
     (set! namespace-uri (if (= namespace-uri-index #xffffffff)
			     #!null
			     (parent:string-table:content
			      namespace-uri-index)))
     (this)))
  ((serialize parent ::AndroidXML)::(list-of ubyte)
   (let ((result 
	  (append!
	   (little-endian-bytes-u32 (tag))
	   (little-endian-bytes-u32 (header-size))
	   (little-endian-bytes-u32 line)
	   (little-endian-bytes-u32 #xffffffff #;comment)
	   (little-endian-bytes-u32 (if namespace-uri
					(parent:string-table:index
					 namespace-uri)
					#xffffffff))
	   (little-endian-bytes-u32 (parent:string-table:index name)))))
     result)))

(define-type (AndroidXMLTag name: string
			    line: uint := 0
			    namespace-uri: string
			    attributes: java.util.List
			    := (java.util.ArrayList))
  implementing BinaryXMLChild
  with
  ((tag)::uint AndroidXMLOpenTag)
  ((header-size)::uint (* 9 word-size))
  ((parse input-port parent ::AndroidXML)::BinaryXMLChild
   (let* ((next-word (lambda () (read-u32le input-port)))
	  (chunk-size (next-word))
	  (line-number (next-word))
	  (comment (next-word))
	  (namespace-uri-index (next-word))
	  (element-name-index (next-word))
	  (x140014 (next-word))
	  (number-of-attributes (next-word))
	  (x000000 (next-word)))
     (assert (= x140014 #x140014))
     (assert (= x000000 #x000000))
     (set! attributes (java.util.ArrayList))
     (set! line line-number)
     (set! name (parent:string-table:content element-name-index))
     (set! namespace-uri (if (= namespace-uri-index #xffffffff)
			     #!null
			     (parent:string-table:content
			      namespace-uri-index)))
     (for i from 0 below number-of-attributes
	  (let ((attribute ::AndroidXMLTagAttribute
			   (AndroidXMLTagAttribute)))
	    (attribute:parse input-port parent)
	    (attributes:add attribute))))
   (this))
  ((serialize parent ::AndroidXML)::(list-of ubyte)
   (let* ((serialized-attributes
	   (concatenate!
	    (map (lambda (attribute ::AndroidXMLTagAttribute)
		   ::(list-of ubyte)
		   (attribute:serialize parent))
		 attributes)))
	  (result
	   (append!
	    (little-endian-bytes-u32 (tag))
	    (little-endian-bytes-u32 (+ (header-size)
					(length
					 serialized-attributes)))
	    (little-endian-bytes-u32 line)
	    (little-endian-bytes-u32 #xffffffff #;comment)
	    (little-endian-bytes-u32 (if namespace-uri
					 (parent:string-table:index
					  namespace-uri)
					 #xffffffff))
	    (little-endian-bytes-u32 (parent:string-table:index name))
	    (little-endian-bytes-u32 #x140014)
	    (little-endian-bytes-u32 (length attributes))
	    (little-endian-bytes-u32 0)
	    serialized-attributes)))
     result)))


(define-type (AndroidXML string-table: AndroidXMLStringTable
			 resource-table: AndroidXMLResourceTable
			 namespace: AndroidXMLNamespace
			 tags: java.util.List := (java.util.ArrayList))
  implementing BinaryXML
  with
  ((tag)::uint AndroidXMLDocumentTag)

  ((header-size)::uint (* 2 word-size))
  
  ((parse input-port)::BinaryXML
   (let* ((read-u32le (lambda () (read-u32le input-port)))
	  (document-tag (read-u32le))
	  (document-size (read-u32le)))

     (assert (= document-tag (tag)))
     
     (set! string-table (AndroidXMLStringTable))
     (let ((string-table-tag (read-u32le)))
       (assert (= string-table-tag (string-table:tag))))
     (string-table:parse input-port)
     
     (set! resource-table (AndroidXMLResourceTable))
     (let ((resource-table-tag (read-u32le)))
       (assert (= resource-table-tag (resource-table:tag))))
     (resource-table:parse input-port)
     
     (set! namespace (AndroidXMLNamespace parent: (this)))
     (let ((namespace-tag (read-u32le)))
       (assert (= namespace-tag (namespace:tag))))
     (namespace:parse input-port (this))

     (let loop ()
       (let ((tag (read-u32le)))
	 (match tag
	   (,AndroidXMLNamespaceEndTag
	    (namespace:parse/close input-port (this))
	    (this))
	   (,AndroidXMLOpenTag
	    (let ((tag ::AndroidXMLTag (AndroidXMLTag)))
	      (tag:parse input-port (this))
	      (tags:add tag)
	      (loop)))
	   (,AndroidXMLCloseTag
	    (let ((tag ::AndroidXMLTagClose (AndroidXMLTagClose)))
	      (tag:parse input-port (this))
	      (tags:add tag)
	      (loop))))))))
  ((serialize)::(list-of ubyte)
   (let* ((strings (string-table:serialize))
	  (resources (resource-table:serialize))
	  (provision (namespace:serialize (this)))
	  (tags (concatenate! (map (lambda (tag::BinaryXMLChild)
					(tag:serialize (this)))
				   tags)))
	  (end-namespace (namespace:serialize/close (this))))
     (let ((content (append!
		     strings
		     resources
		     provision
		     tags
		     end-namespace)))
       (append!
	(little-endian-bytes-u32 (tag))
	(little-endian-bytes-u32 (+ (header-size) (length content)))
	content))))
  )

(define (AndroidManifest #!key (package ::string "io.github.grasp")
			 (label ::string "GRASP"))
  ::AndroidXML
  (AndroidXML
   string-table:
   (AndroidXMLStringTable
    content:
    (vector
     "versionCode"
     "versionName"
     "compileSdkVersion"
     "compileSdkVersionCodename"
     "minSdkVersion"
     "targetSdkVersion"
     "name"
     "requestLegacyExternalStorage"
     "label"
     "icon"
     "shell"
     "theme"
     "configChanges"
     "android"
     "http://schemas.android.com/apk/res/android"
     ""
     "package"
     "platformBuildVersionCode"
     "platformBuildVersionName"
     "manifest"
     package
     "1.0"
     "13"
     "33"
     "uses-sdk"
     "uses-permission"
     "android.permission.WAKE_LOCK"
     "android.permission.READ_EXTERNAL_STORAGE"
     "android.permission.WRITE_EXTERNAL_STORAGE"
     "android.permission.MANAGE_EXTERNAL_STORAGE"
     "android.permission.RECORD_AUDIO"
     "android.permission.INTERNET"
     "application"
     label
     "profileable"
     "activity"
     ".GRASP"
     "intent-filter"
     "action"
     "android.intent.action.MAIN"
     "category"
     "android.intent.category.LAUNCHER"
     "queries"
     "intent"
     "android.intent.action.TTS_SERVICE"
     "android.speech.RecognitionService"))
   resource-table:
   (AndroidXMLResourceTable
    content:
    (vector 16843291 16843292 16844146 16844147 16843276
	    16843376 16842755 16844291 16842753
	    16842754 16844180 16842752 16842783))
   namespace:
   (AndroidXMLNamespace
    prefix: "android"
    uri: "http://schemas.android.com/apk/res/android"
    line: 2
    closing-line: 58)
   tags:
   (vector
    (AndroidXMLTag
      name: "manifest"
      line: 2
      namespace-uri: #!null
      attributes:
      (vector
       (AndroidXMLTagAttribute
	 name: "versionCode"
	 value: #!null
	 value-type: 'int
	 resource: 1
	 namespace: "http://schemas.android.com/apk/res/android")
	(AndroidXMLTagAttribute
	 name: "versionName"
	 value: "1.0"
	 value-type: 'string
	 resource: 21
	 namespace: "http://schemas.android.com/apk/res/android")
	(AndroidXMLTagAttribute
	 name: "compileSdkVersion"
	 value: #!null
	 value-type: 'int
	 resource: 33
	 namespace: "http://schemas.android.com/apk/res/android")
	(AndroidXMLTagAttribute
	 name: "compileSdkVersionCodename"
	 value: "13"
	 value-type: 'string
	 resource: 22
	 namespace: "http://schemas.android.com/apk/res/android")
	(AndroidXMLTagAttribute
	 name: "package"
	 value: package
	 value-type: 'string
	 resource: 20
	 namespace: #!null)
	(AndroidXMLTagAttribute
	 name: "platformBuildVersionCode"
	 value: "33"
	 value-type: 'int
	 resource: 33
	 namespace: #!null)
	(AndroidXMLTagAttribute
	 name: "platformBuildVersionName"
	 value: "13"
	 value-type: 'int
	 resource: 13
	 namespace: #!null)))
     (AndroidXMLTag
      name: "uses-sdk"
      line: 7
      namespace-uri: #!null
      attributes:
      (vector
       (AndroidXMLTagAttribute
	name: "minSdkVersion"
	value: #!null
	value-type: 'int
	resource: 23
	namespace: "http://schemas.android.com/apk/res/android")
       (AndroidXMLTagAttribute
	name: "targetSdkVersion"
	value: #!null
	value-type: 'int
	resource: 29
	namespace: "http://schemas.android.com/apk/res/android")))
     (AndroidXMLTagClose
      name: "uses-sdk"
      line: 8
      namespace-uri: #!null)
     (AndroidXMLTag
      name: "uses-permission"
      line: 10
      namespace-uri: #!null
      attributes:
      (vector
       (AndroidXMLTagAttribute
	name: "name"
	value: "android.permission.WAKE_LOCK"
	value-type: 'string
	resource: 26
	namespace: "http://schemas.android.com/apk/res/android")))
     (AndroidXMLTagClose
      name: "uses-permission"
      line: 12
      namespace-uri: #!null)
     (AndroidXMLTag
      name: "uses-permission"
      line: 13
      namespace-uri: #!null
      attributes:
      (vector
       (AndroidXMLTagAttribute
	name: "name"
	value: "android.permission.READ_EXTERNAL_STORAGE"
	value-type: 'string
	resource: 27
	namespace: "http://schemas.android.com/apk/res/android")))
     (AndroidXMLTagClose
      name: "uses-permission"
      line: 15
      namespace-uri: #!null)
     (AndroidXMLTag
      name: "uses-permission"
      line: 16
      namespace-uri: #!null
      attributes:
      (vector
       (AndroidXMLTagAttribute
	name: "name"
	value: "android.permission.WRITE_EXTERNAL_STORAGE"
	value-type: 'string
	resource: 28
	namespace: "http://schemas.android.com/apk/res/android")))
     (AndroidXMLTagClose
      name: "uses-permission"
      line: 18
      namespace-uri: #!null)
     (AndroidXMLTag
      name: "uses-permission"
      line: 19
      namespace-uri: #!null
      attributes:
      (vector
       (AndroidXMLTagAttribute
	name: "name"
	value: "android.permission.MANAGE_EXTERNAL_STORAGE"
	value-type: 'string
	resource: 29
	namespace: "http://schemas.android.com/apk/res/android")))
     (AndroidXMLTagClose
      name: "uses-permission"
      line: 21
      namespace-uri: #!null)
     (AndroidXMLTag
      name: "uses-permission"
      line: 22
      namespace-uri: #!null
      attributes:
      (vector
       (AndroidXMLTagAttribute
	name: "name"
	value: "android.permission.RECORD_AUDIO"
	value-type: '
	string
	resource: 30
	namespace: "http://schemas.android.com/apk/res/android")))
     (AndroidXMLTagClose
      name: "uses-permission"
      line: 24
      namespace-uri: #!null)
     (AndroidXMLTag
      name: "uses-permission"
      line: 25
      namespace-uri: #!null
      attributes:
      (vector
       (AndroidXMLTagAttribute
	name: "name"
	value: "android.permission.INTERNET"
	value-type: 'string
	resource: 31
	namespace: "http://schemas.android.com/apk/res/android")))
     (AndroidXMLTagClose
      name: "uses-permission"
      line: 27
      namespace-uri: #!null)
     (AndroidXMLTag
      name: "application"
      line: 29
      namespace-uri: #!null
      attributes:
      (vector
       (AndroidXMLTagAttribute
	name: "label"
	value: label
	value-type: 'string
	resource: 33
	namespace: "http://schemas.android.com/apk/res/android")
       (AndroidXMLTagAttribute
	name: "icon"
	value: #!null
	value-type: 'id-reference
	resource: 2130837504
	namespace: "http://schemas.android.com/apk/res/android")
       (AndroidXMLTagAttribute
	name: "requestLegacyExternalStorage"
	value: #!null
	value-type: 'bool
	resource: -1
	namespace: "http://schemas.android.com/apk/res/android")))
     (AndroidXMLTag
      name: "profileable"
      line: 33
      namespace-uri: #!null
      attributes:
      (vector
       (AndroidXMLTagAttribute
	name: "shell"
	value: #!null
	value-type: 'bool resource: -1
	namespace: "http://schemas.android.com/apk/res/android")))
     (AndroidXMLTagClose
      name: "profileable"
      line: 33
      namespace-uri: #!null)
     (AndroidXMLTag
      name: "activity"
      line: 34
      namespace-uri: #!null
      attributes:
      (vector
       (AndroidXMLTagAttribute
	name: "theme"
	value: #!null
	value-type: 'id-reference
	resource: 16973830
	namespace: "http://schemas.android.com/apk/res/android")
       (AndroidXMLTagAttribute
	name: "label"
	value: label
	value-type: 'string
	resource: 33
	namespace: "http://schemas.android.com/apk/res/android")
       (AndroidXMLTagAttribute
	name: "name"
	value: ".GRASP"
	value-type: 'string
	resource: 36
	namespace: "http://schemas.android.com/apk/res/android")
       (AndroidXMLTagAttribute
	name: "configChanges"
	value: #!null
	value-type: 'flags
	resource: 176
	namespace: "http://schemas.android.com/apk/res/android")))
     (AndroidXMLTag
      name: "intent-filter"
      line: 39
      namespace-uri: #!null
      attributes: #())
     (AndroidXMLTag
      name: "action"
      line: 40
      namespace-uri: #!null
      attributes:
      (vector
       (AndroidXMLTagAttribute
	name: "name"
	value: "android.intent.action.MAIN"
	value-type: 'string
	resource: 39
	namespace: "http://schemas.android.com/apk/res/android")))
     (AndroidXMLTagClose
      name: "action"
      line: 41
      namespace-uri: #!null)
     (AndroidXMLTag
      name: "category"
      line: 42
      namespace-uri: #!null
      attributes:
      (vector
       (AndroidXMLTagAttribute
	name: "name"
	value: "android.intent.category.LAUNCHER"
	value-type: 'string
	resource: 41
	namespace: "http://schemas.android.com/apk/res/android")))
     (AndroidXMLTagClose
      name: "category"
      line: 43
      namespace-uri: #!null)
     (AndroidXMLTagClose
      name: "intent-filter"
      line: 44
      namespace-uri: #!null)
     (AndroidXMLTagClose
      name: "activity"
      line: 45
      namespace-uri: #!null)
     (AndroidXMLTagClose
      name: "application"
      line: 47
      namespace-uri: #!null)
     (AndroidXMLTag
      name: "queries"
      line: 49
      namespace-uri: #!null
      attributes: #())
     (AndroidXMLTag
      name: "intent"
      line: 50
      namespace-uri: #!null
      attributes: #())
     (AndroidXMLTag
      name: "action"
      line: 51
      namespace-uri: #!null
      attributes:
      (vector
       (AndroidXMLTagAttribute
	name: "name"
	value: "android.intent.action.TTS_SERVICE"
	value-type: 'string
	resource: 44
	namespace: "http://schemas.android.com/apk/res/android")))
     (AndroidXMLTagClose
      name: "action"
      line: 51
      namespace-uri: #!null)
     (AndroidXMLTagClose
      name: "intent"
      line: 52
      namespace-uri: #!null)
     (AndroidXMLTag
      name: "intent"
      line: 53
      namespace-uri: #!null
      attributes: #())
     (AndroidXMLTag
      name: "action"
      line: 54
      namespace-uri: #!null
      attributes:
      (vector
       (AndroidXMLTagAttribute
	name: "name"
	value: "android.speech.RecognitionService"
	value-type: 'string
	resource: 45
	namespace: "http://schemas.android.com/apk/res/android")))
     (AndroidXMLTagClose
      name: "action"
      line: 54
      namespace-uri: #!null)
     (AndroidXMLTagClose
      name: "intent"
      line: 55
      namespace-uri: #!null)
     (AndroidXMLTagClose
      name: "queries"
      line: 56
      namespace-uri: #!null)
     (AndroidXMLTagClose
      name: "manifest"
      line: 58
      namespace-uri: #!null))))
