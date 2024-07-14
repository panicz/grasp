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
  (class:delete))

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
				 (as-file "init/init.scm")
				 (map internal-module-name
				      dependency-files))))))
    (only
     (lambda (class-file::java.io.File)
       (let ((path ::String (class-file:getPath)))
	 (match (regex-match "^(.*)[.]class$" path)
	   (`(,_ ,stem)
	    (let ((dex-file ::java.io.File (as-file
					    (string-append stem ".dex"))))
	      (and
	       (or (not (dex-file:exists))
		   (is (dex-file:lastModified) <= (class-file:lastModified)))
	       (or (is class-file in android-dependencies)
		   (is "io.github" regex-match path))))))))
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

(define (resources-arsc package-name ::string)::bytevector
  (list->u8vector
   (append!
    (list
     #x02 #x00 #x0c #x00 #x50 #x02 #x00 #x00
     #x01 #x00 #x00 #x00 #x01 #x00 #x1c #x00 
     #x38 #x00 #x00 #x00 #x01 #x00 #x00 #x00
     #x00 #x00 #x00 #x00 #x00 #x01 #x00 #x00 
     #x20 #x00 #x00 #x00 #x00 #x00 #x00 #x00
     #x00 #x00 #x00 #x00 #x15 #x15 #x72 #x65
     #x73 #x2f #x64 #x72 #x61 #x77 #x61 #x62
     #x6c #x65 #x2f #x69 #x63 #x6f #x6e #x2e
     #x70 #x6e #x67 #x00 #x00 #x02 #x20 #x01
     #x0c #x02 #x00 #x00 #x7f #x00 #x00 #x00)
    (concatenate!
     (map (lambda (c)
	    `(,(char->integer c) ,#x00))
	  package-name))
    (make-list (- 256 (* 2 (length package-name))) 0)
    (list
     #x20 #x01 #x00 #x00 #x02 #x00 #x00 #x00
     #x58 #x01 #x00 #x00 #x01 #x00 #x00 #x00
     #x00 #x00 #x00 #x00 #x01 #x00 #x1c #x00
     #x38 #x00 #x00 #x00 #x02 #x00 #x00 #x00
     #x00 #x00 #x00 #x00 #x00 #x01 #x00 #x00
     #x24 #x00 #x00 #x00 #x00 #x00 #x00 #x00
     #x00 #x00 #x00 #x00 #x07 #x00 #x00 #x00
     #x04 #x04 #x61 #x74 #x74 #x72 #x00 #x08
     #x08 #x64 #x72 #x61 #x77 #x61 #x62 #x6c
     #x65 #x00 #x00 #x00 #x01 #x00 #x1c #x00
     #x28 #x00 #x00 #x00 #x01 #x00 #x00 #x00
     #x00 #x00 #x00 #x00 #x00 #x01 #x00 #x00
     #x20 #x00 #x00 #x00 #x00 #x00 #x00 #x00
     #x00 #x00 #x00 #x00 #x04 #x04 #x69 #x63
     #x6f #x6e #x00 #x00 #x02 #x02 #x10 #x00
     #x10 #x00 #x00 #x00 #x01 #x00 #x00 #x00
     #x00 #x00 #x00 #x00 #x02 #x02 #x10 #x00
     #x14 #x00 #x00 #x00 #x02 #x00 #x00 #x00
     #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00
     #x01 #x02 #x54 #x00 #x68 #x00 #x00 #x00
     #x02 #x00 #x00 #x00 #x01 #x00 #x00 #x00
     #x58 #x00 #x00 #x00 #x40 #x00 #x00 #x00
     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
     #x08 #x00 #x00 #x00 #x00 #x00 #x00 #x00
     #x08 #x00 #x00 #x03 #x00 #x00 #x00 #x00))))

(define (android-manifest
	 #!key
	 (package ::string "systems.grasp")
	 (min-sdk ::integer 23)
	 (target-sdk ::integer 29)
	 (permissions ::(list-of string) '("WAKE_LOCK"
					   "READ_EXTERNAL_STORAGE"
					   "WRITE_EXTERNAL_STORAGE"
					   "MANAGE_EXTERNAL_STORAGE"
					   "RECORD_AUDIO"
					   "INTERNET"))
	 (request-legacy-external-storage ::boolean #t)
	 (application-name ::string "GRASP"))
  ::String
  (string-append "\
<?xml version=\"1.0\" encoding=\"utf-8\"?>
<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"
package=\""package"\" android:versionCode=\"1\" android:versionName=\"1.0\">
<uses-sdk android:minSdkVersion=\""(number->string min-sdk)"\"
	  android:targetSdkVersion=\""(number->string target-sdk)"\" />
"(string-join (map (lambda (permission)
		     (string-append "\
<uses-permission android:name=\"android.permission."permission"\" />"))
		   permissions)
	      "\n")"
  <application "(if request-legacy-external-storage
		    "android:requestLegacyExternalStorage=\"true\""
		    "")"
      android:label=\""application-name"\"
      android:icon=\"@drawable/icon\">
    <profileable android:shell=\"true\" />
    <activity android:name=\".GRASP\"
        android:label=\""application-name"\"
	android:theme=\"@android:style/Theme.NoTitleBar\"
	android:configChanges=\"keyboard|keyboardHidden|orientation\">
      <intent-filter>
        <action android:name=\"android.intent.action.MAIN\" />
        <category android:name=\"android.intent.category.LAUNCHER\" />
      </intent-filter>
    </activity>

  </application>

  <queries>
    <intent>
      <action android:name=\"android.intent.action.TTS_SERVICE\" />
    </intent>
    <intent>
      <action android:name=\"android.speech.RecognitionService\" />
    </intent>
  </queries>

</manifest>
"))

(define (build-apk! #!key
		    (init ::string "init/init.scm")
		    (package ::string "io.github.grasp")
		    (icon ::string "icons/grasp.png")
		    (keystore ::string "binary/keystore")
		    (password ::string "untrusted")
		    (key-alias ::string "grasp-public")
		    (output-name ::string "build/grasp.apk"))
  (let* ((apk-file ::java.io.File (as-file output-name))
	 (temp-file ::java.io.File (java.io.File:createTempFile
				    "grasp-" ".apk"
				    (as-file "build")))
	 (assets (list-files from: "assets"))
	 (output (ZipBuilder temp-file)))
    (output:add-file-as! "res/drawable/icon.png" (as-file icon))
    (for asset in assets
      (output:add-file-at-level! 0 asset))
    (output:add-file-as! "assets/init.scm" (as-file init))
    #;(let* ((manifest ::String (android-manifest
			       package: package
			       application-name: "GRASP"))
	   (encoder ::com.bigzhao.xml2axml.Encoder
		    (com.bigzhao.xml2axml.Encoder))
	   (axml ::(array-of byte) (encoder:encodeString
				    (android.content.Context)
				    manifest)))
          (output:add-file-with-bytes! axml
				       (as-file "AndroidManifest.xml")))
    (output:add-file-with-binary-content! (resources-arsc package)
					  (as-file "resources.arsc"))
    (output:add-file-at-level! 2 (as-file "build/cache/classes.dex"))
    (output:close)
    (com.iyxan23.zipalignjava.ZipAlign:alignZip
     (java.io.RandomAccessFile temp-file "r")
     (FileOutputStream (as java.io.File apk-file)))
    (temp-file:delete)
    (let ((args ::(array-of String)
		((array-of String) "sign"
		 "--ks" keystore
		 "--ks-key-alias" key-alias
		 "--ks-pass" (string-append "pass:" password)
		 "--min-sdk-version" "23"
		 apk-file)))
      (com.android.apksigner.ApkSignerTool:main args))))

(print "generating build/grasp.apk")
(build-apk! output-name: "build/grasp.apk")


#;(concurrently
 (build-jar!
  module-dependencies: module-dependencies
  module-classes: module-classes
  main-class: "grasp-desktop"
  extra-dependencies: '("libs/jsvg-1.0.0.jar"))

 (build-jar!
  module-dependencies: module-dependencies
  module-classes: module-classes
  main-class: "grasp-terminal"
  assets: #f
  extra-dependencies: '("libs/lanterna-3.1.1.jar")))
