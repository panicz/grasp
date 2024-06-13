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

(import (utils file))

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

(define (imported-modules contents
			  source-modules)::(list-of (list-of symbol))
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
  (let ((dependencies (mapping (module) '()))
	(source-modules (map internal-module-name files)))
    (for file ::java.io.File in files
      (let* ((contents (with-input-from-file (file:getPath) read-all))
	     (imports (imported-modules contents source-modules))
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


(define (target-file-name+directory source-file ::java.io.File)
  ::(Values string java.io.File)
  (match (regex-match "^src(/[^.]*)[.]scm$" (source-file:getPath))
    (`(,_ ,stem)
     (match (regex-match "(.*)/([^/]*)$" stem)
       (`(,_ ,path ,name)
	(values
	 (string-append name ".zip")
	 (as-file (string-append "build/cache/" path))))))))
