(module-name (utils file))

(import (language assert))
(import (language define-type))
(import (language while))
(import (language for))
(import (language fundamental))
(import (language infix))
(import (language mapping))
(import (utils functions))


(define-alias ZipEntry java.util.zip.ZipEntry)
(define-alias ZipFile java.util.zip.ZipFile)
(define-alias ZipOutputStream java.util.zip.ZipOutputStream)
(define-alias ZipInputStream java.util.zip.ZipInputStream)
(define-alias FileOutputStream java.io.FileOutputStream)
(define-alias FileInputStream java.io.FileInputStream)
(define-alias InputStream java.io.InputStream)
(define-alias OutputStream java.io.OutputStream)

(define (file->path file::java.io.File)::java.nio.file.Path
  (file:toPath))

(define (as-file path . fragments)::java.io.File
  (if (and (java.io.File? path)
	   (null? fragments))
      path
      (java.io.File (as String
			(string-join `(,path . ,fragments)
				     java.io.File:separator)))))

(define (join-path . fragments)::String
  (string-join fragments java.io.File:separator))

(define (save-mapping mapping ::(maps (Object)
				      to: Object)
		      filename ::string)
  ::void
  (call-with-output-file filename
    (lambda (port)
      (for key in (keys mapping)
	(write key port)
	(display #\space port)
	(write (mapping key) port)
	(newline port)))))

(define (load-mapping filename ::string
		      #!key (into (mapping (any)
				    #!null)))
  ::(maps (Object) to: Object)
  (when (file-exists? filename)
    (call-with-input-file filename
      (lambda (port)
	(let loop ()
	  (let* ((key (read port))
		 (value (read port)))
	    (unless (or (eof-object? key)
			(eof-object? value))
	      (set! (into key) value)
	      (loop)))))))
  into)

;; this is a workaround that allows us to call
;; the function from an i terpreter on Android
;; (which has problems with keyword arguments)
(define (load-mapping-from
	 filename ::string
	 destination ::(!maps (Object)
			      to: Object))
  (load-mapping filename into: destination))


(define (list-files #!key
		    (from ::(either string java.io.File) ".")
		    (such-that ::(maps (java.io.File) to: boolean) always)
		    (max-depth +inf.0))
  ::(list-of java.io.File)
  (let ((source ::java.io.File (as-file from)))
    (cond
     ((source:isDirectory)
      (let ((result '()))
	(for file ::java.io.File in (source:listFiles)
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
     ((source:exists)
      `(,source))
     (else
      '()))))

(define (unzip archive ::string #!key (into ::string "."))::void
  ;;(print"decompressing "archive" into "into)
  (let* ((dir ::java.io.File (java.io.File (as String into)))
	 (buffer ::(array-of byte) ((array-of byte) length: 1024))
	 (data ::FileInputStream
	       (java.io.FileInputStream (java.io.File archive)))
	 (source ::ZipInputStream (ZipInputStream data)))
    (let next-entry ()
      (let ((entry ::ZipEntry (source:getNextEntry)))
	(when entry
	  ;;(print"deflating "entry)
	  (let ((file ::java.io.File (java.io.File
				      dir (entry:getName))))
	    (if (entry:isDirectory)
		(file:mkdirs)
		(let ((parent ::java.io.File (file:getParentFile)))
		  (parent:mkdirs)
		  (let ((output ::FileOutputStream
				(FileOutputStream file)))
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

(define (existing-file filename ::string)::java.io.File
  (let ((file ::java.io.File
	      (java.io.File (as String filename))))
    (if (file:exists)
	file
	#!null)))

(define (rewrite! input-stream ::InputStream
	       #;to output-stream ::OutputStream
	       #;via buffer ::(array-of byte))
  (let loop ()
    (let ((n ::int (input-stream:read buffer)))
      (unless (= n -1)
	(output-stream:write buffer 0 n)
	(loop)))))

(define-simple-class ZipBuilder ()
  (file-output type: FileOutputStream)
  (output type: ZipOutputStream)

  (buffer type: (array-of byte)
	  init-value: ((array-of byte) length: 4096))
  
  ((append-entries-unless! exclude::(maps (ZipEntry)
					  to: boolean)
			   file::ZipFile)
   ::void
   (let ((entries ::java.util.Enumeration
		  (file:entries)))
     (while (entries:hasMoreElements)
       (let* ((entry ::ZipEntry (entries:nextElement))
	      (input ::InputStream (file:getInputStream entry)))
	 (unless (exclude entry)
	   (output:putNextEntry entry)
	   (unless (entry:isDirectory)
	     (rewrite! #;from input #;to output #;via buffer))
	   (input:close)
	   (output:closeEntry))))))

  ((append-entries! file::ZipFile)::void
   (append-entries-unless! (lambda (file::ZipEntry) #f) file))

  ((add-file-at-level! level::int file::java.io.File)::void
   (let* ((source-path ::string (file:getPath))
	  (components ::(list-of string) (string-split source-path "/"))
	  (adjusted (drop level components))
	  (target-path (string-join adjusted "/")))
     (add-file-as! target-path file)))

  ((add-file-as! target-name ::string file ::java.io.File)::void
   (let* ((entry ::ZipEntry (ZipEntry target-name))
	  (input ::FileInputStream (FileInputStream file)))
     (output:putNextEntry entry)
     (rewrite! input #;to output #;via buffer)
     (input:close)
     (output:closeEntry)))
  
  ((add-file-with-text! text ::String file-name::string)::void
   (let ((entry ::ZipEntry (ZipEntry file-name)))
     (output:putNextEntry entry)
     (output:write (text:getBytes))
     (output:closeEntry)))

  ((add-file-with-bytes! bytes ::(array-of byte) file-name ::string)::void
   (let ((entry ::ZipEntry (ZipEntry file-name)))
     (output:putNextEntry entry)
     (output:write bytes)
     (output:closeEntry)))

  ((add-file-with-binary-content! content ::bytevector
				  file-name ::string)::void
   (let ((entry ::ZipEntry (ZipEntry file-name)))
     (output:putNextEntry entry)
     (output:write (content:getBuffer) 0 (length content))
     (output:closeEntry)))
  
  ((close)::void
   (output:close)
   (file-output:close))
  
  ((*init* file ::java.io.File)
   (set! file-output (FileOutputStream file))
   (set! output (ZipOutputStream file-output))))


(define (delete-if-exists filename)
  (when (file-exists? filename)
    (delete-file filename)))
