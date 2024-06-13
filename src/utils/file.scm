(module-name (utils file))

(import (language assert))
(import (language define-type))
(import (language for))
(import (language infix))

(define-alias ZipEntry java.util.zip.ZipEntry)
(define-alias ZipFile java.util.zip.ZipFile)
(define-alias ZipOutputStream java.util.zip.ZipOutputStream)
(define-alias ZipInputStream java.util.zip.ZipInputStream)
(define-alias FileOutputStream java.io.FileOutputStream)
(define-alias FileInputStream java.io.FileInputStream)
(define-alias InputStream java.io.InputStream)
(define-alias OutputStream java.io.OutputStream)

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

(define (copy! input-stream ::InputStream
	       #;to output-stream ::OutputStream
	       #;via buffer ::(array-of byte))
  (let loop ()
    (let ((n ::int (input-stream:read buffer)))
      (unless (= n -1)
	(output-stream:write buffer 0 n)
	(loop)))))

(define (append-zip-entries! #!key
			     (such-that ::(maps (ZipEntry)
						to: boolean)
					always)
			     from ::ZipFile
			     to ::ZipOutputStream)
  ::void
  (let ((entries ::java.util.Enumeration
		 (from:entries))
	(buffer ::(array-of byte) ((array-of byte) length: 4096)))
    (while (entries:hasNextElement)
      (let ((entry ::ZipEntry (entries:nextElement)))
	(to:putNextEntry entry)
	(unless (or (entry:isDirectory)
		    (isnt entry such-that))
	  (copy! #;from (from:getInputStream entry)
		 #;to to
		 #;via buffer))
	(to:closeEntry)))))
