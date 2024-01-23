(module-name (utils shell))

(define-alias Charset java.nio.charset.StandardCharsets)

;(define-alias PtyProcess com.pty4j.PtyProcess)

(define runtime ::java.lang.Runtime
  (java.lang.Runtime:getRuntime))

(define (shell . command-parts)::string
  (let* ((command ::string (apply string-append command-parts))
	 (process ::Process (runtime:exec command)))
    (process:waitFor)
    (let* ((output ::java.io.InputStream (process:getInputStream))
	   (bytes ::int (output:available))
	   (data ::($bracket-apply$ byte)
		 (($bracket-apply$ byte) length: bytes)))
      (output:read data)
      (output:close)
      (as string (String data Charset:UTF_8)))))
