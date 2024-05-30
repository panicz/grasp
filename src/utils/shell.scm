(module-name (utils shell))

(define-alias InputStream java.io.InputStream)
(define-alias Runtime java.lang.Runtime)
(define-alias Process java.lang.Process)
(define-alias Charset java.nio.charset.StandardCharsets)

;(define-alias PtyProcess com.pty4j.PtyProcess)

(define runtime ::Runtime (Runtime:getRuntime))

(define (shell . command-parts)::string
  (let* ((command ::string (apply string-append command-parts))
	 (process ::Process (runtime:exec command)))
    (process:waitFor)
    (let* ((output ::InputStream (process:getInputStream))
	   (bytes ::int (output:available))
	   (data ::($bracket-apply$ byte)
		 (($bracket-apply$ byte) length: bytes)))
      (output:read data)
      (output:close)
      (as string (String data Charset:UTF_8)))))
