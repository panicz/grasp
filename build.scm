#!/bin/sh 
#|
mkdir -p build/cache
JARS=`ls libs/*.jar | tr '\n' ':' | sed 's/:$//'`
exec java -cp "$JARS:build/cache" kawa.repl \
  -Dkawa.import.path="|:src:build/cache:." \
  -f $0 $*
|#
(import (language define-syntax-rule))
(import (language define-interface))
(import (language define-object))
(import (language match))
(import (language infix))
(import (utils conversions))
(import (language define-cache))
(import (language define-type))
(import (utils file))
(import (utils build))

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
	 (report "Invalid argument: "args)
	 (report "Available options:")
	 (report "  --"(symbol->string 'name)" "help" ["default"]")
	 ...
	 (exit))))))

(define (build-with command-line-args::(list-of string))
  (with-command-line-arguments (cdr command-line-args)
      ((targets "<comma-separated list of targets>"
		"android,desktop,terminal")
       (name "<target application name>" "GRASP")
       (icon "<path to icon file>" (join-path "icons" "grasp.png"))
       (init "<path to init file>" (join-path "init" "init.scm"))
       (package "<top class package>" "io.github.panicz")
       (keystore "<path to keystore>" (join-path "binary" "keystore"))
       (assets "<comma-separated list of files/directories>" "assets")
       (key "<key alias>" "grasp-public")
       (password "<key password>" "untrusted"))
    (build targets: (map string->symbol (string-split targets ","))
	   name: name
	   icon: icon
	   init: init
	   package: package
	   keystore: keystore
	   key: key
	   assets: (string-split assets ",")
	   password: password)))

(build-with (command-line))

(exit)
