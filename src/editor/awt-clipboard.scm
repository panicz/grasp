(module-name (editor awt-clipboard))

(import (language define-type))
(import (language define-object))
(import (language define-interface))
(import (language match))
(import (utils conversions))
(import (editor types spaces))
(import (editor types primitive))
(import (editor document copy-paste))
(import (editor types texts))
(import (utils print))

(define-alias Transferable java.awt.datatransfer.Transferable)
(define-alias DataFlavor java.awt.datatransfer.DataFlavor)

(define-alias StringSelection java.awt.datatransfer.StringSelection)
(define-alias AWTClipboard java.awt.datatransfer.Clipboard)
(define-alias ClipboardOwner java.awt.datatransfer.ClipboardOwner)

(define-interface OwnClipboard (Clipboard ClipboardOwner))

(define-object (AWTSystemClipboard clipboard::AWTClipboard)
  ::OwnClipboard

  (define own-content ::list '())
  (define own-clip-data ::Transferable #!null)

  (define (try-parse item ::Transferable)::list
    (let ((input ::String (as String (item:getTransferData
				      DataFlavor:stringFlavor))))
      (with-input-from-string input
	(lambda ()
	  (let*-values (((expression preceding-space) (read-list 1))
			((following-space) (read-spaces))
			((next) (peek-char)))
	    (if (eof-object? next)
		expression
		(cons (text input) (empty))))))))

  (define (upload! new-content ::pair)::void
    (and-let* ((`(,head . ,tail) new-content)
	       (text (show->string head))
	       (clip ::Transferable (StringSelection text)))
      (clipboard:setContents clip (this))
      (set! own-clip-data clip)
      (set! own-content new-content)))

  (define (content)::list
    (let ((clip ::Transferable (clipboard:getContents (this))))
      (if (eq? clip own-clip-data)
	  (copy own-content)
	  (try-parse clip))))
  
  (define (lostOwnership context::AWTClipboard
			 content::Transferable)
    ::void
    (set! own-content '()))
  )
