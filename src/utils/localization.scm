(module-name (utils localization))
(import (language assert))
(import (language define-parameter))

(define (flag country-code ::string)::string
  (let* ((a ::int (char->integer #\a)))
    (string-map (lambda (c)
		  (assert (char-alphabetic? c))
		  (integer->char
		   (+ #x1F1E6
		      (- (char->integer
			  (char-downcase c))
			 a))))
		country-code)))

(define-parameter (current-language)::string
  (let ((locale ::java.util.Locale (java.util.Locale:getDefault)))
    (locale:getLanguage)))

(define-parameter (current-country)::string
  (let ((locale ::java.util.Locale (java.util.Locale:getDefault)))
    (locale:getCountry)))
