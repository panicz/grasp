(module-name (language define-parameter))

(define-simple-class SharedThreadLocation (gnu.mapping.ThreadLocation)

  (location-name ::gnu.mapping.Symbol)
  (default-value)
  
  ((setWithSave value) access: 'synchronized
   (let* ((old-location (invoke-special gnu.mapping.ThreadLocation (this) 'get))
	  (new-location (gnu.mapping.SharedLocation
			 location-name
			 #!null
			 (java.lang.System:currentTimeMillis))))
     (invoke-special gnu.mapping.ThreadLocation (this) 'set new-location)
     (new-location:set value)
     old-location))
  
  ((setRestore old-location) access: 'synchronized
   (invoke-special gnu.mapping.ThreadLocation (this) 'set old-location))
  
  ((set value) access: 'synchronized
   (let ((location ::gnu.mapping.SharedLocation
		   (invoke-special gnu.mapping.ThreadLocation
				   (this) 'get)))
     (if location
	 (location:set value)
	 (let ((new-location (gnu.mapping.SharedLocation
			      location-name
			      #!null
			      (java.lang.System:currentTimeMillis))))
	   (invoke-special gnu.mapping.ThreadLocation (this) 'set new-location)
	   (new-location:set value)))))
  
  ((get) access: 'synchronized
   (let ((location ::gnu.mapping.SharedLocation
		   (invoke-special gnu.mapping.ThreadLocation
				   (this) 'get)))
     (if location
	 (location:get)
	 default-value)))
  
  ((*init* name ::gnu.mapping.Symbol value)
   (set! location-name name)
   (set! default-value value)
   (let ((new-location (gnu.mapping.SharedLocation
			location-name
			#!null
			(java.lang.System:currentTimeMillis))))
     (invoke-special gnu.mapping.ThreadLocation (this) 'set new-location)
     (new-location:set value))))

(define (make-shared-parameter name init #!optional (converter #!null))
  (unless (eq? converter #!null)
    (set! init (converter init)))
  (let* ((loc (SharedThreadLocation:new name init))
         (conv ::gnu.mapping.Procedure
               (if (or (eq? converter #!null)
                       (gnu.mapping.Procedure? converter))
                   converter
                   (lambda (x) (converter x)))))
    (gnu.mapping.LocationProc:new loc conv)))

(define-syntax define-parameter
  (syntax-rules (::)
    
    ((_ (parameter-name) :: type initial-value)
     (define-early-constant parameter-name :: parameter[type]
       (make-shared-parameter 'parameter-name initial-value)))

    ((_ (parameter-name) :: type)
     (define-early-constant parameter-name :: parameter[type]
       (make-shared-parameter 'parameter-name #!null)))
    
    ((_ (parameter-name) initial-value)
     (define-early-constant parameter-name :: parameter
       (make-shared-parameter 'parameter-name initial-value)))

    ((_ (parameter-name))
     (define-early-constant parameter-name :: parameter
       (make-shared-parameter 'parameter-name #!null)))
    ))

(define-syntax define-parameter+
  (syntax-rules (::)
    
    ((_ (parameter-name) :: type initial-value)
     (define parameter-name :: parameter[type]
       (make-shared-parameter 'parameter-name initial-value)))

    ((_ (parameter-name) :: type)
     (define parameter-name :: parameter[type]
       (make-shared-parameter 'parameter-name #!null)))
    
    ((_ (parameter-name) initial-value)
     (define parameter-name :: parameter
       (make-shared-parameter 'parameter-name initial-value)))

    ((_ (parameter-name))
     (define parameter-name :: parameter
       (make-shared-parameter 'parameter-name #!null)))
    ))
