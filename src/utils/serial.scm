(module-name (utils serial))

(import (kawa regex))

(import (language define-property))
(import (language define-type))
(import (language define-object))
(import (language define-interface))
(import (language while))
(import (language for))
(import (language match))
(import (language infix))
(import (language examples))

(import (utils functions))
(import (utils binary))
(import (utils print))
(import (utils mbus))

(define-alias serial com.fazecast.jSerialComm.SerialPort)

(define-interface Connection ()
  (close!)::boolean
  (send! message::string)::void
  (receive!)::string
  )

(define-type (SerialConnection port: serial
                               input: gnu.kawa.io.InPort
                               output: gnu.kawa.io.OutPort)
  implementing Connection with

  ((close!)::boolean
   (set! input #!null)
   (set! output #!null)
   (port:closePort))

  ((send! message::string)::void
   ;;(write-string message output) ;<- loses characters with longer messages
   (for c::char in message
        (write-char c output)
        (flush-output-port output)))

  ((receive!)::string
   (read-string (port:bytesAvailable) input)))

(define (list-serial-ports)::(list-of string)
  (map (lambda (x::serial)
         (x:getSystemPortName))
       (serial:getCommPorts)))

(define (open-serial-connection name ::string
                                #!key
                                (baud-rate ::int 115200)
                                (read-timeout/ms ::int 100)
                                (write-timeout/ms ::int 100)
                                (timeout-mode ::int (+ #;serial:TIMEOUT_NONBLOCKING
                                                    #;serial:TIMEOUT_SCANNER
                                                    serial:TIMEOUT_READ_SEMI_BLOCKING
                                                    serial:TIMEOUT_WRITE_BLOCKING)))
  ::SerialConnection
  (otherwise
   #!null
   (and-let* ((port ::serial (find (lambda (x::serial)
                                     (string=? (x:getSystemPortName)
                                               name))
                                   (serial:getCommPorts)))
              ((port:openPort)))
     (port:setBaudRate baud-rate)
     (port:setComPortTimeouts timeout-mode
                              read-timeout/ms
                              write-timeout/ms)
     (SerialConnection port: port
                       input: (gnu.kawa.io.InPort (port:getInputStream))
                       output: (gnu.kawa.io.OutPort (port:getOutputStream))))))

(define-object (Dongle connection::SerialConnection)

  (define timeout/ms ::int 100)

  (define aes-key ::(list-of byte) (make-list 16 0))

  (define access-number ::Byte 0)

  (define (command! . messages)::string
    (let ((discarded (connection:receive!)))
      (unless (equal? "" discarded)
        (DUMP discarded)))

    (let ((command ::string (string-join (only (isnt _ string=? "")
                                               (map any->string messages))
                                         " ")))
      (DUMP command)
      (connection:send! (string-append command "\r\n")))

    (java.lang.Thread:sleep timeout/ms)

    (let ((response ::string (connection:receive!)))
      (DUMP response)
      response))

  (define ambush-repetitions ::int 1)

  (define ambush-delay-ms ::int 0)

  (define ambush-period-ms ::int 6)

  (define ambush-channel ::int 1)

  (define ambush-perpetual? ::boolean #t)

  (define (setup-config-ambush! device-id::integer)
    ::integer
    (otherwise
     #!null
     (and-let* ((ambush-response (command! "ambush new"
                                           ambush-repetitions
                                           ambush-delay-ms
                                           ambush-period-ms
                                           ambush-channel
                                           (if ambush-perpetual?
                                               "perpetual"
                                               "")))
                (`(,_ ,ambush-index) (regex-match "The ambush index is ([0-9]*)"
                                                  ambush-response))
                (config-response (command! "config radio"))
                (`(,_ ,dongle-id) (regex-match "device_id = 0x([0-9]{8}),"
                                               config-response))
                (`(,_ ,dongle-manufacturer) (regex-match "manufacturer = ([A-Z]{3}),"
                                                         config-response))
                (`(,_ ,dongle-version) (regex-match "version = ([0-9]+),"
                                                   config-response))
                (config-frame (wmbus-flowis-config-frame
                               dongle-manufacturer: dongle-manufacturer
                               dongle-serial-number: (string->number dongle-id)
                               dongle-version: (string->number dongle-version)
                               timestamp: (current-UNIX-epoch-second)
                               aes-key: aes-key
                               access-number: access-number
                               target-serial-number: device-id
                               config-frame-payload:
                               `(,(DONGLE-COMMAND:1C-ENTER-CONFIGURATION:ordinal)))))
       (command! "ambush set_trigger" ambush-index (string-join
                                                      (map number->hex
                                                           (little-endian-32
                                                            (number/base
                                                             16 (digits/base
                                                                 10 device-id))))
                                                      ""))
       (command! "ambush set_trigger_offset" ambush-index 4)
       (command! "ambush set_response" ambush-index (string-join
                                                     (map number->hex config-frame)
                                                     ""))
       (string->number ambush-index))))

  (define (disable-ambush! index::int)
    ::void
    (command! "ambush set_trigger_offset" index 255))

  (define (setup-target! device-id::int)::void
    (command! "dongle target config_key "(list->hex aes-key))
    (command! "dongle target id "device-id)
    #;(command! "dongle radio_ch 1")
    #;(command! "radio_cmd_state on"))


  )
