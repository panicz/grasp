(module-name (utils serial))

(import (kawa regex))

(import (language define-property))
(import (language define-type))
(import (language define-object))
(import (language define-interface))
(import (language match))
(import (language infix))
(import (language examples))

(import (utils functions))
(import (utils binary))

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
   (write-string message output)
   (flush-output-port output))

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
    (connection:send! (string-append (string-join messages "") "\r\n"))
    (java.lang.Thread:sleep timeout/ms)
    (connection:receive!))

  (define (setup-config-ambush! device-id::string)
    ::integer
    (otherwise
     #!null
     (and-let* ((ambush-response (command! "ambush new 2 12 12 1 perpetual"))
                (`(,_ ,ambush-index) (regex-match #/The ambush index is ([0-9]*)/
                                                  ambush-response))
                (config-response (command! "config radio"))
                (`(,_ ,dongle-id) (regex-match #/device_id = 0x([0-9]{8}),/
                                               config-response))
                (`(,_ ,dongle-manufacturer) (regex-match #/manufacturer = ([A-Z]{3}),/
                                                         config-response))
                (`(,_ dongle-version) (regex-match #/version = ([0-9]+),/
                                                   config-response))
                (address offset (wmbus-device-id-bytes+offset device-id))
                (serial-number (match offset
                                 (2 (take 4 (drop 2 address)))
                                 (4 (take 4 address))))
                (config-frame (wmbus-flowis-config-frame
                               dongle-manufacturer: dongle-manufacturer
                               dongle-serial-number: (reverse
                                                      (hex->list
                                                       (regex-split every-two-characters
                                                                    dongle-id)))
                               dongle-version: (list->number dongle-version)
                               timestamp: (current-UNIX-epoch-second)
                               aes-key: aes-key
                               access-number: access-number
                               target-serial-number: serial-number
                               config-frame-payload:
                               `(,(DONGLE-COMMAND:1C-ENTER-CONFIGURATION:ordinal)))))

       (command! "ambush set_trigger "ambush-index" "(list->hex address))
       (command! "ambush set_trigger_offset "ambush-index" "(number->string offset))
       (command! "ambush set_response "ambush_index" "(list->hex config-frame))
       (string->number ambush-index))))

  )
