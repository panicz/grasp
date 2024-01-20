(module-name (utils mbus))

(import (kawa regex))

(import (language define-type))
(import (language examples))
(import (language match))
(import (language infix))

(import (utils functions))
(import (utils binary))
(import (utils fixnum))
(import (utils affine))
(import (utils crypto))

(define-constant every-two-characters ::regex #/(?<=..)(?=(?:..)+$)/)

(e.g.
 (regex-split every-two-characters "AABBCCDD") ===> ("AA" "BB" "CC" "DD"))

(define-enum MBUS-DEVICE-TYPE
  (00-OTHER
   01-OIL-METER
   02-ELECTRICITY-METER
   03-GAS-METER
   04-HEAT-METER
   05-STEAM-METER
   06-WARM-WATER-METER
   07-WATER-METER
   08-HEAT-COST-ALLOCATOR
   09-COMPRESSED-AIR
   0A-COOLING-LOAD-METER-OUTLET
   0B-COOLING-LOAD-METER-INLET
   0C-HEAT-METER-INLET
   0D-COMBINED-HEAT-COOLING-METER
   0E-BUS/SYSTEM-COMPONENT
   0F-UNKNOWN-DEVICE-TYPE
   10-CONSUMPTION-METER
   11-CONSUMPTION-METER
   12-CONSUMPTION-METER
   13-CONSUMPTION-METER
   14-CALORIFIC-VALUE
   15-HOT-WATER-METER
   16-COLD-WATER-METER
   17-DUAL-WATER-METER
   18-PRESSURE-METER
   19-A/D-CONVERTER
   1A-SMOKE-DETECTOR
   1B-ROOM-SENSOR
   1C-GAS-DETECTOR
   1D-UNKNOWN-SENSOR
   1E-UNKNOWN-SENSOR
   1F-UNKNOWN-SENSOR
   20-BREAKER
   21-VALVE
   22-SWITCH-DEVICE
   23-SWITCH-DEVICE
   24-SWITCH-DEVICE
   25-CUSTOMER-UNIT
   26-CUSTOMER-UNIT
   27-CUSTOMER-UNIT
   28-WASTE-WATER
   29-GARBAGE
   2A-CARBON-DIOXIDE
   2B-ENVIRONMENTAL-METER
   2C-ENVIRONMENTAL-METER
   2D-ENVIRONMENTAL-METER
   ;;...
   ))

(define-constant CONFIG-FRAME-IDENTIFIER-52 #x52)

(define-enum DONGLE-COMMAND
  (00-SET-USER-CONFIG
   01-GET-USER-CONFIG
   02-SET-PRODUCTION-CONFIG
   03-GET-PRODUCTION-CONFIG
   04-SET-CRYPTO-CONFIG
   05-GET-CRYPTO-CONFIG
   06-SAVE-TO-FLASH
   07-RETRIEVE-FROM-FLASH
   08-ERASE-FLASH
   09-GET-RESET-CAUSE-HISTORY
   0A-ERASE-RESET-CAUSE-HISTORY
   0B-SET-UNIX-TS
   0C-SET-INTERNAL-LITERS
   0D-SET-INTERNAL-PULSES
   0E-CLEAR-ALARMS
   0F-CLEAR-HISTORY
   10-EXIT
   11-READ-FLOWIS
   12-READ-PULSE
   13-READ-MCU-ID
   14-READ-VERSION
   15-LESENSE-SELFTEST
   16-LESENSE-SELFTEST-LAST-RESULT
   17-RESET
   18-HELLO
   19-SET-RADIO-CHANNEL
   1A-GET-WORKING-TEMPERATURE-HISTORY
   1B-SET-OMS-ID
   1C-ENTER-CONFIGURATION))

(define-constant MBUS-STL-STATUS-00 #x00)

(define (wmbus-manufacturer-bytes manufacturer-string::string)::(list-of Byte)
  (reverse
   (digits/base
    256
    (number/base
     32 (map (lambda (c)
               (- (char->integer c) 64))
             manufacturer-string)))))

(e.g.
 (wmbus-manufacturer-bytes "APA") ===> (#x01 #x06))

(e.g.
 (wmbus-manufacturer-bytes "KYN") ===> (#x2e #x2f))

(e.g.
 (wmbus-manufacturer-bytes "PLO") ===> (#x8f #x41))

(e.g.
 (wmbus-manufacturer-bytes "FLM") ===> (#x8d #x19))

(define (wmbus-manufacturer-string b0::integer b1::integer)::string
  (let ((value (number/base 256 `(,b1 ,b0))))
    (list->string
     (map (lambda (x) (integer->char (+ x 64)))
          (digits/base 32 value)))))

(e.g.
 (wmbus-manufacturer-string #x01 #x06) ===> "APA")

(e.g.
 (wmbus-manufacturer-string #x2e #x2f) ===> "KYN")

(e.g.
 (wmbus-manufacturer-string #x8f #x41) ===> "PLO")

(e.g.
 (wmbus-manufacturer-string #x8d #x19) ===> "FLM")

(define (wmbus-device-id-bytes+offset device-id::string)
  ::(Values (list-of byte) integer)
  (cond
   ((regex-match "^([A-Z]{3})([0-9]{8})$" device-id)
    => (lambda (result)
         (match result
           (`(,_ ,manufacturer ,serial-number)
            (values
             `(,@(wmbus-manufacturer-bytes manufacturer)
               ,@(reverse
                  (map hex->number
                       (regex-split every-two-characters
                                    serial-number))))
             2)))))
    ((regex-match "^([0-9]{8})$" device-id)
     => (lambda (result)
          (match result
            (`(,_ ,serial-number)
             (values
              (reverse
               (map hex->number
                    (regex-split every-two-characters
                                 serial-number)))
              4)))))
     (else
     (error "Unsupported device-id format: "device-id))))

(e.g.
 (wmbus-device-id-bytes+offset "KYN12345678")
 ===> (#x2e #x2f #x78 #x56 #x34 #x12) 2)

(e.g.
 (wmbus-device-id-bytes+offset "12345678")
 ===> (#x78 #x56 #x34 #x12) 4)

(define (MBus-STL-conf-word byte0::Byte byte1::Byte)::(list-of Byte)
  ;; TODO: trzeba przeanalizowac owo slowo konfiguracyjne
  `(,byte0 ,byte1))

(define (wmbus-flowis-config-frame
         #!key
         (dongle-manufacturer ::string "FLM")
         (dongle-serial-number ::integer #x00000000)
         (dongle-version ::Byte 0)
         (timestamp ::long (current-UNIX-epoch-second))
         (aes-key ::(list-of byte) (make-list 16 0))
         (access-number ::Byte 0)
         (target-serial-number ::integer #x00000000)
         (config-frame-payload ::(list-of byte) '()))
  (let* ((man (wmbus-manufacturer-bytes dongle-manufacturer))
         (id (little-endian-32 dongle-serial-number))
         (ver dongle-version)
         (dll `(#x44 ,@man ,@id ,ver ,(MBUS-DEVICE-TYPE:00-OTHER:ordinal)))
         (stl `(#x7a ,access-number ,MBUS-STL-STATUS-00
                     ,@(MBus-STL-conf-word #x10 #xa5)))
         (payload `(#x2f #x2f ,CONFIG-FRAME-IDENTIFIER-52
                         ,@(little-endian-32 target-serial-number)
                         ,@(unsigned-little-endian-32 (bitwise-and #xffffffff
                                                                   timestamp))
                         ,@config-frame-payload))
         (padded (let* ((n (length payload))
                        (unpadded (modulo n 16)))
                   (if (= unpaddede 0)
                       payload
                       (append! payload (make-list (- 16 unpadded) #x2f)))))
         (aes-initial-vector `(,@man ,@id ,ver ,(MBUS-DEVICE-TYPE:00-OTHER:ordinal)
                                     ,@(make-list 8 access-number)))
         (encrypted-payload (aes-cbc-encrypt padded key: aes-key
                                             iv: aes-initial-vector))
         (content `(,@dll ,@stl ,@encrypted-payload)))
    `(,(length content) ,@content)))

(e.g.
 (wmbus-flowis-config-frame
  dongle-manufacturer: "FLM"
  dongle-serial-number: #x10000200
  dongle-version: 1
  timestamp: 0
  access-number: #xC5
  target-serial-number: #x41146360
  config-frame-payload:
  `(,(DONGLE-COMMAND:1C-ENTER-CONFIGURATION:ordinal)))
 ;     L  C  MANUF SERIAL NUM  V  T  ?  AC ST CONFW
 ===> "1e 44 8d 19 00 02 00 10 01 00 7a c5 00 10 a5\
 f1 32 44 d8 98 65 93 c3 84 b5 51 ce 80 6a bf 96")
