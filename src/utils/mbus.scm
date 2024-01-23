(module-name (utils mbus))

(import (srfi :11))
(import (kawa regex))
(import (language assert))
(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language match))
(import (language infix))
(import (language while))
(import (language for))
(import (language examples))

(import (utils functions))
(import (utils binary))
(import (utils fixnum))
(import (utils affine))
(import (utils crypto))
(import (utils print))

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
   2E-UNDEFINED
   2F-UNDEFINED
   30-SYSTEM-DEVICE
   31-COMMUNICATION-CONTROLLER
   32-UNIDIRECTIONAL-REPEATER
   33-BIDIRECTIONAL-REPEATER
   34-SYSTEM-DEVICE
   35-SYSTEM-DEVICE
   36-RADIO-CONVERTER-SYSTEM-SIDE
   37-RADIO-CONVERTER-METER-SIDE
   38-SYSTEM-DEVICE
   39-SYSTEM-DEVICE
   3A-SYSTEM-DEVICE
   3B-SYSTEM-DEVICE
   3C-SYSTEM-DEVICE
   3D-SYSTEM-DEVICE
   3E-SYSTEM-DEVICE
   3F-SYSTEM-DEVICE
   ))

(assert (= (MBUS-DEVICE-TYPE:3F-SYSTEM-DEVICE:ordinal) #x3F))

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

(define-enum MBUS-DATA-TYPE
  (0-NO-DATA
   1-INT8
   2-INT16
   3-INT24
   4-INT32
   5-REAL32
   6-INT48
   7-INT64
   8-READOUT
   9-BCD2
   A-BCD4
   B-BCD6
   C-BCD8
   D-LVAR
   E-BCD12
   F-SPECIAL))

(assert (= (DONGLE-COMMAND:1C-ENTER-CONFIGURATION:ordinal) #x1C))

(define-enum MBUS-DATA-FUNCTION
  (0-INSTANTANEOUS-VALUE
   1-MAXIMUM-VALUE
   2-MINIMUM-VALUE
   3-VALUE-DURING-ERROR-STATE))

(define (mbus-data-size type::MBUS-DATA-TYPE)::real
  (match type
    (,MBUS-DATA-TYPE:0-NO-DATA 0)
    (,MBUS-DATA-TYPE:1-INT8 1)
    (,MBUS-DATA-TYPE:2-INT16 2)
    (,MBUS-DATA-TYPE:3-INT24 3)
    (,MBUS-DATA-TYPE:4-INT32 4)
    (,MBUS-DATA-TYPE:5-REAL32 4)
    (,MBUS-DATA-TYPE:6-INT48 6)
    (,MBUS-DATA-TYPE:7-INT64 8)
    (,MBUS-DATA-TYPE:8-READOUT +nan.0)
    (,MBUS-DATA-TYPE:9-BCD2 1)
    (,MBUS-DATA-TYPE:A-BCD4 2)
    (,MBUS-DATA-TYPE:B-BCD6 3)
    (,MBUS-DATA-TYPE:C-BCD8 4)
    (,MBUS-DATA-TYPE:D-LVAR +nan.0)
    (,MBUS-DATA-TYPE:E-BCD12 6)
    (,MBUS-DATA-TYPE:F-SPECIAL +nan.0)))

(define-enum MBUS-UNIT
  (ENERGY-kWh
   ENERGY-J
   VOLUME-m^3
   MASS-kg
   POWER-W
   POWER-J/h
   VOLUME-FLOW-m^3/h
   VOLUME-FLOW-EXT-m^3/min
   VOLUME-FLOW-EXT-m^3/s
   MASS-FLOW-kg/h
   ON-TIME
   OPERATING-TIME
   FLOW-TEMPERATURE-C
   RETURN-TEMPERATURE-C
   TEMPERATURE-DIFFERENCE-K
   EXTERNAL-TEMPERATURE-C
   PRESSURE-BAR
   AVERAGING-DURATION
   ACTUALITY-DURATION
   DATETIME-G
   DATETIME-F
   FOR-HCA
   RESERVED
   FABRICATION-NUMBER
   ENHANCED
   BUS-ADDRESS
   ASCII
   ANY
   MANUFACTURER-SPECIFIC
   VIF-EXTENSION-B
   VIF-EXTENSION-D
   VOLTAGE-V
   CURRENT-A
   ACCESS-NUMBER
   MEDIUM
   MANUFACTURER
   PARAMETER-SET-IDENTIFICATION
   MODEL-VERSION
   HARDWARE-VERSION
   FIRMWARE-VERSION
   SOFTWARE-VERSION
   CUSTOMER-LOCATION
   CUSTOMER
   ACCESS-CODE-USER
   ACCESS-CODE-OPERATOR
   ACCESS-CODE-SYSTEM-OPERATOR
   ACCESS-CODE-DEVELOPER
   PASSWORD
   ERROR-FLAGS
   ERROR-MASK
   DIGITAL-OUTPUT
   DIGITAL-INPUT
   BAUDRATE
   RESPONSE-DELAY-TIME
   RETRY
   FIRST-CYCLIC-STORAGE-NUMBER
   LAST-CYCLIC-STORAGE-NUMBER
   SIZE-OF-STORAGE-BLOCK
   DIMENSIONLESS
   RESET-COUNTER
   CUMULATION-COUNTER
   CONTROL-SIGNAL
   DAY-OF-WEEK
   WEEK-NUMBER
   TIMEPOINT-OF-DAY-CHANGE
   STATE-OF-PARAMETER-ACTIVATION
   DATE-AND-TIME-OF-BATTERY-CHANGE
   CURRENCY-CREDIT
   CURRENCY-DEBIT
   STORAGE-INTERVAL
   DURATION-SINCE-LAST-READOUT
   START-OF-TARIFF
   DURATION-OF-TARIFF
   PERIOD-OF-TARIFF
   DURATION-SINCE-LAST-CUMULATION
   OPERATING-TIME-BATTERY
   CUMULATED-COUNT-MAX-POWER-W
   FLOW-TEMPERATURE-F
   RETURN-TEMPERATURE-F
   TEMPERATURE-DIFFERENCE-F
   EXTERNAL-TEMPERATURE-F
   COLD-WARM-TEMPERATURE-LIMIT-F
   COLD-WARM-TEMPERATURE-LIMIT-C
   ENERGY-MWh
   ENERGY-GJ
   VOLUME-T
   POWER-MW
   POWER-GJ/h
   VOLUME-ft^3
   VOLUME-AMERICAN-GALLON
   FLOW-AMERICAN-GALLON/min
   FLOW-AMERICAN-GALLON/h
   UNKNOWN))

(define-enum MBUS-TIME-UNITS
  (SECONDS
   MINUTES
   HOURS
   DAYS
   MONTHS
   YEARS
   UNKNOWN))

(define (mbus-short-time-units code)::MBUS-TIME-UNITS
  (match code
    (#b00 MBUS-TIME-UNITS:SECONDS)
    (#b01 MBUS-TIME-UNITS:MINUTES)
    (#b10 MBUS-TIME-UNITS:HOURS)
    (#b11 MBUS-TIME-UNITS:DAYS)
    (_    MBUS-TIME-UNITS:UNKNOWN)))

(define (mbus-long-time-units code)::MBUS-TIME-UNITS
  (match code
    (#b00 MBUS-TIME-UNITS:HOURS)
    (#b01 MBUS-TIME-UNITS:DAYS)
    (#b10 MBUS-TIME-UNITS:HOURS)
    (#b11 MBUS-TIME-UNITS:DAYS)
    (_    MBUS-TIME-UNITS:UNKNOWN)))

(define (mbus-unit-scaled? unit::MBUS-UNIT)::boolean
  (is unit in
      (list MBUS-UNIT:ENERGY-kWh
            MBUS-UNIT:ENERGY-J
            MBUS-UNIT:VOLUME-m^3
            MBUS-UNIT:MASS-kg
            MBUS-UNIT:POWER-W
            MBUS-UNIT:POWER-J/h
            MBUS-UNIT:VOLUME-FLOW-m^3/h
            MBUS-UNIT:VOLUME-FLOW-EXT-m^3/min
            MBUS-UNIT:VOLUME-FLOW-EXT-m^3/s
            MBUS-UNIT:MASS-FLOW-kg/h
            MBUS-UNIT:FLOW-TEMPERATURE-C
            MBUS-UNIT:RETURN-TEMPERATURE-C
            MBUS-UNIT:TEMPERATURE-DIFFERENCE-K
            MBUS-UNIT:EXTERNAL-TEMPERATURE-C
            MBUS-UNIT:PRESSURE-BAR
            MBUS-UNIT:VOLTAGE-V
            MBUS-UNIT:CURRENT-A
            MBUS-UNIT:CURRENCY-CREDIT
            MBUS-UNIT:CURRENCY-DEBIT
            MBUS-UNIT:CUMULATED-COUNT-MAX-POWER-W
            MBUS-UNIT:FLOW-TEMPERATURE-F
            MBUS-UNIT:RETURN-TEMPERATURE-F
            MBUS-UNIT:TEMPERATURE-DIFFERENCE-F
            MBUS-UNIT:EXTERNAL-TEMPERATURE-F
            MBUS-UNIT:COLD-WARM-TEMPERATURE-LIMIT-F
            MBUS-UNIT:COLD-WARM-TEMPERATURE-LIMIT-C
            MBUS-UNIT:ENERGY-MWh
            MBUS-UNIT:ENERGY-GJ
            MBUS-UNIT:VOLUME-T
            MBUS-UNIT:POWER-MW
            MBUS-UNIT:POWER-GJ/h
            MBUS-UNIT:VOLUME-ft^3
            MBUS-UNIT:VOLUME-AMERICAN-GALLON
            MBUS-UNIT:FLOW-AMERICAN-GALLON/min
            MBUS-UNIT:FLOW-AMERICAN-GALLON/h)))

(define (mbus-unit-timed? unit::MBUS-UNIT)::boolean
  (is unit in (list
               MBUS-UNIT:ON-TIME
               MBUS-UNIT:OPERATING-TIME
               MBUS-UNIT:AVERAGING-DURATION
               MBUS-UNIT:ACTUALITY-DURATION
               MBUS-UNIT:PERIOD-OF-TARIFF
               MBUS-UNIT:STORAGE-INTERVAL
               MBUS-UNIT:DURATION-SINCE-LAST-READOUT
               MBUS-UNIT:DURATION-OF-TARIFF
               MBUS-UNIT:DURATION-SINCE-LAST-CUMULATION
               MBUS-UNIT:OPERATING-TIME-BATTERY)))

(define (mbus-data-type-integer? type::MBUS-DATA-TYPE)::boolean
  (is type in (list
               MBUS-DATA-TYPE:1-INT8
               MBUS-DATA-TYPE:2-INT16
               MBUS-DATA-TYPE:3-INT24
               MBUS-DATA-TYPE:4-INT32
               MBUS-DATA-TYPE:6-INT48
               MBUS-DATA-TYPE:7-INT64
               MBUS-DATA-TYPE:9-BCD2
               MBUS-DATA-TYPE:A-BCD4
               MBUS-DATA-TYPE:B-BCD6
               MBUS-DATA-TYPE:C-BCD8
               MBUS-DATA-TYPE:E-BCD12)))

(define (mbus-data-type-BCD? type::MBUS-DATA-TYPE)::boolean
  (is unit in (list
               MBUS-DATA-TYPE:9-BCD2
               MBUS-DATA-TYPE:A-BCD4
               MBUS-DATA-TYPE:B-BCD6
               MBUS-DATA-TYPE:C-BCD8
               MBUS-DATA-TYPE:E-BCD12)))

(define (mbus-LVAR-size lvar::int)::int
  (cond
   ((is    0 <= lvar <= #xBF)         lvar)
   ((is #xC0 <= lvar <= #xCF) (* 2 (- lvar #xC0)))
   ((is #xD0 <= lvar <= #xDF) (* 2 (- lvar #xD0)))
   ((is #xE0 <= lvar <= #xEF)      (- lvar #xEF))
   (else                              0)))


(define (mbus-VIF-unit+scale VIF::int)
  ::(Values MBUS-UNIT
            (either real MBUS-TIME-UNITS))
  (escape-with
   return
   (match (bitwise-and #b01111000 VIF)
     (#b00000000
      (return MBUS-UNIT:ENERGY-kWh (expt 10 (- (bitwise-and VIF #b111) 3))))

     (#b00001000
      (return MBUS-UNIT:ENERGY-J (expt 10 (bitwise-and VIF #b111))))

     (#b00010000
      (return MBUS-UNIT:VOLUME-m^3 (expt 10 (- (bitwise-and VIF #b111) 6))))

     (#b00011000
      (return MBUS-UNIT:MASS-kg (expt 10 (- (bitwise-and VIF #b111) 3))))

     (#b00101000
      (return MBUS-UNIT:POWER-W (expt 10 (- (bitwise-and VIF #b111) 3))))

     (#b00110000
      (return MBUS-UNIT:POWER-J/h (expt 10 (bitwise-and VIF #b111))))

     (#b00111000
      (return MBUS-UNIT:VOLUME-FLOW-m^3/h (expt 10 (- (bitwise-and VIF #b111) 6))))

     (#b01000000
      (return MBUS-UNIT:VOLUME-FLOW-EXT-m^3/min (expt 10 (- (bitwise-and VIF #b111) 7))))

     (#b01001000
      (return MBUS-UNIT:VOLUME-FLOW-EXT-m^3/s  (expt 10 (- (bitwise-and VIF #b111) 9))))

     (#b01010000
      (return MBUS-UNIT:MASS-FLOW-kg/h (expt 10 (- (bitwise-and VIF #b111) 3))))

     (_
      (values)))

   (match (bitwise-and #b01111100 VIF)
     (#b00100000
      (return MBUS-UNIT:ON-TIME (mbus-short-time-units (bitwise-and VIF #b11))))

     (#b00100100
      (return MBUS-UNIT:OPERATING-TIME (mbus-short-time-units (bitwise-and VIF #b11))))

     (#b01011000
      (return MBUS-UNIT:FLOW-TEMPERATURE-C (expt 10 (- (bitwise-and VIF #b11) 3))))

     (#b01011100
      (return MBUS-UNIT:RETURN-TEMPERATURE-C (expt 10 (- (bitwise-and VIF #b11) 3))))

     (#b01100000
      (return MBUS-UNIT:TEMPERATURE-DIFFERENCE-K (expt 10 (- (bitwise-and VIF #b11) 3))))

     (#b01100100
      (return MBUS-UNIT:EXTERNAL-TEMPERATURE-C (expt 10 (- (bitwise-and VIF #b11) 3))))

     (#b01101000
      (return MBUS-UNIT:PRESSURE-BAR (expt 10 (- (bitwise-and VIF #b11) 3))))

     (#b01110000
      (return MBUS-UNIT:AVERAGING-DURATION (mbus-short-time-units (bitwise-and VIF #b11))))

     (#b01110100
      (return MBUS-UNIT:ACTUALITY-DURATION (mbus-short-time-units (bitwise-and VIF #b11))))

     (_
      (values)))

   (match (bitwise-and #b01111111 VIF)
     (#b01101100
      (return MBUS-UNIT:DATETIME-G 1))

     (#b01101101
      (return MBUS-UNIT:DATETIME-F 1))

     (#b01101110
      (return MBUS-UNIT:FOR-HCA 1))

     (#b01101111
      (return MBUS-UNIT:RESERVED 1))

     (#b01111000
      (return MBUS-UNIT:FABRICATION-NUMBER 1))

     (#b01111001
      (return MBUS-UNIT:BUS-ADDRESS 1))

     (#b01111100
      (return MBUS-UNIT:ASCII 1))

     (#b01111110
      (return MBUS-UNIT:ANY 1))

     (#b01111111
      (return MBUS-UNIT:MANUFACTURER-SPECIFIC 1))

     (_
      (values)))

   (match VIF
     (#b11111011
      (return MBUS-UNIT:VIF-EXTENSION-B 1))
     (#b11111101
      (return MBUS-UNIT:VIF-EXTENSION-D 1)))

   ))


(define (mbus-VIFED-unit+scale VIFED::int)
  ::(Values MBUS-UNIT
            (either real MBUS-TIME-UNITS))

  (escape-with
   return
   (match (bitwise-and #b01110000 VIFED)
     (#b01000000
      (return MBUS-UNIT:VOLTAGE-V (expt 10 (- (bitwise-and VIFED #b111) 9))))

     (#b01010000
      (return MBUS-UNIT:CURRENT-A (expt 10 (- (bitwise-and #b111 VIFED) 12))))

     (_
      (values)))

   (match (bitwise-and #b01111111 VIFED)
     (#b00001000
      (return MBUS-UNIT:ACCESS-NUMBER 1))

     (#b00001001
      (return MBUS-UNIT:MEDIUM 1))

     (#b00001010
      (return MBUS-UNIT:MANUFACTURER 1))

     (#b00001011
      (return MBUS-UNIT:PARAMETER-SET-IDENTIFICATION 1))

     (#b00001100
      (return MBUS-UNIT:MODEL-VERSION 1))

     (#b00001101
      (return MBUS-UNIT:HARDWARE-VERSION 1))

     (#b00001110
      (return  MBUS-UNIT:FIRMWARE-VERSION 1))

     (#b00001111
      (return MBUS-UNIT:SOFTWARE-VERSION 1))

     (#b00010000
      (return MBUS-UNIT:CUSTOMER-LOCATION 1))

     (#b00010001
      (return MBUS-UNIT:CUSTOMER 1))

     (#b00010010
      (return MBUS-UNIT:ACCESS-CODE-USER 1))

     (#b00010011
      (return MBUS-UNIT:ACCESS-CODE-OPERATOR 1))

     (#b00010100
      (return MBUS-UNIT:ACCESS-CODE-SYSTEM-OPERATOR 1))

     (#b00010101
      (return MBUS-UNIT:ACCESS-CODE-DEVELOPER 1))

     (#b00010110
      (return MBUS-UNIT:PASSWORD 1))

     (#b00010111
      (return MBUS-UNIT:ERROR-FLAGS 1))

     (#b00011000
      (return MBUS-UNIT:ERROR-MASK 1))

     (#b00011001
      (return MBUS-UNIT:RESERVED #x19))

     (#b00011010
      (return MBUS-UNIT:DIGITAL-OUTPUT 1))

     (#b00011011
      (return MBUS-UNIT:DIGITAL-INPUT 1))

     (#b00011100
      (return MBUS-UNIT:BAUDRATE 1))

     (#b00011101
      (return MBUS-UNIT:RESPONSE-DELAY-TIME 1))

     (#b00011110
      (return MBUS-UNIT:RETRY 1))

     (#b00011111
      (return MBUS-UNIT:RESERVED #x1F))

     (#b00100000
      (return MBUS-UNIT:FIRST-CYCLIC-STORAGE-NUMBER 1))

     (#b00100001
      (return MBUS-UNIT:LAST-CYCLIC-STORAGE-NUMBER 1))

     (#b00100010
      (return MBUS-UNIT:SIZE-OF-STORAGE-BLOCK 1))

     (#b00100011
      (return MBUS-UNIT:RESERVED #x23))

     (#b00101000
      (return MBUS-UNIT:STORAGE-INTERVAL MBUS-TIME-UNITS:MONTHS))

     (#b00101001
      (return MBUS-UNIT:STORAGE-INTERVAL MBUS-TIME-UNITS:YEARS))

     (#b00101010
      (return MBUS-UNIT:RESERVED #x2A))

     (#b00101011
      (return MBUS-UNIT:RESERVED #x2B))

     (#b00111000
      (return MBUS-UNIT:PERIOD-OF-TARIFF MBUS-TIME-UNITS:MONTHS))

     (#b00111001
      (return MBUS-UNIT:PERIOD-OF-TARIFF MBUS-TIME-UNITS:YEARS))

     (#b00111010
      (return MBUS-UNIT:DIMENSIONLESS 1))

     (#b00111011
      (return MBUS-UNIT:RESERVED #x3B))

     (#b01100000
      (return MBUS-UNIT:RESET-COUNTER 1))

     (#b01100001
      (return MBUS-UNIT:CUMULATION-COUNTER 1))

     (#b01100010
      (return MBUS-UNIT:CONTROL-SIGNAL 1))

     (#b01100011
      (return  MBUS-UNIT:DAY-OF-WEEK 1))

     (#b01100100
      (return MBUS-UNIT:WEEK-NUMBER 1))

     (#b01100101
      (return MBUS-UNIT:TIMEPOINT-OF-DAY-CHANGE 1))

     (#b01100110
      (return MBUS-UNIT:STATE-OF-PARAMETER-ACTIVATION 1))

     (#b01110000
      (return MBUS-UNIT:DATE-AND-TIME-OF-BATTERY-CHANGE 1))

     (_
      (values)))

   (match (bitwise-and #b01111100 VIFED)
     (#b00000000
      (return MBUS-UNIT:CURRENCY-CREDIT (expt 10 (- (bitwise-and VIFED #b11) 3))))

     (#b00000100
      (return MBUS-UNIT:CURRENCY-DEBIT  (expt 10 (- (bitwise-and VIFED #b11) 3))))

     (#b00100100
      (return MBUS-UNIT:STORAGE-INTERVAL (mbus-short-time-units (bitwise-and VIFED #b11))))

     (#b00101100
      (return MBUS-UNIT:DURATION-SINCE-LAST-READOUT
              (mbus-short-time-units (bitwise-and VIFED #b11))))

     (#b00110000
      (if (zero? (bitwise-and VIFED #b11))
          (return MBUS-UNIT:START-OF-TARIFF 1)
          (return MBUS-UNIT:DURATION-OF-TARIFF
                  (mbus-short-time-units (bitwise-and VIFED #b11)))))

     (#b00110100
      (return MBUS-UNIT:PERIOD-OF-TARIFF
              (mbus-short-time-units (bitwise-and VIFED #b11))))

     (#b01101000
      (return MBUS-UNIT:DURATION-SINCE-LAST-CUMULATION
              (mbus-long-time-units (bitwise-and VIFED #b11))))

     (#b01101100
      (return MBUS-UNIT:OPERATING-TIME-BATTERY
              (mbus-long-time-units (bitwise-and VIFED #b11))))
     (_
      (return MBUS-UNIT:RESERVED #xED)))))

(define (mbus-VIFEB-unit+scale VIFEB::int)
  ::(Values MBUS-UNIT
            (either real MBUS-TIME-UNITS))

  (escape-with
   return
   (match (bitwise-and #b01111000 VIFEB)
     (#b01111000
      (return MBUS-UNIT:CUMULATED-COUNT-MAX-POWER-W;
              (expt 10 (- (bitwise-and VIFEB #b111) 3))))
     (_
      (values)))

   (match (bitwise-and #b01111100 VIFEB)
     (#b01011000
      (return MBUS-UNIT:FLOW-TEMPERATURE-F (expt 10 (- (bitwise-and VIFEB #b11) 3))))

     (#b01011100
      (return MBUS-UNIT:RETURN-TEMPERATURE-F (expt 10 (- (bitwise-and VIFEB #b11) 3))))

     (#b01100000
      (return MBUS-UNIT:TEMPERATURE-DIFFERENCE-F (expt 10 (- (bitwise-and VIFEB #b11) 3))))

     (#b01100100
      (return MBUS-UNIT:EXTERNAL-TEMPERATURE-F (expt 10 (- (bitwise-and VIFEB #b11) 3))))

     (#b01110000
      (return MBUS-UNIT:COLD-WARM-TEMPERATURE-LIMIT-F
              (expt 10 (- (bitwise-and VIFEB #b11) 3))))

     (#b01110100
      (return MBUS-UNIT:COLD-WARM-TEMPERATURE-LIMIT-C
              (expt 10 (- (bitwise-and VIFEB #b11) 3))))
     (_
      (values)))


   (match (bitwise-and #b01111110 VIFEB)
     (#b00000000
      (return MBUS-UNIT:ENERGY-MWh (expt 10 (- (bitwise-and VIFEB #b1) 1))))

     (#b00001000
      (return MBUS-UNIT:ENERGY-GJ (expt 10 (- (bitwise-and VIFEB #b1) 1))))

     (#b00010000
      (return MBUS-UNIT:VOLUME-m^3 (expt 10 (+ (bitwise-and VIFEB #b1) 2))))

     (#b00011000
      (return MBUS-UNIT:VOLUME-T (expt 10 (+ (bitwise-and VIFEB #b1) 2))))

     (#b00101000
      (return MBUS-UNIT:POWER-MW (expt 10 (- (bitwise-and VIFEB #b1) 1))))

     (#b00110000
      (return MBUS-UNIT:POWER-GJ/h (expt 10 (- (bitwise-and VIFEB #b1) 1))))

     (_
      (values)))

   (match (bitwise-and #b01111111 VIFEB)
     (#b00100001
      (return MBUS-UNIT:VOLUME-ft^3 0.1))

     (#b00100010
      (return MBUS-UNIT:VOLUME-AMERICAN-GALLON 0.1))

     (#b00100011
      (return MBUS-UNIT:VOLUME-AMERICAN-GALLON 1.0))

     (#b00100100
      (return MBUS-UNIT:FLOW-AMERICAN-GALLON/min 0.001))

     (#b00100101
      (return MBUS-UNIT:FLOW-AMERICAN-GALLON/min 1.0))

     (#b00100110
      (return MBUS-UNIT:FLOW-AMERICAN-GALLON/h 1.0))

     (_
      (return MBUS-UNIT:RESERVED #xEB)))))

;; TODO przeanalizowac status
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

(define (MBus-STL-conf-word byte0::Byte byte1::Byte)::(list-of Byte)
  ;; TODO: trzeba przeanalizowac owo slowo konfiguracyjne
  `(,byte0 ,byte1))

(define (wmbus-flowis-config-frame
         #!key
         (dongle-manufacturer ::string "FLM")
         (dongle-serial-number ::int 12345678)
         (dongle-version ::Byte 0)
         (timestamp ::long (current-UNIX-epoch-second))
         (aes-key ::(list-of byte) (make-list 16 0))
         (access-number ::Byte 0)
         (target-serial-number ::int 34567890)
         (config-frame-payload ::(list-of byte) '()))
  (let* ((man (wmbus-manufacturer-bytes dongle-manufacturer))
         (id (little-endian-32 (number/base 16 (digits/base 10 dongle-serial-number))))
         (ver dongle-version)
         (dll `(#x44 ,@man ,@id ,ver ,(MBUS-DEVICE-TYPE:00-OTHER:ordinal)))
         (stl `(#x7a ,access-number ,MBUS-STL-STATUS-00
                     ,@(MBus-STL-conf-word #x10 #xa5)))
         (payload `(#x2f #x2f ,CONFIG-FRAME-IDENTIFIER-52
                         ,@(little-endian-32 (number/base
                                              16 (digits/base
                                                  10 target-serial-number)))
                         ,@(unsigned-little-endian-32 (bitwise-and #xffffffff
                                                                   timestamp))
                         ,@config-frame-payload))
         (padded (let* ((n ::int (length payload))
                        (unpadded ::int (modulo n 16)))
                   (if (= unpadded 0)
                       payload
                       (append! payload (make-list (- 16 unpadded) #x2f)))))
         (aes-initial-vector `(,@man ,@id ,ver ,(MBUS-DEVICE-TYPE:00-OTHER:ordinal)
                                     ,@(make-list 8 access-number)))
         (encrypted-payload (aes-cbc-encrypt padded key: aes-key
                                             iv: aes-initial-vector))
         (content `(,@dll ,@stl ,@encrypted-payload)))
    (map (lambda (x) (as int x))
         `(,(length content) ,@content))))

(e.g.
 (wmbus-flowis-config-frame
  dongle-manufacturer: "FLM"
  dongle-serial-number: 10000200
  dongle-version: 1
  timestamp: 0
  access-number: #xC5
  target-serial-number: 41146360
  config-frame-payload:
  `(,(DONGLE-COMMAND:1C-ENTER-CONFIGURATION:ordinal)))
 ===> (#x1e ; length
       #x44 ; control word
       #x8d #x19 ; manufacturer
       #x00 #x02 #x00 #x10 ; serial number
       #x01 ; version
       #x00 ; type
       #x7a ; ?
       #xc5 ; access number
       #x00 ; status
       #x10 #xa5 ; configuration word
       #xf1 #x32 #x44 #xd8 ; encrypted payload
       #x98 #x65 #x93 #xc3 ; (with padding)
       #x84 #xb5 #x51 #xce
       #x80 #x6a #xbf #x96))

(define-type (MBusBlock value: (either number string)
                        unit: MBUS-UNIT
                        scale: (either real MBUS-TIME-UNITS)
                        storage: int
                        tariff: int
                        subunit: int
                        DIB-size: int
                        VIB-size: int
                        data-size: int
                        total-size: int
                        source: (list-of ubyte)))

(define-type (WMBusFrame manufacturer: string
                         serial-number: int
                         type: MBUS-DEVICE-TYPE
                         version: int
                         length: int
                         access-number: ubyte
                         blocks: (list-of (either MBusBlock ubyte))
                         source: (list-of ubyte)))

(define (mbus-value bytes::(list-of ubyte)
                    type::MBUS-DATA-TYPE
                    unit::MBUS-UNIT storage::int)
  bytes)

(define (mbus-parse-data-block input::(list-of ubyte))::MBusBlock
  (otherwise
   #!null
   (and-let* ((DIB- (take-while (isnt (bitwise-and _ #b10000000) zero?) input))
              (`(,last-DIFE . ,VIB+) (drop (length DIB-) input))
              (DIB `(,@DIB- ,last-DIFE))
              (VIB- (take-while (isnt (bitwise-and _ #b10000000) zero?) VIB+))
              (`(,last-VIFE . ,data+) (drop (length VIB-) VIB+))
              (VIB `(,@VIB- ,last-VIFE))
              (`(,DIF . ,DIFE*) DIB)
              (`(,subunit ,tariff ,storage ,function ,type)
               (fold-left (lambda (DIB DIFE)
                            (and-let* ((`(,subunit ,tariff ,storage
                                                   . ,function+type) DIB)
                                       (`(,_ ,subunit0 ,tariff1 ,tariff0
                                             . ,storage3-0) (8-bits (as int DIFE))))
                              `((,subunit0 . ,subunit)
                                (,tariff1 ,tariff0 . ,tariff)
                                (,@storage3-0 . ,storage)
                                . ,function+type)))
                          (and-let* ((`(,_ ,storage ,fun1 ,fun0
                                           . ,type3-0) (8-bits (as int DIF))))
                            `(() () (,storage)
                              ,((MBUS-DATA-FUNCTION:values) (number/base*
                                                             2 fun1 fun0))
                              ,((MBUS-DATA-TYPE:values) (number/base
                                                         2 type3-0))))DIFE*))
              (subunit (number/base 2 subunit))
              (tariff (number/base 2 tariff))
              (storage (number/base 2 storage))
              (`(,VIF . ,VIFE*) VIB)
              (unit scale (mbus-VIF-unit+scale VIF))
              ;; TODO jeszcze powinnismy wyciagnac modyfikatory
              (unit scale (match unit
                            (,MBUS-UNIT:VIF-EXTENSION-B
                             (match VIFE*
                               (`(,VIFE0 . ,VIFE**)
                                (mbus-VIFEB-unit+scale VIFE0))))
                            (,MBUS-UNIT:VIF-EXTENSION-D
                             (match VIFE*
                               (`(,VIFE0 . ,VIFE**)
                                (mbus-VIFED-unit+scale VIFE0))))
                            (_
                             (values unit scale)))))
     (match type
       (,MBUS-DATA-TYPE:D-LVAR
        (match data+
          (`(,lvar . ,data+)
           (let* ((size ::int (mbus-LVAR-size lvar))
                  (data (take size data+)))
             (MBusBlock value: (mbus-value data type unit storage)
                        unit: unit
                        scale: scale
                        storage: storage
                        tariff: tariff
                        subunit: subunit
                        DIB-size: (length DIB)
                        VIB-size: (length VIB)
                        data-size: (length data)
                        total-size: (+ (length DIB)
                                       (length VIB)
                                       1 ;; lvar
                                       (length data))
                        source: `(,@DIB ,@VIB ,lvar ,@data))))))

       (,MBUS-DATA-TYPE:8-READOUT
        (error "Unsupported MBus data type: "type))

       (,MBUS-DATA-TYPE:F-SPECIAL
        (error "Unsupported MBus data type: "type))

       (_
        (let* ((size ::int (mbus-data-size type))
               (data (take size data+)))
          (MBusBlock value: (mbus-value data type unit storage)
                     unit: unit
                     scale: scale
                     storage: storage
                     tariff: tariff
                     subunit: subunit
                     DIB-size: (length DIB)
                     VIB-size: (length VIB)
                     data-size: (length data)
                     total-size: (+ (length DIB)
                                    (length VIB)
                                    (length data))
                     source: `(,@DIB ,@VIB ,@data))))))))

(define (mbus-parse-data-blocks input::(list-of ubyte))::(list-of (either MBusBlock
                                                                          ubyte))
  (match input
    (`(#2f . ,_) input)
    ('() input)
    (else
     (let ((block (mbus-parse-data-block input)))
       (if block
           `(,block . ,(mbus-parse-data-blocks (drop block:total-size input)))
           input)))))

(define (wmbus-parse-frame input::(list-of ubyte)
                           #!key (aes-key (make-list 16 0)))
  ::WMBusFrame
  (match input
    (`(,frame-length
       ,control-field
       ,manufacturer-1 ,manufacturer-2
       ,id1 ,id2 ,id3 ,id4
       ,version
       ,type
       ,control-information
       ,access-number
       ,status
       ,config1 ,config2 . ,encrypted-payload)
     (let* ((aes-initial-vector `(,manufacturer-1
                                  ,manufacturer-2
                                  ,id1 ,id2 ,id3 ,id4
                                  ,version
                                  ,type
                                  ,access-number
                                  ,access-number
                                  ,access-number
                                  ,access-number
                                  ,access-number
                                  ,access-number
                                  ,access-number
                                  ,access-number))
            (decrypted-payload (aes-cbc-decrypt encrypted-payload
                                                key: aes-key
                                                iv: aes-initial-vector)))
       (match decrypted-payload
         (`(#x2f #x2f . ,data-blocks)
          (let ((blocks (mbus-parse-data-blocks data-blocks)))
            (WMBusFrame manufacturer: (wmbus-manufacturer-string manufacturer-1
                                                                 manufacturer-2)
                        serial-number: (number/base
                                        10 (digits/base
                                            16 (number/base* 256
                                                             id4 id3 id2 id1)))
                        type: ((MBUS-DEVICE-TYPE:values) type)
                        version: version
                        length: frame-length
                        access-number: access-number
                        blocks: blocks
                        source: input))))))))


#;(e.g.
 (wmbus-parse-frame
  (hex->list "7e 44 8f 41 60 63 14 41 02 04 7a 0f 00 70 a5 64 1c f6 35 2a 0b 79 5a 64 f3 09 31 92 41 32 6a 11 b1 a1 8d 50 38 b7 17 74 65 b8 24 01 77 3d 42 11 c9 a5 db 9d 4a 32 8b 59 50 2d 5b aa 67 34 16 de 7a 4d f3 ee 61 ed 86 65 3d c7 28 d6 c2 d1 a0 a2 8c 26 90 05 e5 44 fe c6 fe dd 9f ba 54 d4 cf aa a1 33 9d ca f4 a9 2a dc c3 48 a3 2e 64 fa 4f b8 00 9c 43 1a b2 24 eb 35 8b b6 91 22 0d b8 f3") ===>
  [WMBusFrame manufacturer: "PLO"
              serial-number: 41146360
              type: 04-HEAT-METER
              version: 2
              length: 126
              access-number: 15
              blocks: ([MBusBlock value: (3 0 0 0)
                                  unit: ENERGY-J
                                  scale: 10000000
                                  storage: 0 tariff: 0 subunit: 0
                                  DIB-size: 1 VIB-size: 1 data-size: 4 total-size: 6
                                  source: (12 15 3 0 0 0)]
                       [MBusBlock value: (184 48 22 49)
                                  unit: DATETIME-F
                                  scale: 1
                                  storage: 0 tariff: 0 subunit: 0
                                  DIB-size: 1 VIB-size: 1 data-size: 4 total-size: 6
                                  source: (4 109 184 48 22 49)]
                       [MBusBlock value: (16)
                                  unit: ERROR-FLAGS
                                  scale: 1
                                  storage: 0 tariff: 0 subunit: 0
                                  DIB-size: 1 VIB-size: 2 data-size: 1 total-size: 4
                                  source: (1 253 23 16)]
                       [MBusBlock value: (0)
                                  unit: ERROR-FLAGS
                                  scale: 1
                                  storage: 1 tariff: 0 subunit: 0
                                  DIB-size: 1 VIB-size: 2 data-size: 1 total-size: 4
                                  source: (65 253 23 0)]
                       [MBusBlock value: (0 0 0 0 0 0 0 0 0 0 ...)
                                  unit: ERROR-FLAGS
                                  scale: 1
                                  storage: 14 tariff: 0 subunit: 0
                                  DIB-size: 2 VIB-size: 2 data-size: 15 total-size: 19
                                  source: (141 7 253 23 14 0 0 0 0 0 ...)]
                       [MBusBlock value: (0 0 0 0 0 0 0 0 0 0 ...)
                                  unit: ENERGY-J
                                  scale: 1
                                  storage: 14 tariff: 0 subunit: 0
                                  DIB-size: 2 VIB-size: 1 data-size: 68 total-size: 71
                                  source: (141 7 8 0 0 0 0 0 0 0 ...)])
              source: (126 68 143 65 96 99 20 65 2 4 ...)]))
